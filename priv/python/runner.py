#!/usr/bin/env python3
"""Erlang port process that executes sandboxed Python code.

Communicates via stdin/stdout using 4-byte length-prefixed ETF messages.
Receives an ``init`` handshake with an allowed-modules whitelist, then
enters a loop: reads code-execution requests, runs them via ``exec()``,
and returns results.
"""

# +--------------------------------------------------------------+
# | Copyright (c) 2026. All Rights Reserved.                     |
# | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
# +--------------------------------------------------------------+

from __future__ import annotations

import builtins
import itertools
import struct
import sys
import types

from collections.abc import Callable
from typing import Any, NoReturn

# REQUIRED IMPORTS FOR ERLANG TERM CONVERSION!
from term import codec
from term.atom import Atom

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

_DENIED_BUILTINS = frozenset({
    "__import__",
    "breakpoint",
    "compile",
    "eval",
    "exec",
    "exit",
    "input",
    "open",
    "quit",
})

_MAX_MESSAGE_SIZE = 10 * 1024 * 1024
_HEADER_SIZE = 4
_HEADER_FORMAT = "!I"
_DRAIN_CHUNK_SIZE = 65_536

_ATOM_STATUS = Atom("status")
_ATOM_OK = Atom("ok")
_ATOM_ERROR = Atom("error")
_ATOM_TYPE = Atom("type")
_ATOM_CALL_REQUEST = Atom("call_request")
_ATOM_REQUEST_ID = Atom("request_id")
_ATOM_MODULE = Atom("module")
_ATOM_FUNCTION = Atom("function")
_ATOM_ARGS = Atom("args")

# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------


class MessageEOF(Exception):
    """Raised when stdin is closed, clean EOF."""


class MessageOversized(Exception):
    """Raised when message exceeds _MAX_MESSAGE_SIZE."""


class MessageTruncated(Exception):
    """Raised when payload is shorter than declared length."""


class ErlangError(Exception):
    """Raised when an erlang function call returns an error response."""
    def __init__(self, message: str, error_type: str = "exception") -> None:
        super().__init__(message)
        self.error_type = error_type


class ErlangTimeoutError(ErlangError):
    """Raised when an erlang function call request times out."""


class ErlangPortError(ErlangError):
    """Raised when the erlang port connection is lost or interrupted."""


# ---------------------------------------------------------------------------
# Sandbox helpers
# ---------------------------------------------------------------------------


def _make_restricted_import(
    allowed_modules: list[str],
) -> Callable[..., types.ModuleType]:
    """Return an `__import__` replacement that only allows `allowed_modules`."""
    allowed = frozenset(allowed_modules)
    real_import = builtins.__import__

    def restricted_import(
        name: str,
        globals: dict[str, Any] | None = None,
        locals: dict[str, Any] | None = None,
        fromlist: tuple[str, ...] = (),
        level: int = 0,
    ) -> types.ModuleType:
        top_level = name.split(".")[0]
        if top_level not in allowed:
            raise ImportError(f"import of '{name}' is not allowed")
        return real_import(name, globals, locals, fromlist, level)

    return restricted_import


def _build_safe_builtins(allowed_modules: list[str]) -> dict[str, Any]:
    """Build a builtins dict with dangerous names removed."""
    safe = {
        name: getattr(builtins, name)
        for name in dir(builtins)
        if name not in _DENIED_BUILTINS
    }

    if allowed_modules:
        safe["__import__"] = _make_restricted_import(allowed_modules)

    return safe


def _to_str(value: Any) -> str:
    """Decode bytes to UTF-8 or convert to str."""
    if isinstance(value, bytes):
        return value.decode("utf-8")
    return str(value)


# ---------------------------------------------------------------------------
# Response helpers
# ---------------------------------------------------------------------------


def _ok_response(**fields: Any) -> dict[Atom, Any]:
    """Build a successful erlang response map."""
    resp: dict[Atom, Any] = {_ATOM_STATUS: _ATOM_OK}

    for k, v in fields.items():
        resp[Atom(k)] = v

    return resp


def _error_response(error: str) -> dict[Atom, Any]:
    """Build an error erlang response map."""
    return {_ATOM_STATUS: _ATOM_ERROR, _ATOM_ERROR: error}


# ---------------------------------------------------------------------------
# IO / message protocol
# ---------------------------------------------------------------------------


def read_message() -> Any:
    """Read a single length-prefixed ETF message from stdin."""
    header = sys.stdin.buffer.read(_HEADER_SIZE)

    if len(header) == 0:
        raise MessageEOF("stdin closed")

    if len(header) < _HEADER_SIZE:
        raise MessageTruncated(
            f"expected {_HEADER_SIZE}-byte header, got {len(header)} bytes"
        )

    (length,) = struct.unpack(_HEADER_FORMAT, header)
    if length > _MAX_MESSAGE_SIZE:
        remaining = length
        while remaining > 0:
            chunk = sys.stdin.buffer.read(min(remaining, _DRAIN_CHUNK_SIZE))
            if not chunk:
                break
            remaining -= len(chunk)
        raise MessageOversized(
            f"message size {length} exceeds limit {_MAX_MESSAGE_SIZE}"
        )

    payload = sys.stdin.buffer.read(length)
    if len(payload) < length:
        raise MessageTruncated(
            f"expected {length} payload bytes, got {len(payload)}"
        )

    result, _remaining = codec.binary_to_term(payload)
    return result


def write_message(data: Any) -> None:
    """Write a single length-prefixed ETF message to stdout."""
    payload = codec.term_to_binary(data)
    header = struct.pack(_HEADER_FORMAT, len(payload))
    sys.stdout.buffer.write(header + payload)
    sys.stdout.buffer.flush()

# ---------------------------------------------------------------------------
# Erlang caller
# ---------------------------------------------------------------------------


class ErlangCaller:
    """Manages bidirectional call requests from python back to erlang worker process.
    Each call writes a `call_request` message to stdout and blocks reading the response
    from stdin, using a monotonic integer request ID.
    """

    def __init__(self) -> None:
        self._ids = itertools.count(1)

    def call(
        self,
        module: str,
        function: str,
        args: list[Any] | None = None,
    ) -> Any:
        """Call an Erlang function and return its result.
        May raise ErlangError, ErlangTimeoutError, ErlangPortError on failures.
        """

        if args is None:
            args = []

        request_id = str(next(self._ids))

        write_message({
            _ATOM_TYPE: _ATOM_CALL_REQUEST,
            _ATOM_REQUEST_ID: request_id,
            _ATOM_MODULE: Atom(module),
            _ATOM_FUNCTION: Atom(function),
            _ATOM_ARGS: args,
        })

        try:
            response = read_message()
        except (MessageEOF, MessageOversized, MessageTruncated) as e:
            raise ErlangPortError(
                f"lost connection to erlang port: {e}",
                "port_error"
            ) from e

        resp_id = _to_str(response.get("request_id"))
        if resp_id != request_id:
            raise ErlangPortError(
                f"request id mismatch: expected {request_id}, but got {resp_id}",
                "port_error",
            )

        if response.get("status") == "ok":
            return response.get("result")

        error_type = _to_str(response.get("error_type", "exception"))
        error_msg = _to_str(response.get("error", "unknown error"))

        if error_type == "timeout":
            raise ErlangTimeoutError(error_msg, error_type)

        raise ErlangError(error_msg, error_type)


# ---------------------------------------------------------------------------
# Request handling
# ---------------------------------------------------------------------------


def _handle_request(
    message: dict[str, Any],
    safe_builtins: dict[str, Any],
    caller: ErlangCaller,
) -> dict[Atom, Any]:
    """Execute a single code-execution request and return an Erlang response map."""
    code = message.get("code", b"")

    if isinstance(code, bytes):
        code = code.decode("utf-8")

    arguments = message.get("arguments", {})
    local_vars: dict[str, Any] = {"arguments": arguments, "result": None, "erlang": caller}

    try:
        exec(code, {"__builtins__": safe_builtins}, local_vars)
        return _ok_response(result=local_vars.get("result"))
    except Exception as e:
        return _error_response(f"{type(e).__name__}: {e}")


# ---------------------------------------------------------------------------
# Lifecycle
# ---------------------------------------------------------------------------

def _perform_handshake() -> dict[str, Any]:
    """Perform the initialization with the erlang worker process.
    Reads the `init` message, builds the safe-builtins dict, and sends an `ok` acknowledgement.
    Exits the process on any failure.
    """
    def _fail(error: str) -> NoReturn:
        write_message(_error_response(error))
        sys.exit(1)

    try:
        message = read_message()
    except MessageEOF:
        _fail("stdin closed before initialization")
    except Exception as e:
        _fail("malformed initialization message")

    if message is None or message.get("type") != "init":
        _fail("expected initialization message")

    allowed_modules = [_to_str(m) for m in message.get("allowed_modules", [])]
    safe_builtins = _build_safe_builtins(allowed_modules)
    write_message(_ok_response())

    return safe_builtins


def main() -> None:
    """Entry point for the active OS python process."""
    safe_builtins = _perform_handshake()
    caller = ErlangCaller()
    while True:
        try:
            message = read_message()
        except MessageEOF:
            break
        except (MessageOversized, MessageTruncated) as e:
            write_message(_error_response(str(e)))
            continue
        except Exception as e:
            write_message(_error_response("malformed message"))
            continue
        response = _handle_request(message, safe_builtins, caller)
        write_message(response)


if __name__ == "__main__":
    main()
