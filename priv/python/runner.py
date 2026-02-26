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

_ATOM_OK = Atom("ok")
_ATOM_ERROR = Atom("error")
_ATOM_INIT = Atom("init")
_ATOM_EXEC = Atom("exec")
_ATOM_CALL = Atom("call")
_ATOM_CALL_REPLY = Atom("call_reply")

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


class ErlangPortError(ErlangError):
    """Raised when the erlang port connection is lost or interrupted."""


# ---------------------------------------------------------------------------
# Script helpers
# ---------------------------------------------------------------------------


def _make_restricted_import(
    allowed_modules: list[str],
) -> Callable[..., types.ModuleType]:
    """Return an `__import__` replacement that only allows `allowed_modules`."""
    allowed = frozenset(allowed_modules)
    real_import = builtins.__import__

    def restricted_import(
        name: str,
        _globals: dict[str, Any] | None = None,
        _locals: dict[str, Any] | None = None,
        fromlist: tuple[str, ...] = (),
        level: int = 0,
    ) -> types.ModuleType:
        top_level = name.split(".")[0]
        if top_level not in allowed:
            raise ImportError(f"Import of '{name}' is not allowed")
        return real_import(name, _globals, _locals, fromlist, level)

    return restricted_import


def _build_safe_builtins(allowed_modules: list[str] | None) -> dict[str, Any]:
    safe = {
        name: getattr(builtins, name)
        for name in dir(builtins)
        if name not in _DENIED_BUILTINS
    }

    if allowed_modules is None:
        safe["__import__"] = builtins.__import__
    elif allowed_modules:
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


def _ok_response(result: Any) -> tuple:
    return _ATOM_OK, result


def _error_response(error: str) -> tuple:
    return _ATOM_ERROR, error


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
            f"Expected {_HEADER_SIZE}-byte header, got {len(header)} bytes"
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
            f"Message size {length} exceeds limit {_MAX_MESSAGE_SIZE}"
        )

    payload = sys.stdin.buffer.read(length)
    if len(payload) < length:
        raise MessageTruncated(
            f"Expected {length} payload bytes, got {len(payload)}"
        )

    result, _remaining = codec.binary_to_term(payload)
    return result


def write_message(data: Any) -> None:
    """Write a single length-prefixed ETF message to stdout."""
    payload = codec.term_to_binary(data)
    header = struct.pack(_HEADER_FORMAT, len(payload))
    sys.stdout.buffer.writelines([header, payload])
    sys.stdout.buffer.flush()

# ---------------------------------------------------------------------------
# Erlang caller
# ---------------------------------------------------------------------------


class ErlangCaller:
    """Manages bidirectional call requests from python back to erlang worker process.
    Each call writes a ``call`` tuple to stdout and blocks reading the ``call_reply``
    from stdin, using a monotonic integer request ID.
    """

    def __init__(self) -> None:
        self._ids = itertools.count(1)

    def call(
        self,
        module: str,
        function: str,
        arguments: list[Any] | None = None,
    ) -> Any:
        """Call an Erlang function and return its result.
        May raise ErlangError, ErlangPortError on failures.
        """
        if arguments is None:
            arguments = []

        request_id = next(self._ids)

        write_message((
            _ATOM_CALL,
            request_id,
            Atom(module),
            Atom(function),
            arguments,
        ))

        try:
            response = read_message()
        except (MessageEOF, MessageOversized, MessageTruncated) as e:
            raise ErlangPortError(
                f"Lost connection to erlang port: {e}",
                "port_error",
            ) from e

        if (
            not isinstance(response, tuple)
            or len(response) != 3
            or response[0] != _ATOM_CALL_REPLY
        ):
            raise ErlangPortError("Unexpected response format", "port_error")

        _, response_id, result = response

        if response_id != request_id:
            raise ErlangPortError(
                f"RequestID mismatch: expected {request_id}, but got {response_id}",
                "port_error",
            )

        if isinstance(result, tuple) and len(result) == 2 and result[0] == _ATOM_OK:
            return result[1]

        if isinstance(result, tuple) and len(result) == 2 and result[0] == _ATOM_ERROR:
            raise ErlangError(_to_str(result[1]))

        raise ErlangPortError("Malformed callback result", "port_error")


# ---------------------------------------------------------------------------
# Request handling
# ---------------------------------------------------------------------------


def _handle_request(
    message: Any,
    safe_builtins: dict[str, Any],
    caller: ErlangCaller,
) -> tuple:
    if not isinstance(message, tuple) or len(message) != 4 or message[0] != _ATOM_EXEC:
        return _error_response("Invalid message format")

    _, code, arguments, state = message

    if isinstance(code, bytes):
        code = code.decode("utf-8")

    local_vars: dict[str, Any] = {
        "arguments": arguments,
        "state": state,
        "result": None,
        "erlang": caller,
    }

    try:
        exec(code, {"__builtins__": safe_builtins}, local_vars)
        return _ok_response((local_vars.get("result"), local_vars.get("state")))
    except SystemExit:
        return _error_response("SystemExit is not allowed")
    except Exception as e:
        return _error_response(f"{type(e).__name__}: {e}")


# ---------------------------------------------------------------------------
# Lifecycle
# ---------------------------------------------------------------------------

def _perform_handshake() -> dict[str, Any]:
    """Perform the initialization with the erlang worker process.
    Reads the ``(init, AllowedModules)`` tuple, builds the safe-builtins dict,
    and sends an ``ok`` acknowledgement.
    Exits the process on any failure.
    """
    def _fail(error: str) -> NoReturn:
        write_message(_error_response(error))
        sys.exit(1)

    try:
        message = read_message()
    except MessageEOF:
        _fail("stdin closed before initialization")
    except Exception:
        _fail("malformed initialization message")

    if not isinstance(message, tuple) or len(message) != 2 or message[0] != _ATOM_INIT:
        _fail("expected initialization message")

    raw_modules = message[1]
    if raw_modules == Atom("all"):
        allowed_modules = None
    else:
        allowed_modules = [_to_str(m) for m in raw_modules]
    safe_builtins = _build_safe_builtins(allowed_modules)

    write_message(_ATOM_OK)

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
            write_message(_error_response("Malformed message"))
            continue

        response = _handle_request(message, safe_builtins, caller)

        try:
            write_message(response)
        except (BrokenPipeError, OSError):
            break

if __name__ == "__main__":
    main()
