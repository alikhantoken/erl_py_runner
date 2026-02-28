#!/usr/bin/env python3
"""Erlang port process that executes sandboxed Python code.

Communicates via stdin/stdout using 4-byte length-prefixed ETF messages.
Receives an ``init`` handshake with an allowed-modules whitelist, then
enters a loop: reads code-execution requests, runs them via ``exec()``,
and returns results.

Exit codes:
    0 = normal end of file (EOF)
    1 = handshake failure
    2 = communication failure
"""

# +--------------------------------------------------------------+
# | Copyright (c) 2026. All Rights Reserved.                     |
# | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
# +--------------------------------------------------------------+

from __future__ import annotations

import builtins
import itertools
import logging
import struct
import sys
import traceback
import types
from collections.abc import Callable
from typing import IO, Any, Final, NoReturn

# REQUIRED IMPORTS FOR ERLANG TERM CONVERSION!
from term import codec
from term.atom import Atom

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

_DENIED_BUILTINS: Final[frozenset[str]] = frozenset({
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

_MAX_MESSAGE_SIZE: Final[int] = 10 * 1024 * 1024
_DRAIN_CHUNK_SIZE: Final[int] = 65_536
_HEADER_STRUCT: Final[struct.Struct] = struct.Struct("!I")

_ATOM_OK: Final[Atom] = Atom("ok")
_ATOM_ERROR: Final[Atom] = Atom("error")
_ATOM_INIT: Final[Atom] = Atom("init")
_ATOM_EXEC: Final[Atom] = Atom("exec")
_ATOM_CALL: Final[Atom] = Atom("call")
_ATOM_CALL_REPLY: Final[Atom] = Atom("call_reply")
_ATOM_LOAD_LIBRARY: Final[Atom] = Atom("load_library")
_ATOM_DELETE_LIBRARY: Final[Atom] = Atom("delete_library")

# These module names are prohibited for user libraries.
_RESERVED_LIBRARY_NAMES: Final[frozenset[str]] = frozenset({
    "sys", "builtins", "term", "codec", "types", "erl_py_runner",
})

# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------


class MessageEOF(Exception):
    """Raised when stdin is closed - EOF."""


class MessageOversized(Exception):
    """Raised when a declared message length exceeds _MAX_MESSAGE_SIZE."""


class MessageTruncated(Exception):
    """Raised when fewer bytes arrive than declared."""


class ErlangError(Exception):
    """Raised when an erlang function call returns an error response."""
    __slots__ = ("error_type",)
    def __init__(self, message: str, error_type: str = "exception") -> None:
        super().__init__(message)
        self.error_type = error_type


class ErlangPortError(ErlangError):
    """Raised when the Erlang port connection is lost or corrupted."""


# ---------------------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------------------


def _to_str(value: Any) -> str:
    return value.decode("utf-8") if isinstance(value, bytes) else str(value)


def _ok_response(result: Any) -> tuple[Atom, Any]:
    return _ATOM_OK, result


def _error_response(error: str) -> tuple[Atom, str]:
    return _ATOM_ERROR, error


# ---------------------------------------------------------------------------
# Sandboxing
# ---------------------------------------------------------------------------


def _make_restricted_import(
    allowed_modules: list[str],
) -> Callable[..., types.ModuleType]:
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
    safe: dict[str, Any] = {
        name: getattr(builtins, name)
        for name in dir(builtins)
        if name not in _DENIED_BUILTINS
    }

    if allowed_modules is None:
        safe["__import__"] = builtins.__import__
    elif allowed_modules:
        safe["__import__"] = _make_restricted_import(allowed_modules)

    return safe


# ---------------------------------------------------------------------------
# Port I/O
# ---------------------------------------------------------------------------


class ErlangPort:
    __slots__ = ("_reader", "_writer")

    def __init__(
        self,
        reader: IO[bytes],
        writer: IO[bytes],
    ) -> None:
        self._reader = reader
        self._writer = writer

    def read(self) -> Any:
        header = self._reader.read(_HEADER_STRUCT.size)

        if len(header) == 0:
            raise MessageEOF("stdin closed")

        if len(header) < _HEADER_STRUCT.size:
            raise MessageTruncated(
                f"Expected {_HEADER_STRUCT.size}-byte header, got {len(header)} bytes"
            )

        (length,) = _HEADER_STRUCT.unpack(header)

        if length > _MAX_MESSAGE_SIZE:
            self._drain(length)
            raise MessageOversized(
                f"Message size {length} exceeds limit {_MAX_MESSAGE_SIZE}"
            )

        payload = self._reader.read(length)
        if len(payload) < length:
            raise MessageTruncated(
                f"Expected {length} bytes, got {len(payload)}"
            )

        term, _ = codec.binary_to_term(payload)

        return term

    def write(self, data: Any) -> None:
        payload = codec.term_to_binary(data)
        self._writer.write(_HEADER_STRUCT.pack(len(payload)) + payload)
        self._writer.flush()

    def _drain(self, length: int) -> None:
        remaining = length
        while remaining > 0:
            chunk = self._reader.read(min(remaining, _DRAIN_CHUNK_SIZE))
            if not chunk:
                break
            remaining -= len(chunk)


# ---------------------------------------------------------------------------
# Erlang callback
# ---------------------------------------------------------------------------


class ErlangCaller:
    __slots__ = ("_port", "_ids")

    def __init__(self, port: ErlangPort) -> None:
        self._port = port
        self._ids = itertools.count(1)

    def call(
        self,
        module: str,
        function: str,
        arguments: list[Any] | None = None,
    ) -> Any:
        """Call `Module:Function(Arguments)` in Erlang Worker Process."""
        request_id = next(self._ids)

        self._port.write((
            _ATOM_CALL,
            request_id,
            Atom(module),
            Atom(function),
            [] if arguments is None else arguments,
        ))

        try:
            response = self._port.read()
        except (MessageEOF, MessageOversized, MessageTruncated) as exception:
            raise ErlangPortError(
                f"Lost connection to Erlang port: {exception}", "port_error"
            ) from exception

        self._validate_call_reply(response, request_id)

        _, _, result = response

        if isinstance(result, tuple) and len(result) == 2:
            tag, value = result
            if tag == _ATOM_OK:
                return value
            if tag == _ATOM_ERROR:
                raise ErlangError(_to_str(value))

        raise ErlangPortError("Malformed Erlang callback result", "port_error")

    @staticmethod
    def _validate_call_reply(response: Any, request_id: int) -> None:
        if (
            not isinstance(response, tuple)
            or len(response) != 3
            or response[0] != _ATOM_CALL_REPLY
        ):
            raise ErlangPortError("Unexpected callback response format", "port_error")

        if response[1] != request_id:
            raise ErlangPortError(
                f"RequestID mismatch, expected {request_id}, got {response[1]}",
                "port_error",
            )


# ---------------------------------------------------------------------------
# Library library_storage
# ---------------------------------------------------------------------------


class LibraryStorage:
    __slots__ = ("_safe_builtins", "_loaded")

    def __init__(self, safe_builtins: dict[str, Any]) -> None:
        self._safe_builtins = safe_builtins
        self._loaded: set[str] = set()

    def load(self, name: str, code: str) -> None:
        top_level = name.split(".")[0]

        if name in _RESERVED_LIBRARY_NAMES or top_level in _RESERVED_LIBRARY_NAMES:
            raise ValueError(f"Reserved library name: {name!r}")

        module = types.ModuleType(name)
        module.__dict__["__builtins__"] = self._safe_builtins

        try:
            exec(code, module.__dict__)
        except SystemExit as exception:
            raise ValueError("SystemExit is not allowed in library code") from exception

        sys.modules[name] = module
        self._loaded.add(name)

    def delete(self, name: str) -> None:
        if name not in self._loaded:
            raise KeyError(name)
        self._loaded.discard(name)
        sys.modules.pop(name, None)


# ---------------------------------------------------------------------------
# Message dispatcher
# ---------------------------------------------------------------------------


class MessageDispatcher:
    __slots__ = ("_safe_builtins", "_library_storage", "_caller", "_table")

    def __init__(
        self,
        safe_builtins: dict[str, Any],
        library_storage: LibraryStorage,
        caller: ErlangCaller,
    ) -> None:
        self._safe_builtins = safe_builtins
        self._library_storage = library_storage
        self._caller = caller
        self._table: dict[Any, Callable[[Any], Any]] = {
            _ATOM_LOAD_LIBRARY: self._handle_load_library,
            _ATOM_DELETE_LIBRARY: self._handle_delete_library,
            _ATOM_EXEC: self._handle_exec,
        }

    def dispatch(self, message: Any) -> Any:
        tag = message[0] if isinstance(message, tuple) and message else None
        handler = self._table.get(tag, self._handle_unknown)
        return handler(message)

    # ------------------------------------------------------------------
    # Handlers
    # ------------------------------------------------------------------

    def _handle_load_library(self, message: Any) -> Any:
        if not isinstance(message, tuple) or len(message) != 3:
            return _error_response("Invalid load library message format")

        _, name, code = message

        try:
            self._library_storage.load(_to_str(name), _to_str(code))
        except ValueError as exception:
            return _error_response(str(exception))
        except Exception as exception:
            return _error_response(
                f"{type(exception).__name__}: {exception}\n{traceback.format_exc()}"
            )

        return _ATOM_OK

    def _handle_delete_library(self, message: Any) -> Any:
        if not isinstance(message, tuple) or len(message) != 2:
            return _error_response("Invalid delete library message format")

        _, name = message

        try:
            self._library_storage.delete(_to_str(name))
        except KeyError:
            return _error_response("Library not loaded")
        return _ATOM_OK

    def _handle_exec(self, message: Any) -> Any:
        if not isinstance(message, tuple) or len(message) != 4:
            return _error_response("Invalid exec message format")

        _, code, arguments, state = message

        local_vars: dict[str, Any] = {
            "arguments": arguments,
            "state": state,
            "result": None,
            "erlang": self._caller,
        }

        try:
            exec(
                _to_str(code),
                {"__builtins__": self._safe_builtins},
                local_vars,
            )
            return _ok_response((local_vars.get("result"), local_vars.get("state")))
        except SystemExit:
            return _error_response("SystemExit is not allowed")
        except Exception as exception:
            return _error_response(
                f"{type(exception).__name__}: {exception}\n{traceback.format_exc()}"
            )

    def _handle_unknown(self, message: Any) -> Any:
        return _error_response(f"Unknown message tag: {message!r}")


# ---------------------------------------------------------------------------
# Worker lifecycle
# ---------------------------------------------------------------------------


class Worker:
    __slots__ = ("_port",)

    def __init__(self, port: ErlangPort) -> None:
        self._port = port

    def run(self) -> None:
        dispatcher = self._handshake()
        self._loop(dispatcher)

    def _handshake(self) -> MessageDispatcher:
        try:
            message = self._port.read()
        except MessageEOF:
            self._fatal("stdin closed before initialization", exit_code=1)
        except (MessageOversized, MessageTruncated) as exception:
            self._fatal(str(exception), exit_code=1)
        except Exception as exception:
            self._fatal(
                f"Malformed initialization message: {type(exception).__name__}: {exception}",
                exit_code=1,
            )

        if (
            not isinstance(message, tuple)
            or len(message) != 2
            or message[0] != _ATOM_INIT
        ):
            self._fatal("Expected (init, Modules) tuple", exit_code=1)

        raw_modules = message[1]
        if raw_modules == Atom("all"):
            allowed_modules: list[str] | None = None
        else:
            try:
                allowed_modules = [_to_str(m) for m in raw_modules]
            except (TypeError, ValueError, AttributeError) as exception:
                self._fatal(f"Invalid allowed modules: {exception}", exit_code=1)

        safe_builtins = _build_safe_builtins(allowed_modules)
        caller = ErlangCaller(self._port)
        library_storage = LibraryStorage(safe_builtins)
        dispatcher = MessageDispatcher(safe_builtins, library_storage, caller)

        self._port.write(_ATOM_OK)

        return dispatcher

    def _loop(self, dispatcher: MessageDispatcher) -> None:
        while True:
            message = self._read_next()
            if message is None:
                break
            try:
                response = dispatcher.dispatch(message)
                self._port.write(response)
            except (BrokenPipeError, OSError):
                break
            except Exception as exception:
                self._try_write(
                    _error_response(f"Handler error: {type(exception).__name__}: {exception}")
                )
                sys.exit(2)

    def _read_next(self) -> Any | None:
        try:
            return self._port.read()
        except MessageEOF:
            return None
        except Exception as exception:
            self._try_write(
                _error_response(f"Message error: {type(exception).__name__}: {exception}")
            )
            sys.exit(2)

    def _fatal(self, reason: str, *, exit_code: int) -> NoReturn:
        self._try_write(_error_response(reason))
        sys.exit(exit_code)

    def _try_write(self, data: Any) -> None:
        try:
            self._port.write(data)
        except (BrokenPipeError, OSError):
            pass


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def main() -> None:
    Worker(ErlangPort(sys.stdin.buffer, sys.stdout.buffer)).run()


if __name__ == "__main__":
    main()