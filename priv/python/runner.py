#!/usr/bin/env python3
"""Erlang port process that executes sandboxed Python code.

Communicates via stdin/stdout using 4-byte length-prefixed ETF messages.
Receives an ``init`` handshake with an options map, then enters a loop:
reads code-execution requests, runs them via ``exec()``, and returns results.

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
import functools
import gc
import struct
import sys
import traceback
import types

from collections.abc import Callable
from dataclasses import dataclass
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

_ATOM_OK:             Final[Atom] = Atom("ok")
_ATOM_ERROR:          Final[Atom] = Atom("error")
_ATOM_INIT:           Final[Atom] = Atom("init")
_ATOM_EXEC:           Final[Atom] = Atom("exec")
_ATOM_CALL:           Final[Atom] = Atom("call")
_ATOM_CALL_REPLY:     Final[Atom] = Atom("call_reply")
_ATOM_LOG:            Final[Atom] = Atom("log")
_ATOM_LOAD_LIBRARY:   Final[Atom] = Atom("load_library")
_ATOM_DELETE_LIBRARY: Final[Atom] = Atom("delete_library")

_ATOM_ALL:         Final[Atom] = Atom("all")
_ATOM_MODULES:     Final[Atom] = Atom("modules")
_ATOM_CACHE_SIZE:  Final[Atom] = Atom("cache_size")
_ATOM_GC_INTERVAL: Final[Atom] = Atom("gc_interval")

# These module names are prohibited for user libraries.
_RESERVED_LIBRARY_NAMES: Final[frozenset[str]] = frozenset({
    "sys", "builtins", "term", "codec", "types", "erl_py_runner",
})

# Value range for Erlang ETF INTEGER_EXT
# https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#integer_ext
_MAX_REQUEST_ID: Final[int] = (1 << 31) - 1

# Maximum entry count for LRU CACHE
# https://docs.python.org/3/library/functools.html
_DEFAULT_CACHE_SIZE:  Final[int] = 512

# Default execution count for GC SWEEP
_DEFAULT_GC_INTERVAL: Final[int] = 128

# ---------------------------------------------------------------------------
# Exceptions
# ---------------------------------------------------------------------------


class ProtocolError(Exception):
    """Base for all erlang-python communication protocol errors."""


class MessageEOF(ProtocolError):
    """Raised when stdin is closed - EOF."""


class MessageOversized(ProtocolError):
    """Raised when a declared message length exceeds _MAX_MESSAGE_SIZE."""


class MessageTruncated(ProtocolError):
    """Raised when fewer bytes arrive than declared."""


class ErlangError(Exception):
    """Raised when an erlang function call returns an error response."""
    __slots__ = ("error_type",)
    def __init__(self, message: str, error_type: str = "exception") -> None:
        super().__init__(message)
        self.error_type = error_type


class ErlangPortError(ErlangError):
    """Raised when the Erlang port connection is lost or corrupted."""


class LibraryVersionError(ValueError):
    """Raised when stored library version does not match the expected version."""


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
        header = self._read_exact(_HEADER_STRUCT.size)
        (length,) = _HEADER_STRUCT.unpack(header)

        if length > _MAX_MESSAGE_SIZE:
            self._drain(length)
            raise MessageOversized(
                f"Message size {length} exceeds limit {_MAX_MESSAGE_SIZE}"
            )

        payload = self._read_exact(length)
        term, _ = codec.binary_to_term(payload)

        return term

    def write(self, data: Any) -> None:
        payload = codec.term_to_binary(data)
        self._writer.write(_HEADER_STRUCT.pack(len(payload)))
        self._writer.write(payload)
        self._writer.flush()

    def _read_exact(self, n: int) -> bytes:
        data = self._reader.read(n)
        if not data:
            raise MessageEOF("stdin closed")

        while len(data) < n:
            chunk = self._reader.read(n - len(data))
            if not chunk:
                raise MessageTruncated(
                    f"Expected {n} bytes, got {len(data)}"
                )
            data += chunk

        return data

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
    __slots__ = ("_port", "_next_id")

    def __init__(self, port: ErlangPort) -> None:
        self._port = port
        self._next_id: int = 1

    def call(
        self,
        module: str,
        function: str,
        arguments: list[Any] | None = None,
    ) -> Any:
        """Call `Module:Function(Arguments)` in Erlang Worker Process."""
        request_id = self._next_id
        self._next_id = self._next_id % _MAX_REQUEST_ID + 1

        self._port.write((
            _ATOM_CALL,
            request_id,
            Atom(module),
            Atom(function),
            [] if arguments is None else arguments,
        ))

        try:
            response = self._port.read()
        except ProtocolError as exception:
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


class ErlangLogger:
    _LEVELS: Final[dict[str, Atom]] = {
        "debug":   Atom("debug"),
        "info":    Atom("info"),
        "notice":  Atom("notice"),
        "warning": Atom("warning"),
        "warn":    Atom("warning"),
        "error":   Atom("error"),
    }

    __slots__ = ("_port",)

    def __init__(self, port: ErlangPort) -> None:
        self._port = port

    def log(
        self,
        level: str,
        message: Any
    ) -> None:
        level_atom = self._LEVELS.get(_to_str(level).lower(), Atom("info"))
        payload = (_ATOM_LOG, level_atom, message)

        try:
            self._port.write(payload)
        except Exception:
            return


# ---------------------------------------------------------------------------
# Library Storage
# ---------------------------------------------------------------------------


@dataclass(frozen=True, slots=True)
class _LibraryEntity:
    hash: bytes
    version: int
    loaded: bool


class LibraryStorage:
    __slots__ = ("_safe_builtins", "_entries")

    def __init__(self, safe_builtins: dict[str, Any]) -> None:
        self._safe_builtins = safe_builtins
        self._entries: dict[str, _LibraryEntity] = {}

    def load(self, name: str, code: str, target_entry: _LibraryEntity, expected_version: int) -> None:
        self._check_name(name)
        current_entry = self._entries.get(name)
        if current_entry == target_entry:
            return

        self._check_version(name, current_entry, expected_version)

        module = types.ModuleType(name)
        module.__dict__["__builtins__"] = self._safe_builtins

        try:
            exec(code, module.__dict__)
        except SystemExit as exception:
            raise ValueError("SystemExit is not allowed in library code") from exception

        sys.modules[name] = module
        self._entries[name] = target_entry

    def delete(self, name: str, target_entry: _LibraryEntity, expected_version: int) -> None:
        current_entry = self._entries.get(name)
        if current_entry == target_entry:
            return

        self._check_version(name, current_entry, expected_version)

        if current_entry is None or not current_entry.loaded:
            raise KeyError(name)

        sys.modules.pop(name, None)
        self._entries[name] = target_entry

    @staticmethod
    def _check_name(name: str) -> None:
        top_level = name.split(".")[0]
        if name in _RESERVED_LIBRARY_NAMES or top_level in _RESERVED_LIBRARY_NAMES:
            raise ValueError(f"Reserved library name: {name!r}")

    @staticmethod
    def _check_version(name: str, entry: _LibraryEntity | None, expected_version: int) -> None:
        current_version = 0 if entry is None else entry.version
        if current_version != expected_version:
            raise LibraryVersionError(
                f"Library version mismatch for {name!r}: "
                f"expected {expected_version}, current {current_version}"
            )

    def load_direct(self, name: str, code: str, hash_bytes: bytes, version: int) -> None:
        self._check_name(name)

        entry = _LibraryEntity(
            hash=hash_bytes,
            version=version,
            loaded=True
        )

        if self._entries.get(name) == entry:
            return

        module = types.ModuleType(name)
        module.__dict__["__builtins__"] = self._safe_builtins

        try:
            exec(code, module.__dict__)
        except SystemExit as exception:
            raise ValueError("SystemExit is not allowed in library code") from exception

        sys.modules[name] = module
        self._entries[name] = entry

    def delete_direct(self, name: str) -> None:
        sys.modules.pop(name, None)
        self._entries.pop(name, None)

    # ------------------------------------------------------------------
    # Library parsers
    # ------------------------------------------------------------------

    @staticmethod
    def parse_load_message(message: tuple) -> tuple[str, str, _LibraryEntity, int]:
        _, name, code, library_hash, expected_version, version = message
        return (
            _to_str(name),
            _to_str(code),
            _LibraryEntity(hash=bytes(library_hash), version=version, loaded=True),
            expected_version,
        )

    @staticmethod
    def parse_delete_message(message: tuple) -> tuple[str, _LibraryEntity, int]:
        _, name, library_hash, expected_version, version = message
        return (
            _to_str(name),
            _LibraryEntity(hash=bytes(library_hash), version=version, loaded=False),
            expected_version,
        )


# ---------------------------------------------------------------------------
# Message dispatcher
# ---------------------------------------------------------------------------


class MessageDispatcher:
    __slots__ = (
        "_safe_builtins", "_library_storage", "_caller", "_logger",
        "_table", "_exec_count", "_gc_interval", "_compile_code",
    )

    def __init__(
        self,
        safe_builtins: dict[str, Any],
        library_storage: LibraryStorage,
        caller: ErlangCaller,
        logger: ErlangLogger,
        cache_size: int,
        gc_interval: int,
    ) -> None:
        self._safe_builtins = safe_builtins
        self._library_storage = library_storage
        self._caller = caller
        self._logger = logger
        self._exec_count = 0
        self._gc_interval = gc_interval
        self._compile_code = self._generate_code_cache(cache_size)
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
        try:
            name, code, target_entry, expected_version = LibraryStorage.parse_load_message(message)
        except Exception as exception:
            return _error_response(
                f"{type(exception).__name__}: {exception}\n{traceback.format_exc()}"
            )

        try:
            self._library_storage.load(name, code, target_entry, expected_version)
        except LibraryVersionError:
            meta = self._fetch_library_from_loader(name)

            if meta is None:
                return _error_response(
                    f"Library {name!r} version mismatch and not found in loader"
                )

            try:
                _, fetched_code, fetched_hash, fetched_version = meta
                self._library_storage.load_direct(
                    name, _to_str(fetched_code), bytes(fetched_hash), fetched_version
                )
            except Exception as exception:
                return _error_response(
                    f"{type(exception).__name__}: {exception}\n{traceback.format_exc()}"
                )
        except ValueError as exception:
            return _error_response(str(exception))
        except Exception as exception:
            return _error_response(
                f"{type(exception).__name__}: {exception}\n{traceback.format_exc()}"
            )

        return _ATOM_OK

    def _handle_delete_library(self, message: Any) -> Any:
        try:
            name, target_entry, expected_version = LibraryStorage.parse_delete_message(message)
        except Exception as exception:
            return _error_response(
                f"{type(exception).__name__}: {exception}\n{traceback.format_exc()}"
            )

        try:
            self._library_storage.delete(name, target_entry, expected_version)
        except LibraryVersionError:
            meta = self._fetch_library_from_loader(name)
            if meta is not None:
                return _error_response(
                    f"Library {name!r} version mismatch but still exists in loader"
                )
            try:
                self._library_storage.delete_direct(name)
            except Exception as exception:
                return _error_response(
                    f"{type(exception).__name__}: {exception}\n{traceback.format_exc()}"
                )
        except KeyError:
            return _error_response("Library not loaded")
        except ValueError as exception:
            return _error_response(str(exception))
        except Exception as exception:
            return _error_response(
                f"{type(exception).__name__}: {exception}\n{traceback.format_exc()}"
            )

        gc.collect(0)
        return _ATOM_OK

    def _fetch_library_from_loader(self, name: str) -> tuple | None:
        try:
            return self._caller.call('erl_py_runner_loader', 'get_library_meta', [name])
        except ErlangError:
            return None

    def _handle_exec(self, message: Any) -> Any:
        _, code, arguments, state = message

        namespace: dict[str, Any] = {
            "__builtins__": self._safe_builtins,
            "arguments": arguments,
            "state": state,
            "result": None,
            "erlang": self._caller,
            "logger": self._logger,
        }

        try:
            exec(self._compile_code(_to_str(code)), namespace)
            return _ok_response((namespace.get("result"), namespace.get("state")))
        except SystemExit:
            return _error_response("SystemExit is not allowed")
        except Exception as exception:
            return _error_response(
                f"{type(exception).__name__}: {exception}\n{traceback.format_exc()}"
            )
        finally:
            namespace.clear()
            self._exec_count += 1
            self._check_gc_sweep()

    @staticmethod
    def _handle_unknown(message: Any) -> Any:
        return _error_response(f"Unknown message tag: {message!r}")

    # ------------------------------------------------------------------
    # GC management
    # ------------------------------------------------------------------

    @staticmethod
    def _generate_code_cache(maxsize: int) -> Callable[[str], types.CodeType]:
        @functools.lru_cache(maxsize=maxsize)
        def compile_code(code: str) -> types.CodeType:
            return compile(code, "<erl_py_runner>", "exec")
        return compile_code

    def _check_gc_sweep(self) -> None:
        """GC Collect on Young Generation
        https://docs.python.org/3/library/gc.html
        """
        if (self._exec_count % self._gc_interval) == 0:
            gc.collect(0)


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
        except ProtocolError as exception:
            self._fatal(str(exception), exit_code=1)
        except Exception as exception:
            self._fatal(
                f"Malformed initialization message: {type(exception).__name__}: {exception}",
                exit_code=1,
            )

        if message[0] != _ATOM_INIT:
            self._fatal("Expected {init, Options} message", exit_code=1)

        _, options = message

        raw_modules = options.get(_ATOM_MODULES, _ATOM_ALL)
        cache_size = options.get(_ATOM_CACHE_SIZE, _DEFAULT_CACHE_SIZE)
        gc_interval = options.get(_ATOM_GC_INTERVAL, _DEFAULT_GC_INTERVAL)

        if raw_modules == _ATOM_ALL:
            allowed_modules: list[str] | None = None
        else:
            allowed_modules = [_to_str(m) for m in raw_modules]

        safe_builtins = _build_safe_builtins(allowed_modules)
        library_storage = LibraryStorage(safe_builtins)

        caller = ErlangCaller(self._port)
        logger = ErlangLogger(self._port)

        dispatcher = MessageDispatcher(
            safe_builtins,
            library_storage,
            caller,
            logger,
            cache_size,
            gc_interval
        )

        self._port.write(_ATOM_OK)

        gc.collect()
        gc.freeze()

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