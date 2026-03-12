"""Message routing, handler execution, library management and stats collection."""

from __future__ import annotations

import functools
import gc
import sys
import time
import types
from collections.abc import Callable
from typing import Any

import resource

from erl_py_runner.constants import (
    _ATOM_DELETE_LIBRARY,
    _ATOM_EXEC,
    _ATOM_LOAD_LIBRARY,
    _ATOM_OK,
    _ATOM_STATS,
    _ATOM_STATS_REPLY,
)
from erl_py_runner.exceptions import ErlangError, LibraryVersionError
from erl_py_runner.utils import _error_response, _ok_response, _read_rss_kilobytes, _to_str
from erl_py_runner.library import LibraryStorage
from erl_py_runner.caller import ErlangCaller
from erl_py_runner.logger import ErlangLogger


class MessageDispatcher:
    __slots__ = (
        "_safe_builtins", "_library_storage", "_caller", "_logger",
        "_table", "_exec_count", "_error_count", "_gc_interval",
        "_compile_code", "_start_time",
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
        self._error_count = 0
        self._gc_interval = gc_interval
        self._compile_code = self._generate_code_cache(cache_size)
        self._start_time = time.monotonic()
        self._table: dict[Any, Callable[[Any], Any]] = {
            _ATOM_LOAD_LIBRARY:   self._handle_load_library,
            _ATOM_DELETE_LIBRARY: self._handle_delete_library,
            _ATOM_EXEC:           self._handle_exec,
            _ATOM_STATS:          self._handle_stats,
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
        except Exception as e:
            return _error_response(exception=e)

        try:
            self._library_storage.load(name, code, target_entry, expected_version)
        except LibraryVersionError:
            meta = self._fetch_library_from_loader(name)

            if meta is None:
                return _error_response(f"Library {name!r} version mismatch and not found in loader")

            try:
                _, fetched_code, fetched_hash, fetched_version = meta
                self._library_storage.load_direct(
                    name, _to_str(fetched_code), bytes(fetched_hash), fetched_version
                )
            except Exception as e:
                return _error_response(exception=e)
        except ValueError as e:
            return _error_response(exception=e)
        except Exception as e:
            return _error_response(exception=e)

        return _ATOM_OK

    def _handle_delete_library(self, message: Any) -> Any:
        try:
            name, target_entry, expected_version = LibraryStorage.parse_delete_message(message)
        except Exception as e:
            return _error_response(exception=e)

        try:
            self._library_storage.delete(name, target_entry, expected_version)
        except LibraryVersionError:
            meta = self._fetch_library_from_loader(name)
            if meta is not None:
                return _error_response(f"Library {name!r} version mismatch but still exists in loader")
            try:
                self._library_storage.delete_direct(name)
            except Exception as e:
                return _error_response(exception=e)
        except KeyError as e:
            return _error_response("Library not loaded", exception=e)
        except ValueError as e:
            return _error_response(exception=e)
        except Exception as e:
            return _error_response(exception=e)

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
        except SystemExit as e:
            self._error_count += 1
            return _error_response("SystemExit is not allowed", exception=e)
        except Exception as e:
            self._error_count += 1
            return _error_response(exception=e)
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
        """GC Collect on Young Generation.
        https://docs.python.org/3/library/gc.html
        """
        if (self._exec_count % self._gc_interval) == 0:
            gc.collect(0)

    # ------------------------------------------------------------------
    # Stats collection
    # ------------------------------------------------------------------

    def _handle_stats(self, _message: Any) -> Any:
        return _ATOM_STATS_REPLY, self._collect_stats()

    def _collect_stats(self) -> dict[str, Any]:
        return {
            "exec_count": self._exec_count,
            "error_count": self._error_count,
            "callback_count": self._caller.callback_count,
            "uptime_seconds": int(time.monotonic() - self._start_time),
            "python_version": sys.version,
            "gc_collections": self._collect_gc_stats(),
            "libraries": self._collect_library_stats(),
            "memory": self._collect_memory_stats(),
            "code_cache": self._collect_code_cache_stats(),
        }

    @staticmethod
    def _collect_gc_stats() -> dict[str, Any]:
        young, middle, old = gc.get_count()
        return {
            "young": young,
            "middle": middle,
            "old": old,
        }

    @staticmethod
    def _collect_memory_stats() -> dict[str, Any]:
        return {
            "resident_memory_kilobytes": _read_rss_kilobytes() or 0,
            "peak_resident_memory_kilobytes": resource.getrusage(resource.RUSAGE_SELF).ru_maxrss,
        }

    def _collect_library_stats(self) -> dict[str, Any]:
        names = self._library_storage.list_names()
        return {
            "count": len(names),
            "names": names,
        }

    def _collect_code_cache_stats(self) -> dict[str, Any]:
        cache_info = self._compile_code.cache_info()
        return {
            "hits": cache_info.hits,
            "misses": cache_info.misses,
            "current_size": cache_info.currsize,
            "max_size": cache_info.maxsize,
        }
