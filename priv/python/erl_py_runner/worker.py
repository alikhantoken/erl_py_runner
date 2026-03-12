"""Worker Lifecycle => Handshake with Erlang => Enter the Message Loop."""

from __future__ import annotations

import gc
import sys
from typing import Any, NoReturn

from erl_py_runner.constants import (
    _ATOM_ALL,
    _ATOM_CACHE_SIZE,
    _ATOM_GC_INTERVAL,
    _ATOM_INIT,
    _ATOM_MODULES,
    _ATOM_OK,
    _DEFAULT_CACHE_SIZE,
    _DEFAULT_GC_INTERVAL,
)
from erl_py_runner.exceptions import MessageEOF, ProtocolError
from erl_py_runner.utils import _error_response, _to_str
from erl_py_runner.sandbox import _build_safe_builtins
from erl_py_runner.library import LibraryStorage
from erl_py_runner.caller import ErlangCaller
from erl_py_runner.logger import ErlangLogger
from erl_py_runner.dispatcher import MessageDispatcher
from erl_py_runner.port import ErlangPort


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
            gc_interval,
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
            except Exception as e:
                self._try_write(_error_response(exception=e))
                sys.exit(2)

    def _read_next(self) -> Any | None:
        try:
            return self._port.read()
        except MessageEOF:
            return None
        except Exception as e:
            self._try_write(_error_response(exception=e))
            sys.exit(2)

    def _fatal(self, reason: str, *, exit_code: int) -> NoReturn:
        self._try_write(_error_response(reason))
        sys.exit(exit_code)

    def _try_write(self, data: Any) -> None:
        try:
            self._port.write(data)
        except (BrokenPipeError, OSError):
            pass
