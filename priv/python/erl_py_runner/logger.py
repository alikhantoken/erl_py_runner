"""Sending log send to the Erlang Worker Process."""

from __future__ import annotations

from typing import Any, Final

from term.atom import Atom

from erl_py_runner.constants import _ATOM_LOG
from erl_py_runner.port import ErlangPort
from erl_py_runner.utils import _to_str


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
        message: Any,
    ) -> None:
        level_atom = self._LEVELS.get(_to_str(level).lower(), Atom("info"))
        payload = (_ATOM_LOG, level_atom, message)

        try:
            self._port.write(payload)
        except Exception:
            return
