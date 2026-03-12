"""Erlang callback RPC"""

from __future__ import annotations

from typing import Any

from term.atom import Atom

from erl_py_runner.constants import (
    _ATOM_CALL,
    _ATOM_CALL_REPLY,
    _ATOM_ERROR,
    _ATOM_OK,
    _MAX_REQUEST_ID,
)
from erl_py_runner.exceptions import (
    ErlangError,
    ErlangPortError,
    ProtocolError,
)
from erl_py_runner.port import ErlangPort
from erl_py_runner.utils import _to_str


class ErlangCaller:
    __slots__ = ("_port", "_next_id", "_callback_count")

    def __init__(self, port: ErlangPort) -> None:
        self._port = port
        self._next_id: int = 1
        self._callback_count: int = 0

    @property
    def callback_count(self) -> int:
        return self._callback_count

    def call(
        self,
        module: str,
        function: str,
        arguments: list[Any] | None = None,
    ) -> Any:
        """Call `Module:Function(Arguments)` in Erlang Worker Process."""
        self._callback_count += 1
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
