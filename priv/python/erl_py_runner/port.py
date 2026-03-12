"""Erlang port I/O: 4-byte length-prefixed ETF message framing."""

from __future__ import annotations

from typing import IO, Any

from term import codec

from erl_py_runner.constants import (
    _DRAIN_CHUNK_SIZE,
    _HEADER_STRUCT,
    _MAX_MESSAGE_SIZE,
)
from erl_py_runner.exceptions import (
    MessageEOF,
    MessageOversized,
    MessageTruncated,
)


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
