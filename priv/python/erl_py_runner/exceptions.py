"""Custom exceptions for the Erlang-Python Port Protocol."""

from __future__ import annotations


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
