"""Python Library Loading => Versioning and Lifecycle management."""

from __future__ import annotations

import sys
import types
from dataclasses import dataclass
from typing import Any

from erl_py_runner.constants import _RESERVED_LIBRARY_NAMES
from erl_py_runner.exceptions import LibraryVersionError
from erl_py_runner.utils import _to_str


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

    def list_names(self) -> list[str]:
        return [name for name, entry in self._entries.items() if entry.loaded]

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
            loaded=True,
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
    # Message parsers
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
