"""Sandboxing utilities to restrict user code execution."""

from __future__ import annotations

import builtins
import types
from collections.abc import Callable
from typing import Any

from erl_py_runner.constants import _DENIED_BUILTINS


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
