from __future__ import annotations

import traceback
from typing import Any

from term.atom import Atom

from erl_py_runner.constants import (
    _ATOM_ERROR,
    _ATOM_MESSAGE,
    _ATOM_OK,
    _ATOM_TRACE,
)


def _to_str(value: Any) -> str:
    return value.decode("utf-8") if isinstance(value, bytes) else str(value)


def _ok_response(result: Any) -> tuple[Atom, Any]:
    return _ATOM_OK, result


def _error_response(
    message: str | None = None,
    exception: BaseException | None = None,
) -> tuple[Atom, dict]:
    if exception is not None:
        msg = message if message is not None else f"{type(exception).__name__}: {exception}"
        trace = "".join(traceback.format_exception(exception))
    else:
        msg = message or ""
        trace = ""
    return _ATOM_ERROR, {_ATOM_MESSAGE: msg, _ATOM_TRACE: trace}


def _read_rss_kilobytes() -> int | None:
    """Return current RSS in kilobytes from /proc/self/status on Linux."""
    try:
        with open("/proc/self/status") as proc_status:
            for line in proc_status:
                if line.startswith("VmRSS:"):
                    return int(line.split()[1])
    except OSError:
        return None
