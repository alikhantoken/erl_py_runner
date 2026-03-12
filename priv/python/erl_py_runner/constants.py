"""Defining protocol constants & erlang atoms & settings parameters."""

from __future__ import annotations

import struct
from typing import Final

from term.atom import Atom

# ---------------------------------------------------------------------------
# Security
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

# These module names are prohibited for user libraries.
_RESERVED_LIBRARY_NAMES: Final[frozenset[str]] = frozenset({
    "sys", "builtins", "term", "codec", "types", "erl_py_runner",
})

# ---------------------------------------------------------------------------
# Protocol framing
# ---------------------------------------------------------------------------

_MAX_MESSAGE_SIZE: Final[int] = 10 * 1024 * 1024
_DRAIN_CHUNK_SIZE: Final[int] = 65_536
_HEADER_STRUCT: Final[struct.Struct] = struct.Struct("!I")

# ---------------------------------------------------------------------------
# Erlang atoms
# ---------------------------------------------------------------------------

_ATOM_OK:             Final[Atom] = Atom("ok")
_ATOM_ERROR:          Final[Atom] = Atom("error")
_ATOM_MESSAGE:        Final[Atom] = Atom("message")
_ATOM_TRACE:          Final[Atom] = Atom("trace")
_ATOM_INIT:           Final[Atom] = Atom("init")
_ATOM_EXEC:           Final[Atom] = Atom("exec")
_ATOM_CALL:           Final[Atom] = Atom("call")
_ATOM_CALL_REPLY:     Final[Atom] = Atom("call_reply")
_ATOM_LOG:            Final[Atom] = Atom("log")
_ATOM_LOAD_LIBRARY:   Final[Atom] = Atom("load_library")
_ATOM_DELETE_LIBRARY: Final[Atom] = Atom("delete_library")
_ATOM_STATS:          Final[Atom] = Atom("stats")
_ATOM_STATS_REPLY:    Final[Atom] = Atom("stats_reply")

_ATOM_ALL:         Final[Atom] = Atom("all")
_ATOM_MODULES:     Final[Atom] = Atom("modules")
_ATOM_CACHE_SIZE:  Final[Atom] = Atom("cache_size")
_ATOM_GC_INTERVAL: Final[Atom] = Atom("gc_interval")

# ---------------------------------------------------------------------------
# Tuning parameters
# ---------------------------------------------------------------------------

# Value range for Erlang ETF INTEGER_EXT
# https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#integer_ext
_MAX_REQUEST_ID: Final[int] = (1 << 31) - 1

# Maximum entry count for LRU cache
# https://docs.python.org/3/library/functools.html
_DEFAULT_CACHE_SIZE: Final[int] = 512

# Default execution count between GC sweeps
_DEFAULT_GC_INTERVAL: Final[int] = 128
