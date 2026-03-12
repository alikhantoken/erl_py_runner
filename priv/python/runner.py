#!/usr/bin/env python3
"""Erlang port process that executes custom Python Code.

Communicates via stdin/stdout using 4-byte length-prefixed ETF messages.
Receives an ``init`` handshake with an options map, then enters a loop:
reads code-execution requests, runs them via ``exec()``, and returns results.

Exit codes:
    0 = normal end of file (EOF)
    1 = handshake failure
    2 = communication failure
"""

# +--------------------------------------------------------------+
# | Copyright (c) 2026. All Rights Reserved.                     |
# | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
# +--------------------------------------------------------------+

from __future__ import annotations

import sys

from erl_py_runner.port import ErlangPort
from erl_py_runner.worker import Worker

def main() -> None:
    Worker(ErlangPort(sys.stdin.buffer, sys.stdout.buffer)).run()

if __name__ == "__main__":
    main()