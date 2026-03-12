#!/bin/bash
# This script verifies that the Pyrlang/Term library is available and installed.
# Github Link: https://github.com/Pyrlang/Term
# License: Apache License 2.0: https://www.apache.org/licenses/LICENSE-2.0
set -euo pipefail

PYTHON="${1:-python3}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PYTHON_DIR="$(cd "$SCRIPT_DIR/../python" && pwd)"

export PYTHONPATH="$PYTHON_DIR${PYTHONPATH:+:$PYTHONPATH}"

echo "using system python: $PYTHON"
echo "python package path: $PYTHON_DIR"

if "$PYTHON" -c "from term import codec" 2>/dev/null; then
    echo "pyrlang-term available"
else
    echo "ERROR: pyrlang-term not importable from $PYTHON_DIR" >&2
    exit 1
fi