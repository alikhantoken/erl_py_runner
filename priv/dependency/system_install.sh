#!/bin/bash
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