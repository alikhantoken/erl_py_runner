#!/bin/bash
set -euo pipefail

VENV_DIR="$1"
REQUIREMENTS="$2"
PIP="$VENV_DIR/bin/pip"

MARKER="$VENV_DIR/.installed"
REQ_HASH=$(sha256sum "$REQUIREMENTS" 2>/dev/null | cut -d' ' -f1 || true)
STORED_HASH=$(cat "$MARKER" 2>/dev/null || true)

if [ "$REQ_HASH" = "$STORED_HASH" ] && [ -f "$VENV_DIR/bin/activate" ]; then
  echo "environment up to date, skipping install"
  exit 0
fi

echo "virtual environment path set to: $VENV_DIR"
echo "requirements file path: $REQUIREMENTS"

if [ ! -f "$VENV_DIR/bin/activate" ]; then
  echo "creating virtual environment: $VENV_DIR"
  python3 -m venv "$VENV_DIR"
else
  echo "virtual environment already exists: $VENV_DIR"
fi

PY="$VENV_DIR/bin/python"

"$PY" -m pip install --upgrade pip setuptools wheel
"$PY" -m pip install --no-build-isolation "git+https://github.com/Pyrlang/Term.git@1.3#egg=pyrlang-term"

if [ -f "$REQUIREMENTS" ]; then
  echo "installing requirements from: $REQUIREMENTS"
  "$PY" -m pip install -r "$REQUIREMENTS"
else
  echo "WARNING! requirements file $REQUIREMENTS not found, skipping"
fi

echo "$REQ_HASH" > "$MARKER"