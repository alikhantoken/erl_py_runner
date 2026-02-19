#!/usr/bin/env python3

# +--------------------------------------------------------------+
# | Copyright (c) 2026. All Rights Reserved.                     |
# | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
# +--------------------------------------------------------------+

import sys
import struct
import builtins
import logging
from typing import Any

logging.getLogger("term").setLevel(logging.ERROR)

logger = logging.getLogger("erl_py_runner")
logger.setLevel(logging.DEBUG)
_stderr_handler = logging.StreamHandler(sys.stderr)
_stderr_handler.setFormatter(
    logging.Formatter("%(asctime)s [%(levelname)s] %(name)s: %(message)s")
)
logger.addHandler(_stderr_handler)

from term import codec
from term.atom import Atom

MAX_MESSAGE_SIZE = 10 * 1024 * 1024

_DENIED_BUILTINS = frozenset({
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


class MessageEOF(Exception):
    """Raised when stdin is closed (clean EOF)."""


class MessageOversized(Exception):
    """Raised when message exceeds MAX_MESSAGE_SIZE."""


class MessageTruncated(Exception):
    """Raised when payload is shorter than declared length."""


def _make_restricted_import(allowed_modules: list[str]):
    allowed = frozenset(allowed_modules)
    real_import = builtins.__import__

    def restricted_import(name, globals=None, locals=None, fromlist=(), level=0):
        top_level = name.split(".")[0]
        if top_level not in allowed:
            raise ImportError(f"import of '{name}' is not allowed")
        return real_import(name, globals, locals, fromlist, level)

    return restricted_import


def _build_safe_builtins(allowed_modules: list[str]) -> dict[str, Any]:
    safe = {
        name: getattr(builtins, name)
        for name in dir(builtins)
        if name not in _DENIED_BUILTINS
    }
    if allowed_modules:
        safe["__import__"] = _make_restricted_import(allowed_modules)
    return safe


def _to_str(value: Any) -> str:
    if isinstance(value, bytes):
        return value.decode("utf-8")
    return str(value)


def read_message() -> Any:
    header = sys.stdin.buffer.read(4)
    if len(header) == 0:
        raise MessageEOF("stdin closed")
    if len(header) < 4:
        raise MessageTruncated(f"expected 4-byte header, got {len(header)} bytes")
    (length,) = struct.unpack("!I", header)
    if length > MAX_MESSAGE_SIZE:
        remaining = length
        while remaining > 0:
            chunk = sys.stdin.buffer.read(min(remaining, 65536))
            if not chunk:
                break
            remaining -= len(chunk)
        raise MessageOversized(
            f"message size {length} exceeds limit {MAX_MESSAGE_SIZE}"
        )
    payload = sys.stdin.buffer.read(length)
    if len(payload) < length:
        raise MessageTruncated(
            f"expected {length} payload bytes, got {len(payload)}"
        )
    result, _remaining = codec.binary_to_term(payload)
    return result


def write_message(data: Any) -> None:
    payload = codec.term_to_binary(data)
    header = struct.pack("!I", len(payload))
    sys.stdout.buffer.write(header + payload)
    sys.stdout.buffer.flush()


class ErlangError(Exception):
    def __init__(self, message: str, error_type: str = "exception"):
        super().__init__(message)
        self.error_type = error_type


class ErlangTimeoutError(ErlangError):
    pass


class ErlangPortError(ErlangError):
    pass


class ErlangCaller:
    def __init__(self):
        self._counter = 0

    def call(
        self,
        module: str,
        function: str,
        args: list | None = None,
    ) -> Any:
        if args is None:
            args = []
        self._counter += 1
        request_id = str(self._counter)
        write_message({
            Atom("type"): Atom("call_request"),
            Atom("request_id"): request_id,
            Atom("module"): Atom(module),
            Atom("function"): Atom(function),
            Atom("args"): args,
        })
        try:
            response = read_message()
        except (MessageEOF, MessageOversized, MessageTruncated) as e:
            raise ErlangPortError(
                f"lost connection to Erlang port: {e}", "port_error"
            ) from e
        resp_id = _to_str(response.get("request_id"))
        if resp_id != request_id:
            raise ErlangPortError(
                f"request_id mismatch: expected {request_id}, got {resp_id}",
                "port_error",
            )
        if response.get("status") == "ok":
            return response.get("result")
        error_type = _to_str(response.get("error_type", "exception"))
        error_msg = _to_str(response.get("error", "unknown error"))
        if error_type == "timeout":
            raise ErlangTimeoutError(error_msg, error_type)
        raise ErlangError(error_msg, error_type)


def handle(
    message: dict[str, Any],
    safe_builtins: dict[str, Any],
    caller: ErlangCaller,
) -> dict[str, Any]:
    code = message.get("code", b"")
    if isinstance(code, bytes):
        code = code.decode("utf-8")
    arguments = message.get("arguments", {})
    local_vars: dict[str, Any] = {"args": arguments, "result": None, "erlang": caller}
    try:
        exec(code, {"__builtins__": safe_builtins}, local_vars)
        return {Atom("status"): Atom("ok"), Atom("result"): local_vars.get("result")}
    except Exception as e:
        return {Atom("status"): Atom("error"), Atom("error"): f"{type(e).__name__}: {e}"}


def init() -> dict[str, Any]:
    try:
        message = read_message()
    except MessageEOF:
        logger.error("stdin closed before init message")
        write_message({Atom("status"): Atom("error"), Atom("error"): "stdin closed before init"})
        sys.exit(1)
    except Exception as e:
        logger.error("failed to read init message: %s", e)
        write_message({Atom("status"): Atom("error"), Atom("error"): "malformed init message"})
        sys.exit(1)
    if message is None or message.get("type") != "init":
        write_message({Atom("status"): Atom("error"), Atom("error"): "expected init message"})
        sys.exit(1)
    allowed_modules = [
        _to_str(m) for m in message.get("allowed_modules", [])
    ]
    safe_builtins = _build_safe_builtins(allowed_modules)
    write_message({Atom("status"): Atom("ok")})
    return safe_builtins


def main() -> None:
    logger.info("runner starting")
    safe_builtins = init()
    caller = ErlangCaller()
    while True:
        try:
            message = read_message()
        except MessageEOF:
            logger.info("stdin closed, shutting down")
            break
        except (MessageOversized, MessageTruncated) as e:
            logger.error("message read error: %s", e)
            write_message({Atom("status"): Atom("error"), Atom("error"): str(e)})
            continue
        except Exception as e:
            logger.error("unexpected error reading message: %s", e)
            write_message({Atom("status"): Atom("error"), Atom("error"): "malformed message"})
            continue
        response = handle(message, safe_builtins, caller)
        write_message(response)
    logger.info("runner stopped")


if __name__ == "__main__":
    main()
