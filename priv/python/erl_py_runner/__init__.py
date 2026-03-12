"""Erlang Port Runtime Package for 'erl_py_runner'."""

# REQUIRED to supress pyterm codec warning on application launch.
import logging as _logging
_logging.getLogger("term").setLevel(_logging.ERROR)
del _logging
