%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner).

%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([
  run/2, run/3, run/4,
  load_library/2,
  delete_library/1,
  restart/0,
  info/0
]).

%%% +--------------------------------------------------------------+
%%% |                         Implementation                       |
%%% +--------------------------------------------------------------+

%% BLOCKING OPERATION
%% The simplest form of call request to run a Python snippet.
%% Picks a free worker from the pool, runs the code, and returns the result.
%% Inside the snippet, arguments are available as 'arguments' and you
%% can store the output in 'result' if needed.
%% Args:
%%   Code: Python snippet to execute. Result is assigned to the 'result' variable.
%%   Arguments: Any Erlang term. Visible inside the snippet as 'arguments.
-spec run(binary(), term()) -> {ok, term(), term()} | {error, term()}.
run(Code, Arguments) ->
  erl_py_runner_worker:run(Code, Arguments).

%% BLOCKING OPERATION
%% This is like run/2, but also passes caller-owned state into the snippet.
%% Args:
%%   Code: Python snippet to execute. Result is assigned to the 'result' variable.
%%   Arguments: Any Erlang term. Visible inside the snippet as 'arguments'.
%%   State: Any Erlang value from the previous call; visible inside the snippet as 'state'.
-spec run(binary(), term(), term()) -> {ok, term(), term()} | {error, term()}.
run(Code, Arguments, State) ->
  erl_py_runner_worker:run(Code, Arguments, State).

%% BLOCKING OPERATION
%% This is like run/3, but lets you set a hard time limit for the call request.
%% If the limit is exceeded, the worker is stopped immediately and {error, timeout}
%% is returned to the caller.
%% Args:
%%   Code: Python snippet to execute. Result is assigned to the 'result' variable.
%%   Arguments: Any Erlang term. Visible inside the snippet as 'arguments'.
%%   State: Any Erlang value from the previous call; visible inside the snippet as 'state'.
%%   Timeout: Maximum allowed execution time in milliseconds.
-spec run(binary(), term(), term(), timeout()) -> {ok, term(), term()} | {error, term()}.
run(Code, Arguments, State, Timeout) ->
  erl_py_runner_worker:run(Code, Arguments, State, Timeout).

%% BLOCKING OPERATION
%% Installs a Python module (library) across all pool workers so that any
%% subsequent code snippets can import it by name.
%% If a library with the same name already exists, it is replaced in place.
%% All workers confirm the load before this call returns.
%% Args:
%%   Name: Unique name for the library (e.g. <<"my_library">>).
%%         Lowercase letters, digits, and underscores only.
%%   Code: Full Python source of the module.
%%         Must not exceed the max_code_size limit.
-spec load_library(binary(), binary()) -> ok | {error, term()}.
load_library(Name, Code) ->
  erl_py_runner_loader:load_library(Name, Code).

%% BLOCKING OPERATION
%% Unloads a previously installed library from all pool workers.
%% Args:
%%   Name: Name of the library to remove, as previously passed to load_library/2.
-spec delete_library(binary()) -> ok | {error, term()}.
delete_library(Name) ->
  erl_py_runner_loader:delete_library(Name).

%% BLOCKING OPERATION
%% Restarts the entire application and starts it in a fresh state.
%% This is a heavy operation. Should be used for situations where
%% pool cannot recover on its own.
-spec restart() -> ok | {error, term()}.
restart() ->
  erl_py_runner_app:restart().

%% BLOCKING OPERATION
%% Returns a live snapshot of every worker in the pool, one map per worker.
%% Workers that do not respond within the deadline appear in the list with an
%% error field rather than being silently skipped. Used for debugging.
-spec info() -> list(map()).
info() ->
  erl_py_runner_worker_sup:info().