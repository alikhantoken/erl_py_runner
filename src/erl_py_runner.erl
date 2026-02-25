%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner).

%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([
  run/2, run/3,
  restart/0,
  info/0
]).

%%% +--------------------------------------------------------------+
%%% |                         Implementation                       |
%%% +--------------------------------------------------------------+

-spec run(binary(), term()) -> {ok, term()} | {error, term()}.
run(Code, Arguments) ->
  run(Code, Arguments, _DefaultTimeout = 60000).

-spec run(binary(), term(), timeout()) -> {ok, term()} | {error, term()}.
run(Code, Arguments, Timeout) ->
  erl_py_runner_worker:run(Code, Arguments, Timeout).
  
-spec restart() -> ok | {error, term()}.
restart() ->
  erl_py_runner_app:restart().
  
-spec info() -> list(map()).
info() ->
  erl_py_runner_worker_sup:info().