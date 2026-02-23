%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner).

%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([
  run/1, run/2,
  restart/0
]).

%%% +--------------------------------------------------------------+
%%% |                         Implementation                       |
%%% +--------------------------------------------------------------+

-spec run(term()) -> {ok, term()} | {error, term()}.
run(Data) ->
  run(Data, _DefaultTimeout = 60000).

-spec run(term(), timeout()) -> {ok, term()} | {error, term()}.
run(Data, Timeout) ->
  erl_py_runner_worker:run(Data, Timeout).
  
-spec restart() -> ok | {error, term()}.
restart() ->
  erl_py_runner_app:restart().