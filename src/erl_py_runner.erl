%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner).

%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([
  start/1,
  stop/1,
  run/1, run/2
]).

-spec start(map()) -> {ok, pid()}.
start(Config) ->
  erl_py_runner_worker:start(Config).

-spec stop(pid()) -> ok.
stop(Worker) ->
  erl_py_runner_worker:stop(Worker).
  
-spec run(term()) -> term().
run(Data) ->
  erl_py_runner_worker:run(Data, _DefaultTimeout = 60000).
run(Data, Timeout) ->
  erl_py_runner_worker:run(Data, Timeout).
