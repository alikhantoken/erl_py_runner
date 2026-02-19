%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner_app).
-behaviour(application).
-include("erl_py_runner.hrl").

%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([
  start/2,
  stop/1
]).

%%% +--------------------------------------------------------------+
%%% |                         Implementation                       |
%%% +--------------------------------------------------------------+

start(_StartType, _StartArgs) ->
  case ?ENV(worker) of
    {ok, #{environment := Environment}} ->
      case erl_py_runner_env:ensure(Environment) of
        ok ->
          erl_py_runner_sup:start_link();
        {error, Reason} ->
          {error, Reason}
      end;
    _NotExists ->
      {error, worker_config_missing}
  end.

stop(_State) ->
  ok.