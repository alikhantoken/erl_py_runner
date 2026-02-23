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
  stop/1,
  restart/0
]).

%%% +--------------------------------------------------------------+
%%% |                         Implementation                       |
%%% +--------------------------------------------------------------+

start(_StartType, _StartArgs) ->
  Config = application:get_all_env(?APP_NAME),
  maps:foreach(
    fun(GroupName, GroupConfig) ->
      application:set_env(?APP_NAME, GroupName, GroupConfig)
    end,
    erl_py_runner_config:verify(Config)
  ),
  erl_py_runner_sup:start_link().

stop(_State) ->
  ok.

restart() ->
  case application:stop(?APP_NAME) of
    ok ->
      case application:start(?APP_NAME) of
        ok -> ok;
        {error, Reason} -> {error, {start_failed, Reason}}
      end;
    {error, Reason} ->
      {error, {stop_failed, Reason}}
  end.