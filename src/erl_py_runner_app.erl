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
  case ?ENV(environment) of
    {ok, Environment} ->
      ResolvedEnvironment = resolve_environment(Environment),
      application:set_env(?APP_NAME, environment, ResolvedEnvironment),
      case erl_py_runner_env:ensure(ResolvedEnvironment) of
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

%%% +--------------------------------------------------------------+
%%% |                       Internal functions                      |
%%% +--------------------------------------------------------------+

resolve_environment(#{
  runner := Runner,
  requirements := Requirements,
  venv_dir := VenvDir
} = Env) ->
  Env#{
    runner := resolve_path(code:priv_dir(?APP_NAME), Runner),
    requirements := resolve_path(code:priv_dir(?APP_NAME), Requirements),
    venv_dir := resolve_path(code:priv_dir(?APP_NAME), VenvDir)
  }.

resolve_path(RootPath, Path) ->
  case filename:pathtype(Path) of
    absolute -> Path;
    relative -> filename:join(RootPath, Path);
    _Other -> Path
  end.