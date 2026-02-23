%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner_worker_sup).
-include("erl_py_runner.hrl").
-behaviour(supervisor).

%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([
  start_link/2,
  init/1
]).

%%% +--------------------------------------------------------------+
%%% |                         Implementation                       |
%%% +--------------------------------------------------------------+

start_link(Intensity, Period) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Intensity, Period]).

init([Intensity, Period]) ->
  Supervisor = #{
    strategy => one_for_one,
    intensity => Intensity,
    period => Period
  },
  {ok, {Supervisor, []}}.
