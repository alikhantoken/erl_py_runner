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
  init/1,
  info/0
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

info() ->
  [erl_py_runner_worker:info(PID) || {_, PID, _, _} <- supervisor:which_children(?MODULE)].