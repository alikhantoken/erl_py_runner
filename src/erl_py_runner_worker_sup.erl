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
  start_link/0,
  init/1
]).

%%% +--------------------------------------------------------------+
%%% |                         Implementation                       |
%%% +--------------------------------------------------------------+

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Supervisor = #{
    strategy => one_for_one,
    intensity => ?DEFAULT_SUP_INTENSITY,
    period => ?DEFAULT_SUP_PERIOD
  },
  {ok, {Supervisor, []}}.
