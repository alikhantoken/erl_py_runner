%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner_worker_sup).
-include("erl_py_runner.hrl").
-include("erl_py_runner_worker.hrl").
-behaviour(supervisor).

%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([
  start_link/2,
  init/1,
  stats/0
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

stats() ->
  Requests =
    [begin
      {ID, PID, gen_server:send_request(PID, ?CALL_STATS)}
     end || {ID, PID, _, _} <- supervisor:which_children(?MODULE), is_pid(PID)],
  lists:foldl(
    fun({ID, PID, Reference}, Acc) ->
      maps:merge(Acc, #{ID => collect_worker_stats(ID, PID, Reference, ?DEADLINE(?TIMEOUT_STATS))})
    end,
    #{},
    Requests
  ).

collect_worker_stats(ID, PID, Ref, Deadline) ->
  case gen_server:receive_response(Ref, ?REMAINING(Deadline)) of
    {reply, Stats} ->
      Stats;
    timeout ->
      #{pid => PID, error => timeout};
    {error, Error} ->
      ?LOGDEBUG(
        "worker ~p with pid ~p is unavailable, error: ~p",
        [ID, PID, Error]
      ),
      #{pid => PID, error => unavailable}
  end.