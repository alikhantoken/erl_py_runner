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
  Requests =
    [begin
      {PID, gen_server:send_request(PID, ?CALL_INFO)}
     end || {_, PID, _, _} <- supervisor:which_children(?MODULE), is_pid(PID)],
  [collect_worker_info(PID, Ref, ?DEADLINE(?TIMEOUT_INFO)) || {PID, Ref} <- Requests].

collect_worker_info(PID, Ref, Deadline) ->
  case gen_server:receive_response(Ref, ?REMAINING(Deadline)) of
    {reply, Info} -> Info;
    timeout -> #{pid => PID, error => timeout};
    {error, _} -> #{pid => PID, error => unavailable}
  end.