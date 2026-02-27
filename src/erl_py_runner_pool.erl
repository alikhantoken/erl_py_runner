%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner_pool).
-behaviour(gen_server).

-include("erl_py_runner.hrl").
-include("erl_py_runner_pool.hrl").

%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([
  get_worker/1,
  send_worker_ready/1,
  send_worker_start/1,
  send_load_library/2
]).

%%% +--------------------------------------------------------------+
%%% |                       Gen Server Behaviour                   |
%%% +--------------------------------------------------------------+

-export([
  start_link/3,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2
]).

%%% +--------------------------------------------------------------+
%%% |                         Public API                           |
%%% +--------------------------------------------------------------+

%% Called by any process that wants to run a script.
%%   Non-Blocking: Tries to acquire a worker right away through ETS for idle workers.
%%   Blocking: Otherwise, it calls the gen_server and waits until one is free.
get_worker(Timeout) ->
  case pop_idle() of
    {ok, Worker} ->
      {ok, Worker};
    empty ->
      gen_server:call(?MODULE, ?GET_WORKER, Timeout)
  end.

send_worker_start(PID) ->
  ?MODULE ! ?WORKER_START(PID).
  
send_worker_ready(PID) ->
  ?MODULE ! ?WORKER_READY(PID).

send_load_library(Name, Code) ->
  gen_server:call(?MODULE, ?CALL_LOAD_LIBRARY(Name, Code), ?TIMEOUT_LOAD_LIBRARY).

%%% +--------------------------------------------------------------+
%%% |                Gen Server Behaviour Callbacks                |
%%% +--------------------------------------------------------------+

start_link(PoolSize, MaxPending, WorkerConfig) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [PoolSize, MaxPending, WorkerConfig], []).

init([PoolSize, MaxPending, WorkerConfig]) ->
  ets:new(?IDLE_WORKERS_TAB, [
    named_table,
    public,
    set,
    {read_concurrency, true}
  ]),
  [erl_py_runner_worker:start_child(Number, WorkerConfig) || Number <- lists:seq(1, PoolSize)],
  {ok, #pool{
    max_pending = MaxPending,
    worker_monitors = #{},
    caller_monitors = #{},
    pending = queue:new(),
    pending_size = 0
  }}.
  
%% Blocking operation: the caller couldn't get a worker from ETS.
%% Check ETS again (a worker might have returned after the caller checked).
%% If still none, either put the caller in the queue or reject the request.
handle_call(
  ?GET_WORKER,
  Caller,
  #pool{
    pending = PendingQueue,
    pending_size = Size,
    max_pending = Max,
    caller_monitors = CalledMonitors
  } = Pool
) ->
  case pop_idle() of
    {ok, WorkerPID} ->
      {reply, {ok, WorkerPID}, Pool};
    empty ->
      case Max =/= infinity andalso Size >= Max of
        true ->
          {reply, {error, overloaded}, Pool};
        false ->
          {CallerPID, _Tag} = Caller,
          MonRef = erlang:monitor(process, CallerPID),
          {noreply, Pool#pool{
            pending = queue:in({Caller, MonRef}, PendingQueue),
            pending_size = Size + 1,
            caller_monitors = CalledMonitors#{MonRef => true}
          }}
      end
  end;

handle_call(
  ?CALL_LOAD_LIBRARY(Name, Code),
  _From,
  #pool{worker_monitors = WorkerMonitors} = Pool
) ->
  Errors =
    lists:foldl(
      fun(WorkerPID, Acc) ->
        case gen_server:call(WorkerPID, ?CALL_LOAD_LIBRARY(Name, Code), ?TIMEOUT_LOAD_LIBRARY) of
          ok ->
            Acc;
          {error, Reason} ->
            [{WorkerPID, Reason} | Acc]
        end
      end,
      [],
      maps:values(WorkerMonitors)
    ),
  Reply =
    case Errors of
      [] -> ok;
      _  -> {error, Errors}
    end,
  {reply, Reply, Pool};

handle_call(Unexpected, _From, Pool) ->
  ?LOGWARNING("pool received unexpected call: ~p", [Unexpected]),
  {reply, {error, unexpected_message}, Pool}.

handle_info(
  ?WORKER_START(PID),
  #pool{worker_monitors = WorkerMonitors} = Pool
) ->
  MonRef = erlang:monitor(process, PID),
  NewPool = Pool#pool{worker_monitors = WorkerMonitors#{MonRef => PID}},
  {noreply, dispatch(PID, NewPool)};

handle_info(
  ?WORKER_READY(PID),
  Pool
) ->
  {noreply, dispatch(PID, Pool)};

handle_info(
  {'DOWN', MonRef, process, DeadPID, Reason},
  #pool{worker_monitors = WorkerMonitors, caller_monitors = CallerMonitors} = Pool
) ->
  NewPool =
    case maps:take(MonRef, WorkerMonitors) of
      % A worker died, remove from idle ETS.
      {_PID, NewWorkerMonitors} ->
        ?LOGWARNING("received down from worker: ~p, reason: ~p", [DeadPID, Reason]),
        ets:delete(?IDLE_WORKERS_TAB, DeadPID),
        Pool#pool{worker_monitors = NewWorkerMonitors};
      % A caller died, remove from the pending queue.
      error ->
        case CallerMonitors of
          #{MonRef := true} ->
            Filtered =
              [Element || {_Caller, Ref} = Element <- queue:to_list(Pool#pool.pending), Ref =/= MonRef],
            Pool#pool{
              pending = queue:from_list(Filtered),
              pending_size = Pool#pool.pending_size - 1,
              caller_monitors = maps:remove(MonRef, CallerMonitors)
            };
          _Unexpected ->
            ?LOGWARNING("pool received DOWN for unknown monitor: ~p, pid: ~p", [MonRef, DeadPID]),
            Pool
        end
    end,
  {noreply, NewPool};

handle_info(Unexpected, Pool) ->
  ?LOGWARNING("pool received unexpected info: ~p", [Unexpected]),
  {noreply, Pool}.

handle_cast(Unexpected, Pool) ->
  ?LOGWARNING("pool received unexpected cast: ~p", [Unexpected]),
  {noreply, Pool}.

terminate(Reason, _Pool) ->
  ?LOGINFO("pool terminating, reason: ~p", [Reason]),
  ok.

%%% +--------------------------------------------------------------+
%%% |                      Internal Functions                      |
%%% +--------------------------------------------------------------+

pop_idle() ->
  case ets:first(?IDLE_WORKERS_TAB) of
    '$end_of_table' ->
      empty;
    PID ->
      case ets:take(?IDLE_WORKERS_TAB, PID) of
        [{PID}] ->
          {ok, PID};
        [] ->
          pop_idle()
      end
  end.

%% Give the worker to the oldest waiting caller, or store it as idle in ETS.
dispatch(
  PID,
  #pool{
    pending = PendingQueue,
    pending_size = Size,
    caller_monitors = CallerMonitors
  } = Pool
) ->
  case queue:out(PendingQueue) of
    {{value, {Caller, MonRef}}, RestPendingQueue} ->
      erlang:demonitor(MonRef, [flush]),
      gen_server:reply(Caller, {ok, PID}),
      Pool#pool{
        pending = RestPendingQueue,
        pending_size = Size - 1,
        caller_monitors = maps:remove(MonRef, CallerMonitors)
      };
    {empty, _} ->
      ets:insert(?IDLE_WORKERS_TAB, {PID}),
      Pool
  end.