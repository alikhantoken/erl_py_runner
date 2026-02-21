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
  send_worker_start/1
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
%%% |                        API Interface                         |
%%% +--------------------------------------------------------------+

get_worker(Timeout) ->
  gen_server:call(?MODULE, ?GET_WORKER, Timeout).

send_worker_start(PID) ->
  ?MODULE ! {?WORKER_START, PID}.
  
send_worker_ready(PID) ->
  ?MODULE ! {?WORKER_READY, PID}.

%%% +--------------------------------------------------------------+
%%% |                Gen Server Behaviour Callbacks                |
%%% +--------------------------------------------------------------+

start_link(PoolSize, MaxPending, WorkerConfig) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [PoolSize, MaxPending, WorkerConfig], []).

init([PoolSize, MaxPending, WorkerConfig]) ->
  [erl_py_runner_worker:start_child(Number, WorkerConfig) || Number <- lists:seq(1, PoolSize)],
  Pool =
    #pool{
      idle = queue:new(),
      pending = queue:new(),
      max_pending = MaxPending
    },
  {ok, Pool}.

handle_call(
  ?GET_WORKER,
  Caller,
  #pool{
    idle = Idle,
    pending = Pending,
    max_pending = MaxPending
  } = Pool
) ->
  case queue:out(Idle) of
    {{value, Worker}, RestIdle} ->
      {reply, Worker, Pool#pool{idle = RestIdle}};
    {empty, _} ->
      case queue:len(Pending) >= MaxPending andalso MaxPending =/= infinity of
        true ->
          {reply, {error, overloaded}, Pool};
        false ->
          {noreply, Pool#pool{pending = queue:in(Caller, Pending)}}
      end
  end;
handle_call(UnexpectedMessage, _From, Pool) ->
  ?LOGWARNING("received unexpected message: ~p", [UnexpectedMessage]),
  {reply, {error, unexpected_message}, Pool}.

handle_info({?WORKER_START, PID}, Pool) ->
  erlang:monitor(process, PID),
  UpdatePool = dispatch(PID, Pool),
  {noreply, UpdatePool};
handle_info({?WORKER_READY, PID}, Pool) ->
  UpdatePool = dispatch(PID, Pool),
  {noreply, UpdatePool};
handle_info({'DOWN', _, process, DeadPID, _Reason}, #pool{idle = Idle} = Pool) ->
  {noreply, Pool#pool{idle = queue:filter(fun(PID) -> PID =/= DeadPID end, Idle)}};
handle_info(UnexpectedMessage, Pool) ->
  ?LOGWARNING("received unexpected message: ~p", [UnexpectedMessage]),
  {noreply, Pool}.

handle_cast(UnexpectedMessage, Pool) ->
  ?LOGWARNING("received unexpected message: ~p", [UnexpectedMessage]),
  {noreply, Pool}.

terminate(Reason, _Pool) ->
  ?LOGINFO("terminating w/ reason: ~p", [Reason]),
  ok.

%%% +--------------------------------------------------------------+
%%% |                       Internal functions                     |
%%% +--------------------------------------------------------------+

dispatch(PID, #pool{pending = Pending, idle = Idle} = Pool) ->
  case queue:out(Pending) of
    {{value, Caller}, RestPending} ->
      gen_server:reply(Caller, PID),
      Pool#pool{pending = RestPending};
    {empty, _} ->
      Pool#pool{idle = queue:in(PID, Idle)}
  end.