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
  start_link/2,
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

start_link(PoolSize, WorkerConfig) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [PoolSize, WorkerConfig], []).

init([PoolSize, WorkerConfig]) ->
  Workers =
    [erl_py_runner_worker:start_child(Number, WorkerConfig) || Number <- lists:seq(1, PoolSize)],
  {ok, #pool{idle = Workers, pending = []}}.

handle_call(?GET_WORKER, _Caller, #pool{idle = [Worker | Idle]} = Pool) ->
  {reply, Worker, Pool#pool{idle = Idle}};
handle_call(?GET_WORKER, Caller, #pool{idle = [], pending = Pending} = Pool) ->
  {noreply, Pool#pool{pending = Pending ++ [Caller]}};
handle_call(UnexpectedMessage, _From, Pool) ->
  ?LOGWARNING("received unexpected message: ~p", [UnexpectedMessage]),
  {reply, {error, unexpected_message}, Pool}.

handle_info({?WORKER_START, PID}, Pool) ->
  erlang:monitor(process, PID),
  dispatch(PID, Pool);
handle_info({?WORKER_READY, PID}, Pool) ->
  dispatch(PID, Pool);
handle_info({'DOWN', _, process, PID, _Reason}, #pool{idle = Idle} = Pool) ->
  {noreply, Pool#pool{idle = lists:delete(PID, Idle)}};
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

dispatch(PID, #pool{pending = [Caller | Rest]} = Pool) ->
  gen_server:reply(Caller, PID),
  {noreply, Pool#pool{pending = Rest}};
dispatch(PID, #pool{pending = [], idle = Idle} = Pool) ->
  {noreply, Pool#pool{idle = Idle ++ [PID]}}.