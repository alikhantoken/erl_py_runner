%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner_loader).
-behaviour(gen_server).

-include("erl_py_runner.hrl").
-include("erl_py_runner_loader.hrl").

%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([
  start_link/0,
  load_library/2,
  delete_library/1,
  get_libraries/0,
  get_libraries_meta/0,
  get_library_meta/1
]).

%%% +--------------------------------------------------------------+
%%% |                     Gen Server Callbacks                     |
%%% +--------------------------------------------------------------+

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2
]).

%%% +--------------------------------------------------------------+
%%% |                          Public API                          |
%%% +--------------------------------------------------------------+

%%% IMPORTANT!
%%% The pool is NOT involved in library loading.
%%% This loader process acts as the single source of truth for
%%% which libraries must be present in every worker.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

load_library(Name, Code) ->
  case is_library_name_valid(Name) of
    false ->
      {error, invalid_library_name};
    true ->
      {ok, #{config := #{max_code_size := MaxCodeSize}}} = ?ENV(worker),
      case byte_size(Code) =< MaxCodeSize of
        true -> gen_server:call(?MODULE, ?CALL_LOAD_LIBRARY(Name, Code), ?TIMEOUT_LOAD_LIBRARY);
        false -> {error, code_too_large}
      end
  end.

delete_library(Name) ->
  case is_library_name_valid(Name) of
    true -> gen_server:call(?MODULE, ?CALL_DELETE_LIBRARY(Name), ?TIMEOUT_LOAD_LIBRARY);
    false -> {error, invalid_library_name}
  end.

get_library_meta(Name) ->
  case ets:lookup(?LIBRARIES_TAB, Name) of
    [#library{name = Name, code = Code, hash = Hash, version = Version}] ->
      {ok, {Name, Code, Hash, Version}};
    [] ->
      {error, not_found}
  end.

get_libraries() ->
  {ok, [{Name, Code} || #library{name = Name, code = Code} <- sorted_libraries()]}.

get_libraries_meta() ->
  {ok, sorted_libraries()}.

%%% +--------------------------------------------------------------+
%%% |                     Gen Server Callbacks                     |
%%% +--------------------------------------------------------------+

init([]) ->
  ets:new(?LIBRARIES_TAB, [
    named_table,
    protected,
    set,
    {keypos, #library.name},
    {read_concurrency, true}
  ]),
  {ok, #loader{version_counter = 0}}.

handle_call(
  ?CALL_LOAD_LIBRARY(Name, Code),
  _Caller,
  #loader{version_counter = VersionCounter} = State
) ->
  WorkerPIDs = erl_py_runner_pool:get_workers(),
  case do_load_library(WorkerPIDs, Name, Code, VersionCounter) of
    {ok, NewVersionCounter} ->
      {reply, ok, State#loader{version_counter = NewVersionCounter}};
    {error, Errors} ->
      {reply, {error, Errors}, State}
  end;

handle_call(
  ?CALL_DELETE_LIBRARY(Name),
  _Caller,
  #loader{version_counter = VersionCounter} = State
) ->
  WorkerPIDs = erl_py_runner_pool:get_workers(),
  case do_delete_library(WorkerPIDs, Name, VersionCounter) of
    {ok, NewVersionCounter} ->
      {reply, ok, State#loader{version_counter = NewVersionCounter}};
    {error, Errors} ->
      {reply, {error, Errors}, State}
  end;

handle_call(Unexpected, _From, State) ->
  ?LOGWARNING("received unexpected message: ~p", [Unexpected]),
  {reply, {error, unexpected_message}, State}.

handle_cast(Unexpected, State) ->
  ?LOGWARNING("received unexpected message: ~p", [Unexpected]),
  {noreply, State}.

handle_info(Unexpected, State) ->
  ?LOGWARNING("received unexpected message: ~p", [Unexpected]),
  {noreply, State}.

terminate(Reason, _State) ->
  ?LOGINFO("terminating, reason: ~p", [Reason]),
  ok.

%%% +--------------------------------------------------------------+
%%% |                       Internal Functions                     |
%%% +--------------------------------------------------------------+

is_library_name_valid(Name) when is_binary(Name), byte_size(Name) > 0 ->
  re:run(Name, <<"^(?:_)?[a-z][a-z0-9_]*$">>, [{capture, none}]) =:= match;
is_library_name_valid(_) ->
  false.

do_load_library(WorkerPIDs, Name, Code, VersionCounter) ->
  {Library, ExpectedVersion, NewVersionCounter} = prepare_library(Name, Code, VersionCounter),
  LoadFun =
    fun(PID) ->
      try erl_py_runner_worker:load_library(PID, Library, ExpectedVersion)
      catch _Class:Error -> {error, {load_failed, Error}}
      end
    end,
  case broadcast(WorkerPIDs, LoadFun, ?DEADLINE(?TIMEOUT_LOAD_LIBRARY)) of
    {_, []} ->
      ?LOGINFO(
        "successfully loaded library ~ts on ~p workers",
        [Name, length(WorkerPIDs)]
      ),
      ets:insert(?LIBRARIES_TAB, Library),
      {ok, NewVersionCounter};
    {[], Errors} ->
      ?LOGWARNING(
        "failed to load library ~ts on all workers: ~p",
        [Name, Errors]
      ),
      {error, Errors};
    {_SuccessfulPIDs, FailedWorkers} ->
      ?LOGWARNING(
        "partial broadcast failure for library ~ts, restarting ~p failed workers: ~p",
        [Name, length(FailedWorkers), FailedWorkers]
      ),
      ets:insert(?LIBRARIES_TAB, Library),
      restart_workers(FailedWorkers),
      {ok, NewVersionCounter}
  end.

do_delete_library(WorkerPIDs, Name, VersionCounter) ->
  case ets:lookup(?LIBRARIES_TAB, Name) of
    [] ->
      {error, not_found};
    [#library{hash = Hash, version = CurrentVersion}] ->
      DeleteVersion = VersionCounter + 1,
      DeleteLibrary = #library{name = Name, hash = Hash, version = DeleteVersion},
      DeleteFun =
        fun(PID) ->
          try erl_py_runner_worker:delete_library(PID, DeleteLibrary, CurrentVersion)
          catch _Class:Error -> {error, {delete_failed, Error}}
          end
        end,
      case broadcast(WorkerPIDs, DeleteFun, ?DEADLINE(?TIMEOUT_LOAD_LIBRARY)) of
        {_, []} ->
          ?LOGINFO(
            "successfully deleted library ~ts from ~p workers",
            [Name, length(WorkerPIDs)]
          ),
          ets:delete(?LIBRARIES_TAB, Name),
          {ok, DeleteVersion};
        {[], Errors} ->
          ?LOGWARNING(
            "failed to delete library ~ts from all workers: ~p",
            [Name, Errors]
          ),
          {error, Errors};
        {_SuccessfulPIDs, FailedWorkers} ->
          ?LOGWARNING(
            "partial broadcast failure for delete ~ts, restarting ~p failed workers: ~p",
            [Name, length(FailedWorkers), FailedWorkers]
          ),
          ets:delete(?LIBRARIES_TAB, Name),
          restart_workers(FailedWorkers),
          {ok, DeleteVersion}
      end
  end.

prepare_library(Name, Code, VersionCounter) ->
  Hash = crypto:hash(sha256, Code),
  Base = #library{name = Name, code = Code, hash = Hash},
  case ets:lookup(?LIBRARIES_TAB, Name) of
    [#library{hash = Hash, version = Version, insert_order = Order}] ->
      {Base#library{version = Version, insert_order = Order}, Version, VersionCounter};
    [#library{version = Version, insert_order = Order}] ->
      NewVersion = VersionCounter + 1,
      {Base#library{version = NewVersion, insert_order = Order}, Version, NewVersion};
    [] ->
      NewVersion = VersionCounter + 1,
      {Base#library{version = NewVersion, insert_order = NewVersion}, 0, NewVersion}
  end.

restart_workers([]) ->
  ok;
restart_workers(FailedWorkers) ->
  FailedPIDs = [PID || {PID, _Reason} <- FailedWorkers],
  ?LOGWARNING("restarting ~p workers: ~p", [length(FailedPIDs), FailedWorkers]),
  [catch exit(PID, shutdown) || PID <- FailedPIDs],
  ok.

broadcast(WorkerPIDs, RequestFun, Deadline) ->
  Owner = self(),
  Pending =
    maps:from_list(
      [begin
         Ref = make_ref(),
         spawn(
           fun() ->
             Owner ! {broadcast_reply, Ref, PID, RequestFun(PID)}
           end
         ),
         {Ref, PID}
       end || PID <- WorkerPIDs]
    ),
  collect(#broadcast_status{pending = Pending, successful = [], errors = []}, Deadline).

collect(#broadcast_status{pending = Pending, successful = Successful, errors = Errors}, _Deadline)
    when map_size(Pending) =:= 0 ->
  {Successful, Errors};
collect(#broadcast_status{pending = Pending, successful = Successful, errors = Errors} = Acc, Deadline) ->
  receive
    {broadcast_reply, Ref, _PID, Result} ->
      case maps:take(Ref, Pending) of
        {ExpectedPID, Rest} ->
          case Result of
            ok ->
              collect(Acc#broadcast_status{pending = Rest, successful = [ExpectedPID | Successful]}, Deadline);
            {error, Reason} ->
              collect(Acc#broadcast_status{pending = Rest, errors = [{ExpectedPID, Reason} | Errors]}, Deadline)
          end;
        error ->
          collect(Acc, Deadline)
      end
  after
    ?REMAINING(Deadline) ->
      TimeoutErrors = maps:fold(fun(_Ref, PID, Acc2) -> [{PID, timeout} | Acc2] end, Errors, Pending),
      {Successful, TimeoutErrors}
  end.

sorted_libraries() ->
  lists:sort(
    fun(A, B) ->
      A#library.insert_order =< B#library.insert_order
    end,
    ets:tab2list(?LIBRARIES_TAB)
  ).