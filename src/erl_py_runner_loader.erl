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
  get_libraries/0
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
  gen_server:call(?MODULE, ?CALL_LOAD_LIBRARY(Name, Code), ?TIMEOUT_LOAD_LIBRARY).

delete_library(Name) ->
  gen_server:call(?MODULE, ?CALL_DELETE_LIBRARY(Name), ?TIMEOUT_LOAD_LIBRARY).

get_libraries() ->
  gen_server:call(?MODULE, ?CALL_GET_LIBRARIES, ?TIMEOUT_GET_LIBRARIES).

%%% +--------------------------------------------------------------+
%%% |                     Gen Server Callbacks                     |
%%% +--------------------------------------------------------------+

init([]) ->
  {ok, #loader{
    libraries = [],
    version_counter = 0
  }}.

handle_call(
  ?CALL_LOAD_LIBRARY(Name, Code),
  _Caller,
  #loader{libraries = Libraries, version_counter = VersionCounter} = State
) ->
  WorkerPIDs = erl_py_runner_pool:get_workers(),
  case do_load_library(WorkerPIDs, Name, Code, Libraries, VersionCounter) of
    {ok, NewLibraries, NewVersionCounter} ->
      {reply, ok, State#loader{
        libraries = NewLibraries,
        version_counter = NewVersionCounter
      }};
    {error, BroadcastErrors} ->
      {reply, {error, BroadcastErrors}, State};
    {error, BroadcastErrors, RollbackErrors} ->
      {reply, {error, {broadcast_failed, BroadcastErrors, rollback_failed, RollbackErrors}}, State}
  end;

handle_call(
  ?CALL_DELETE_LIBRARY(Name),
  _Caller,
  #loader{libraries = Libraries} = State
) ->
  WorkerPIDs = erl_py_runner_pool:get_workers(),
  case do_delete_library(WorkerPIDs, Name, Libraries) of
    {ok, NewLibraries} ->
      {reply, ok, State#loader{
        libraries = NewLibraries
      }};
    {error, not_found} ->
      {reply, {error, not_found}, State};
    {error, DeleteErrors} ->
      {reply, {error, DeleteErrors}, State};
    {error, DeleteErrors, RollbackErrors} ->
      {reply, {error, {broadcast_failed, DeleteErrors, rollback_failed, RollbackErrors}}, State}
  end;

handle_call(
  ?CALL_GET_LIBRARIES,
  _Caller,
  #loader{
    libraries = Libraries
  } = State
) ->
  Result = [{Name, Code} || #library{name = Name, code = Code} <- Libraries],
  {reply, {ok, Result}, State};

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

do_load_library(WorkerPIDs, Name, Code, Libraries, VersionCounter) ->
  Deadline = erlang:monotonic_time(millisecond) + ?TIMEOUT_LOAD_LIBRARY,
  case broadcast_library(WorkerPIDs, Name, Code, Deadline) of
    {_, []} ->
      {NewLibraries, NewVersionCounter} = insert_library(Name, Code, Libraries, VersionCounter),
      {ok, NewLibraries, NewVersionCounter};
    {SuccessfulPIDs, BroadcastErrors} ->
      case rollback_library(SuccessfulPIDs, Name, Libraries, Deadline) of
        [] ->
          {error, BroadcastErrors};
        RollbackErrors ->
          {error, BroadcastErrors, RollbackErrors}
      end
  end.

do_delete_library(WorkerPIDs, Name, Libraries) ->
  Deadline = erlang:monotonic_time(millisecond) + ?TIMEOUT_LOAD_LIBRARY,
  case lists:keyfind(Name, #library.name, Libraries) of
    false ->
      {error, not_found};
    #library{code = Code} ->
      case broadcast_delete(WorkerPIDs, Name, Deadline) of
        {_, []} ->
          {ok, lists:keydelete(Name, #library.name, Libraries)};
        {SuccessfulPIDs, DeleteErrors} ->
          case rollback_delete(SuccessfulPIDs, Name, Code, Deadline) of
            [] ->
              {error, DeleteErrors};
            RollbackErrors ->
              {error, DeleteErrors, RollbackErrors}
          end
      end
  end.

insert_library(Name, Code, Libraries, VersionCounter) ->
  Hash = crypto:hash(sha256, Code),
  case lists:keyfind(Name, #library.name, Libraries) of
    #library{hash = Hash, version = Version} ->
      UpdatedLibrary = #library{
        name = Name,
        code = Code,
        hash = Hash,
        version = Version
      },
      {lists:keyreplace(Name, #library.name, Libraries, UpdatedLibrary), VersionCounter};
    #library{} ->
      NewVersion = VersionCounter + 1,
      UpdatedLibrary = #library{
        name = Name,
        code = Code,
        hash = Hash,
        version = NewVersion
      },
      {lists:keyreplace(Name, #library.name, Libraries, UpdatedLibrary), NewVersion};
    false ->
      NewVersion = VersionCounter + 1,
      NewLibrary = #library{
        name = Name,
        code = Code,
        hash = Hash,
        version = NewVersion
      },
      % Maintain insert order of libraries
      {Libraries ++ [NewLibrary], NewVersion}
  end.

rollback_library([], _Name, _Libraries, _Deadline) ->
  [];
rollback_library(SuccessfulPIDs, Name, Libraries, Deadline) ->
  case lists:keyfind(Name, #library.name, Libraries) of
    #library{code = PreviousCode} ->
      {_, RollbackErrors} = broadcast_library(SuccessfulPIDs, Name, PreviousCode, Deadline),
      RollbackErrors;
    false ->
      {_, RollbackErrors} = broadcast_delete(SuccessfulPIDs, Name, Deadline),
      RollbackErrors
  end.

rollback_delete([], _Name, _Code, _Deadline) ->
  [];
rollback_delete(SuccessfulPIDs, Name, Code, Deadline) ->
  {_, RollbackErrors} = broadcast_library(SuccessfulPIDs, Name, Code, Deadline),
  RollbackErrors.

broadcast_library(WorkerPIDs, Name, Code, Deadline) ->
  broadcast(
    WorkerPIDs,
    fun(PID) ->
      try erl_py_runner_worker:load_library(PID, Name, Code)
      catch _Class:Error -> {error, {load_failed, Error}}
      end
    end,
    Deadline
  ).

broadcast_delete(WorkerPIDs, Name, Deadline) ->
  broadcast(
    WorkerPIDs,
    fun(PID) ->
      try erl_py_runner_worker:delete_library(PID, Name)
      catch _Class:Error -> {error, {delete_failed, Error}}
      end
    end,
    Deadline
  ).

broadcast(WorkerPIDs, RequestFun, Deadline) ->
  Owner = self(),
  PendingRefs =
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
  collect(PendingRefs, [], [], Deadline).

collect(PendingRefs, SuccessfulPIDs, Errors, _Deadline) when map_size(PendingRefs) =:= 0 ->
  {SuccessfulPIDs, Errors};
collect(PendingRefs, SuccessfulPIDs, Errors, Deadline) ->
  Remaining = erlang:max(0, Deadline - erlang:monotonic_time(millisecond)),
  receive
    {broadcast_reply, Ref, _PID, Result} ->
      case maps:take(Ref, PendingRefs) of
        {ExpectedPID, Rest} ->
          case Result of
            ok ->
              collect(Rest, [ExpectedPID | SuccessfulPIDs], Errors, Deadline);
            {error, Reason} ->
              collect(Rest, SuccessfulPIDs, [{ExpectedPID, Reason} | Errors], Deadline);
            _ ->
              collect(Rest, SuccessfulPIDs, [{ExpectedPID, invalid_reply} | Errors], Deadline)
          end;
        error ->
          collect(PendingRefs, SuccessfulPIDs, Errors, Deadline)
      end
  after
    Remaining ->
      TimeoutErrors = maps:fold(fun(_Ref, PID, Acc) -> [{PID, timeout} | Acc] end, Errors, PendingRefs),
      {SuccessfulPIDs, TimeoutErrors}
  end.
