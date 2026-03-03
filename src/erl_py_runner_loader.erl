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
  get_libraries_meta/0
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

get_libraries_meta() ->
  gen_server:call(?MODULE, ?CALL_GET_LIBRARIES_META, ?TIMEOUT_GET_LIBRARIES).

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
  #loader{libraries = Libraries, version_counter = VersionCounter} = State
) ->
  WorkerPIDs = erl_py_runner_pool:get_workers(),
  case do_delete_library(WorkerPIDs, Name, Libraries, VersionCounter) of
    {ok, NewLibraries, NewVersionCounter} ->
      {reply, ok, State#loader{
        libraries = NewLibraries,
        version_counter = NewVersionCounter
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

handle_call(
  ?CALL_GET_LIBRARIES_META,
  _Caller,
  #loader{
    libraries = Libraries
  } = State
) ->
  Result =
    [{Name, Code, Hash, Version} || #library{
      name = Name,
      code = Code,
      hash = Hash,
      version = Version
    } <- Libraries],
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
  {Library, ExpectedVersion, NewVersionCounter} =
    prepare_library(Name, Code, Libraries, VersionCounter),
  Deadline =
    erlang:monotonic_time(millisecond) + ?TIMEOUT_LOAD_LIBRARY,
  case broadcast_library(WorkerPIDs, Library, ExpectedVersion, Deadline) of
    {_, []} ->
      ?LOGINFO("successfully loaded library ~ts on ~p workers", [Name, length(WorkerPIDs)]),
      {ok, insert_library(Library, Libraries), NewVersionCounter};
    {SuccessfulPIDs, BroadcastErrors} ->
      ?LOGWARNING("failed to load library ~ts on ~p workers, errors: ~p, rolling back ~p workers",
                  [Name, length(WorkerPIDs), BroadcastErrors, length(SuccessfulPIDs)]),
      case rollback_library(SuccessfulPIDs, Library, Libraries, Deadline) of
        [] ->
          {error, BroadcastErrors};
        RollbackErrors ->
          {error, BroadcastErrors, RollbackErrors}
      end
  end.

do_delete_library(WorkerPIDs, Name, Libraries, VersionCounter) ->
  Deadline = erlang:monotonic_time(millisecond) + ?TIMEOUT_LOAD_LIBRARY,
  case lists:keyfind(Name, #library.name, Libraries) of
    false ->
      {error, not_found};
    #library{hash = Hash, version = CurrentVersion} = Library ->
      DeleteVersion = VersionCounter + 1,
      DeleteLibrary =
        #library{
          name = Name,
          hash = Hash,
          version = DeleteVersion
        },
      case broadcast_delete(WorkerPIDs, DeleteLibrary, CurrentVersion, Deadline) of
        {_, []} ->
          ?LOGINFO("successfully deleted library ~ts from ~p workers", [
            Name,
            length(WorkerPIDs)
          ]),
          {ok, lists:keydelete(Name, #library.name, Libraries), DeleteVersion};
        {SuccessfulPIDs, DeleteErrors} ->
          ?LOGWARNING("failed to delete library ~ts on ~p workers, errors: ~p, rolling back ~p workers", [
            Name,
            length(WorkerPIDs),
            DeleteErrors,
            length(SuccessfulPIDs)
          ]),
          case rollback_delete(SuccessfulPIDs, Library, DeleteVersion, Deadline) of
            [] ->
              {error, DeleteErrors};
            RollbackErrors ->
              {error, DeleteErrors, RollbackErrors}
          end
      end
  end.

prepare_library(Name, Code, Libraries, VersionCounter) ->
  Hash = crypto:hash(sha256, Code),
  case lists:keyfind(Name, #library.name, Libraries) of
    #library{hash = Hash, version = Version} ->
      UpdatedLibrary =
        #library{
          name = Name,
          code = Code,
          hash = Hash,
          version = Version
        },
      {UpdatedLibrary, Version, VersionCounter};
    #library{version = Version} ->
      NewVersion = VersionCounter + 1,
      UpdatedLibrary =
        #library{
          name = Name,
          code = Code,
          hash = Hash,
          version = NewVersion
        },
      {UpdatedLibrary, Version, NewVersion};
    false ->
      NewVersion = VersionCounter + 1,
      UpdatedLibrary =
        #library{
          name = Name,
          code = Code,
          hash = Hash,
          version = NewVersion
        },
      {UpdatedLibrary, _Initial = 0, NewVersion}
  end.

insert_library(#library{name = Name} = Library, Libraries) ->
  lists:keystore(Name, #library.name, Libraries, Library).

rollback_library([], _Library, _Libraries, _Deadline) ->
  [];
rollback_library(
  SuccessfulPIDs,
  #library{name = Name, hash = Hash, version = Version},
  Libraries,
  Deadline
) ->
  case lists:keyfind(Name, #library.name, Libraries) of
    #library{} = PreviousLibrary ->
      {_, RollbackErrors} = broadcast_library(SuccessfulPIDs, PreviousLibrary, Version, Deadline),
      RollbackErrors;
    false ->
      RollbackLibrary = #library{name = Name, hash = Hash, version = 0},
      {_, RollbackErrors} = broadcast_delete(SuccessfulPIDs, RollbackLibrary, Version, Deadline),
      RollbackErrors
  end.

rollback_delete([], _Library, _DeleteVersion, _Deadline) ->
  [];
rollback_delete(SuccessfulPIDs, #library{} = Library, DeleteVersion, Deadline) ->
  {_, RollbackErrors} = broadcast_library(SuccessfulPIDs, Library, DeleteVersion, Deadline),
  RollbackErrors.

broadcast_library(WorkerPIDs, #library{} = Library, ExpectedVersion, Deadline) ->
  broadcast(
    WorkerPIDs,
    fun(PID) ->
      try erl_py_runner_worker:load_library(PID, Library, ExpectedVersion)
      catch _Class:Error -> {error, {load_failed, Error}}
      end
    end,
    Deadline
  ).

broadcast_delete(WorkerPIDs, #library{} = Library, ExpectedVersion, Deadline) ->
  broadcast(
    WorkerPIDs,
    fun(PID) ->
      try erl_py_runner_worker:delete_library(PID, Library, ExpectedVersion)
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
