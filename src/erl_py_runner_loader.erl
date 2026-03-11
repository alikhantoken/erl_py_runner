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
    {error, BroadcastErrors} ->
      {reply, {error, BroadcastErrors}, State};
    {error, BroadcastErrors, RollbackErrors} ->
      {reply, {error, {broadcast_failed, BroadcastErrors, rollback_failed, RollbackErrors}}, State}
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
    {error, DeleteErrors} ->
      {reply, {error, DeleteErrors}, State};
    {error, DeleteErrors, RollbackErrors} ->
      {reply, {error, {broadcast_failed, DeleteErrors, rollback_failed, RollbackErrors}}, State}
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
  {Library, ExpectedVersion, NewVersionCounter} =
    prepare_library(Name, Code, VersionCounter),
  Deadline = ?DEADLINE(?TIMEOUT_LOAD_LIBRARY),
  case broadcast_library(WorkerPIDs, Library, ExpectedVersion, Deadline) of
    {_, []} ->
      ?LOGINFO(
        "successfully loaded library ~ts on ~p workers",
        [Name, length(WorkerPIDs)]
      ),
      ets:insert(?LIBRARIES_TAB, Library),
      {ok, NewVersionCounter};
    {SuccessfulPIDs, BroadcastErrors} ->
      ?LOGWARNING(
        "failed to load library ~ts on ~p workers, errors: ~p, rolling back ~p workers",
        [Name, length(WorkerPIDs), BroadcastErrors, length(SuccessfulPIDs)]
      ),
      case rollback_library(SuccessfulPIDs, Library, Deadline) of
        [] ->
          {error, BroadcastErrors};
        RollbackErrors ->
          {error, BroadcastErrors, RollbackErrors}
      end
  end.

do_delete_library(WorkerPIDs, Name, VersionCounter) ->
  case ets:lookup(?LIBRARIES_TAB, Name) of
    [] ->
      {error, not_found};
    [#library{hash = Hash, version = CurrentVersion} = Library] ->
      DeleteVersion = VersionCounter + 1,
      DeleteLibrary = #library{name = Name, hash = Hash, version = DeleteVersion},
      case broadcast_delete(WorkerPIDs, DeleteLibrary, CurrentVersion, ?DEADLINE(?TIMEOUT_LOAD_LIBRARY)) of
        {_, []} ->
          ?LOGINFO(
            "successfully deleted library ~ts from ~p workers",
            [Name, length(WorkerPIDs)]
          ),
          ets:delete(?LIBRARIES_TAB, Name),
          {ok, DeleteVersion};
        {SuccessfulPIDs, DeleteErrors} ->
          ?LOGWARNING(
            "failed to delete library ~ts, errors: ~p, rolling back ~p workers",
            [Name, DeleteErrors, length(SuccessfulPIDs)]
          ),
          case rollback_delete(SuccessfulPIDs, Library, DeleteVersion, ?DEADLINE(?TIMEOUT_LOAD_LIBRARY)) of
            [] ->
              {error, DeleteErrors};
            RollbackErrors ->
              {error, DeleteErrors, RollbackErrors}
          end
      end
  end.

prepare_library(Name, Code, VersionCounter) ->
  Hash = crypto:hash(sha256, Code),
  case ets:lookup(?LIBRARIES_TAB, Name) of
    [#library{hash = Hash, version = Version, insert_order = Order}] ->
      UpdatedLibrary =
        #library{
          name = Name,
          code = Code,
          hash = Hash,
          version = Version,
          insert_order = Order
        },
      {UpdatedLibrary, Version, VersionCounter};
    [#library{version = Version, insert_order = Order}] ->
      NewVersion = VersionCounter + 1,
      UpdatedLibrary =
        #library{
          name = Name,
          code = Code,
          hash = Hash,
          version = NewVersion,
          insert_order = Order
        },
      {UpdatedLibrary, Version, NewVersion};
    [] ->
      NewVersion = VersionCounter + 1,
      NewLibrary =
        #library{
          name = Name,
          code = Code,
          hash = Hash,
          version = NewVersion,
          insert_order = NewVersion
        },
      {NewLibrary, 0, NewVersion}
  end.

rollback_library([], _Library, _Deadline) ->
  [];
rollback_library(SuccessfulPIDs, #library{name = Name, hash = Hash, version = Version}, Deadline) ->
  case ets:lookup(?LIBRARIES_TAB, Name) of
    [#library{} = PreviousLibrary] ->
      {_, RollbackErrors} = broadcast_library(SuccessfulPIDs, PreviousLibrary, Version, Deadline),
      RollbackErrors;
    [] ->
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

sorted_libraries() ->
  lists:sort(
    fun(A, B) ->
      A#library.insert_order =< B#library.insert_order
    end,
    ets:tab2list(?LIBRARIES_TAB)
  ).