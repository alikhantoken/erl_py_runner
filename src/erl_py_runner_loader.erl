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
    library_count = 0
  }}.

handle_call(
  ?CALL_LOAD_LIBRARY(Name, Code),
  _Caller,
  #loader{libraries = Libraries} = State
) ->
  WorkerPIDs = erl_py_runner_pool:get_workers(),
  case broadcast_library(WorkerPIDs, Name, Code) of
    [] ->
      NewLibraries = add_library(Name, Code, Libraries),
      ?LOGINFO("successfully loaded library ~ts from ~p workers", [Name, length(WorkerPIDs)]),
      {reply, ok, State#loader{
        libraries = NewLibraries,
        library_count = length(NewLibraries)
      }};
    Errors ->
      ?LOGERROR("failed to load library ~ts on workers: ~p", [Name, Errors]),
      {reply, {error, Errors}, State}
  end;

handle_call(
  ?CALL_DELETE_LIBRARY(Name),
  _Caller,
  #loader{libraries = Libraries} = State
) ->
  case lists:keymember(Name, 1, Libraries) of
    false ->
      {reply, {error, not_found}, State};
    true ->
      WorkerPIDs = erl_py_runner_pool:get_workers(),
      case broadcast_delete(WorkerPIDs, Name) of
        [] ->
          NewLibraries = lists:keydelete(Name, 1, Libraries),
          ?LOGINFO("successfully deleted library ~ts from ~p workers", [Name, length(WorkerPIDs)]),
          {reply, ok, State#loader{
            libraries = NewLibraries,
            library_count = length(NewLibraries)
          }};
        Errors ->
          ?LOGERROR("failed to delete library ~ts on workers: ~p", [Name, Errors]),
          {reply, {error, Errors}, State}
      end
  end;

handle_call(
  ?CALL_GET_LIBRARIES,
  _Caller,
  #loader{
    libraries = Libraries
  } = State
) ->
  {reply, {ok, Libraries}, State};

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

broadcast_library(WorkerPIDs, Name, Code) ->
  Owner = self(),
  ExpectedRefs =
    [begin
       Reference = make_ref(),
       spawn(
         fun() ->
           Result =
             try erl_py_runner_worker:load_library(PID, Name, Code)
             catch _Class:Error -> {error, {load_failed, Error}}
             end,
           Owner ! {Reference, PID, Result}
         end),
       {Reference, PID}
     end || PID <- WorkerPIDs],
  collect(ExpectedRefs).

broadcast_delete(WorkerPIDs, Name) ->
  Parent = self(),
  ExpectedRefs =
    [begin
       Reference = make_ref(),
       spawn(
         fun() ->
           Result =
             try erl_py_runner_worker:delete_library(PID, Name)
             catch _Class:Error -> {error, {delete_failed, Error}}
             end,
           Parent ! {Reference, PID, Result}
         end),
       {Reference, PID}
     end || PID <- WorkerPIDs],
  collect(ExpectedRefs).

add_library(Name, Code, Libraries) ->
  case lists:keymember(Name, 1, Libraries) of
    true -> lists:keyreplace(Name, 1, Libraries, {Name, Code});
    false -> Libraries ++ [{Name, Code}]
  end.

collect(Refs) ->
  collect(Refs, []).
collect([], Errors) ->
  Errors;
collect([{Ref, PID} | Rest], Errors) ->
  receive
    {Ref, PID, ok} -> collect(Rest, Errors);
    {Ref, PID, {error, Reason}} -> collect(Rest, [{PID, Reason} | Errors])
  after
    ?TIMEOUT_LOAD_LIBRARY -> collect(Rest, [{PID, timeout} | Errors])
  end.
