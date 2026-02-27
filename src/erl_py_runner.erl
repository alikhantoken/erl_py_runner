%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner).

%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([
  run/2, run/3, run/4,
  load_library/2,
  delete_library/1,
  restart/0,
  info/0
]).

%%% +--------------------------------------------------------------+
%%% |                         Implementation                       |
%%% +--------------------------------------------------------------+

-spec run(binary(), term()) -> {ok, term(), term()} | {error, term()}.
run(Code, Arguments) ->
  erl_py_runner_worker:run(Code, Arguments).

-spec run(binary(), term(), term()) -> {ok, term(), term()} | {error, term()}.
run(Code, Arguments, State) ->
  erl_py_runner_worker:run(Code, Arguments, State).
  
-spec run(binary(), term(), term(), timeout()) -> {ok, term(), term()} | {error, term()}.
run(Code, Arguments, State, Timeout) ->
  erl_py_runner_worker:run(Code, Arguments, State, Timeout).

-spec load_library(binary(), binary()) -> ok | {error, term()}.
load_library(Name, Code) ->
  erl_py_runner_loader:load_library(Name, Code).

-spec delete_library(binary()) -> ok | {error, term()}.
delete_library(Name) ->
  erl_py_runner_loader:delete_library(Name).

-spec restart() -> ok | {error, term()}.
restart() ->
  erl_py_runner_app:restart().
  
-spec info() -> list(map()).
info() ->
  erl_py_runner_worker_sup:info().