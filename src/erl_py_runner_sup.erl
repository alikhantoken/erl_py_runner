%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner_sup).
-behaviour(supervisor).
-include("erl_py_runner.hrl").


%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([
  start_link/0,
  init/1
]).

%%% +--------------------------------------------------------------+
%%% |                         Implementation                       |
%%% +--------------------------------------------------------------+

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).
  
init([]) ->
  {ok, #{
    intensity := Intensity,
    period := Period,
    worker_shutdown := Shutdown
  }} = ?ENV(supervisor),

  {ok, #{
    environment := #{
      python := Python,
      runner := Runner,
      venv_dir := VenvDirectory
    },
    config := #{
      pool_size := PoolSize,
      timeout := Timeout
    },
    modules_whitelist := #{
      erlang_modules := ErlangModules,
      python_modules := PythonModules
    }
  }} = ?ENV(worker),
  
  Supervisor = #{
    strategy => one_for_one,
    intensity => Intensity,
    period => Period
  },
  
  WorkerConfig = #{
    runner => ?PYTHON_PATH(VenvDirectory, Python) ++ " -u " ++ Runner,
    timeout => Timeout,
    python_modules => PythonModules,
    erlang_modules => ErlangModules
  },
  
  Children =
    [begin
       #{
         id => get_id(Index),
         start => {erl_py_runner_worker, start, [WorkerConfig]},
         restart => permanent,
         shutdown => Shutdown,
         type => worker
       }
     end || Index <- lists:seq(1, PoolSize)],
  
  {ok, {Supervisor, Children}}.

%%% +--------------------------------------------------------------+
%%% |                       Internal functions                     |
%%% +--------------------------------------------------------------+
  
get_id(Index) ->
  erlang:list_to_atom(?WORKER_NAME ++ erlang:integer_to_list(Index)).