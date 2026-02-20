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
    period := Period
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
    strategy => one_for_all,
    intensity => Intensity,
    period => Period
  },
  
  WorkerConfig = #{
    runner => ?PYTHON_PATH(VenvDirectory, Python) ++ " -u " ++ Runner,
    timeout => Timeout,
    python_modules => PythonModules,
    erlang_modules => ErlangModules
  },
  
  Children = [
    #{
      id => erl_py_runner_worker_sup,
      start => {erl_py_runner_worker_sup, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => supervisor
    },
    #{
      id => erl_py_runner_pool,
      start => {erl_py_runner_pool, start_link, [PoolSize, WorkerConfig]},
      restart => permanent,
      shutdown => 5000,
      type => worker
    }
  ],
  
  {ok, {Supervisor, Children}}.
