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
    supervisor := #{
      intensity := Intensity,
      period := Period
    },
    config := #{
      max_pending := MaxPending,
      pool_size := PoolSize,
      timeout := Timeout
    },
    modules_whitelist := #{
      erlang_modules := ErlangModules,
      python_modules := PythonModules
    }
  }} = ?ENV(worker),
  
  {ok, #{
    runner := Runner,
    python := Python,
    venv_dir := VenvDirectory
  }} = ?ENV(environment),
  
  Supervisor = #{
    strategy => one_for_all,
    intensity => ?SUP_DEFAULT_INTENSITY,
    period => ?SUP_DEFAULT_PERIOD
  },
  
  PythonCMD =
    case Python of
      system -> "python3 -u ";
      environment -> ?PYTHON_PATH(VenvDirectory) ++ " -u "
    end,
    
  WorkerConfig = #{
    runner => PythonCMD ++ Runner,
    timeout => Timeout,
    python_modules => PythonModules,
    erlang_modules => ErlangModules
  },
  
  %% NOTE!
  %% Loader must start before pool!
  %% Pool starts workers and all workers call loader during
  %% initialization to load the libraries list before joining the pool.
  Children = [
    #{
      id => erl_py_runner_worker_sup,
      start => {erl_py_runner_worker_sup, start_link, [Intensity * PoolSize, Period]},
      restart => permanent,
      shutdown => infinity,
      type => supervisor
    },
    #{
      id => erl_py_runner_loader,
      start => {erl_py_runner_loader, start_link, []},
      restart => permanent,
      shutdown => ?WORKER_DEFAULT_LOADER_SHUTDOWN,
      type => worker
    },
    #{
      id => erl_py_runner_pool,
      start => {erl_py_runner_pool, start_link, [PoolSize, MaxPending, WorkerConfig]},
      restart => permanent,
      shutdown => ?WORKER_DEFAULT_POOL_SHUTDOWN,
      type => worker
    }
  ],
  
  {ok, {Supervisor, Children}}.
