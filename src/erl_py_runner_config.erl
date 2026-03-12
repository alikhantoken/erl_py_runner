%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner_config).
-include("erl_py_runner.hrl").

%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([verify/1]).

%%% +--------------------------------------------------------------+
%%% |                         Implementation                       |
%%% +--------------------------------------------------------------+

verify(Config) when is_list(Config) ->
  verify(maps:from_list(Config));
verify(Config) ->
  Verified =
    maps:fold(
      fun(ConfigGroup, DefaultConfig, Acc) ->
        UserConfig = maps:get(ConfigGroup, Config, #{}),
        MergeConfig = deep_merge(DefaultConfig, UserConfig),
        ok = application:set_env(?APP_NAME, ConfigGroup, MergeConfig),
        maps:merge(Acc, #{ConfigGroup => MergeConfig})
      end,
      #{},
      default()
    ),
  #{
    worker := #{
      supervisor := SupConfig,
      config := WorkerConfig
    }
  } = Verified,
  lists:foreach(fun validate_option/1, maps:to_list(SupConfig)),
  lists:foreach(fun validate_option/1, maps:to_list(WorkerConfig)),
  Environment = resolve_environment(maps:get(environment, Verified)),
  ok = ensure(Environment),
  Verified#{environment => Environment}.
  
%%% +--------------------------------------------------------------+
%%% |                       Internal functions                     |
%%% +--------------------------------------------------------------+

default() ->
  #{
    environment => #{
      python => system,
      requirements => "python/requirements.txt",
      runner => "python/runner.py",
      venv_dir => ".venv"
    },
    worker => #{
      supervisor => #{
        intensity => 5,
        period => 30
      },
      config => #{
        pool_size => 3,
        max_pending => infinity,
        exec_timeout => 60000,       % Milliseconds
        operation_timeout => 30000,  % Milliseconds
        restart_delay => 0,          % Delay between worker restart
        max_code_size => 1048576     % 1 megabyte
      },
      modules_whitelist => #{
        erlang_modules => [
          math,
          lists,
          maps,
          binary,
          string
        ],
        python_modules => all
      }
    }
  }.
  
ensure(#{
  python := system
}) ->
  BashScript = filename:join(code:priv_dir(?APP_NAME), "dependency/system_install.sh"),
  ok = run_command(BashScript, ["python3"]);
ensure(#{
  python := environment,
  venv_dir := VenvDirectory,
  requirements := Requirements
}) ->
  BashScript = filename:join(code:priv_dir(?APP_NAME), "dependency/venv_install.sh"),
  ok = run_command(BashScript, [VenvDirectory, Requirements]).
  
%%% +--------------------------------------------------------------+
%%% |                       Internal functions                     |
%%% +--------------------------------------------------------------+

run_command(Script, Args) ->
  Port =
    open_port(
      {spawn_executable, Script},
      [
        {args, Args},
        exit_status,
        stderr_to_stdout,
        binary,
        {line, 1024}
      ]
    ),
  collect_port_output(Port).

collect_port_output(Port) ->
  collect_port_output(Port, <<>>).
collect_port_output(Port, Acc) ->
  receive
    {Port, {data, {noeol, Line}}} ->
      collect_port_output(Port, <<Acc/binary, Line/binary>>);
    {Port, {data, {eol, Line}}} ->
      ?LOGINFO("~ts", [<<Acc/binary, Line/binary>>]),
      collect_port_output(Port, <<>>);
    {Port, {exit_status, 0}} ->
      ok;
    {Port, {exit_status, Code}} ->
      {error, {exit_code, Code}}
  end.
  
deep_merge(Default, Override) when is_map(Default), is_map(Override) ->
  maps:fold(
    fun(Key, OverrideValue, Acc) ->
      case Acc of
        #{Key := DefaultValue} when is_map(DefaultValue), is_map(OverrideValue) ->
          Acc#{Key => deep_merge(DefaultValue, OverrideValue)};
        _ ->
          Acc#{Key => OverrideValue}
      end
    end,
    Default,
    Override
  );
deep_merge(_Default, Override) ->
  Override.

resolve_environment(#{
  python := system,
  runner := Runner
} = Environment) ->
  Environment#{
    runner => resolve_path(code:priv_dir(?APP_NAME), Runner),
    requirements => undefined,
    venv_dir => undefined
  };
resolve_environment(#{
  python := environment,
  runner := Runner,
  requirements := Requirements,
  venv_dir := VenvDir
} = Environment) ->
  Environment#{
    runner => resolve_path(code:priv_dir(?APP_NAME), Runner),
    requirements => resolve_path(code:priv_dir(?APP_NAME), Requirements),
    venv_dir => resolve_path(code:priv_dir(?APP_NAME), VenvDir)
  }.

resolve_path(RootPath, Path) ->
  case filename:pathtype(Path) of
    absolute -> Path;
    relative -> filename:join(RootPath, Path);
    _Other -> Path
  end.

validate_option({pool_size, Value}) when is_integer(Value), Value > 0 -> ok;
validate_option({pool_size, Value}) ->
  throw({invalid_config, pool_size, Value});

validate_option({max_code_size, Value}) when is_integer(Value), Value > 0 -> ok;
validate_option({max_code_size, Value}) ->
  throw({invalid_config, max_code_size, Value});

validate_option({intensity, Value}) when is_integer(Value), Value > 0 -> ok;
validate_option({intensity, Value}) ->
  throw({invalid_config, intensity, Value});

validate_option({period, Value}) when is_integer(Value), Value > 0 -> ok;
validate_option({period, Value}) ->
  throw({invalid_config, period, Value});

validate_option({exec_timeout, Value}) when is_integer(Value), Value > 0 -> ok;
validate_option({exec_timeout, Value}) ->
  throw({invalid_config, exec_timeout, Value});

validate_option({operation_timeout, Value}) when is_integer(Value), Value > 0 -> ok;
validate_option({operation_timeout, Value}) ->
  throw({invalid_config, operation_timeout, Value});

validate_option({max_pending, infinity}) -> ok;
validate_option({max_pending, Value}) when is_integer(Value), Value >= 0 -> ok;
validate_option({max_pending, Value}) ->
  throw({invalid_config, max_pending, Value});

validate_option({restart_delay, Value}) when is_integer(Value), Value >= 0 -> ok;
validate_option({restart_delay, Value}) ->
  throw({invalid_config, restart_delay, Value});

validate_option({_Key, _Value}) -> ok.