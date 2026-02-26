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
  Environment = resolve_environment(maps:get(environment, Verified)),
  ok = ensure(Environment),
  Verified#{
    environment => Environment
  }.
  
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
        timeout => 60000,
        pool_size => 3,
        max_pending => infinity
      },
      modules_whitelist => #{
        erlang_modules => [
          math,
          lists,
          maps,
          binary,
          string
        ],
        python_modules => [
          <<"math">>,
          <<"re">>,
          <<"datetime">>,
          <<"json">>
        ]
      }
    }
  }.
  
ensure(#{
  python := system
}) ->
  ok;
ensure(#{
  python := environment,
  venv_dir := VenvDirectory,
  requirements := Requirements
}) ->
  BashScript = filename:join(code:priv_dir(?APP_NAME), "install.sh"),
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