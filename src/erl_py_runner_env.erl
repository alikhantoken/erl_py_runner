%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner_env).
-include("erl_py_runner.hrl").

%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([ensure/1]).

%%% +--------------------------------------------------------------+
%%% |                         Implementation                       |
%%% +--------------------------------------------------------------+

ensure(#{
  python := Python,
  pyterm := Pyterm,
  venv_dir := VenvDirectory,
  requirements := Requirements
}) ->
  case ensure_venv(VenvDirectory, Python) of
    ok ->
      install_deps(VenvDirectory, Requirements, Pyterm);
    {error, Error} ->
      {error, Error}
  end.

%%% +--------------------------------------------------------------+
%%% |                       Internal functions                     |
%%% +--------------------------------------------------------------+

ensure_venv(VenvDirectory, Python) ->
  case filelib:is_dir(VenvDirectory) of
    true ->
      ok;
    false ->
      ?LOGINFO("creating venv: ~ts, python: ~p", [VenvDirectory, Python]),
      case run_command(Python ++ " -m venv " ++ VenvDirectory) of
        ok ->
          ok;
        {error, Error} ->
          ?LOGERROR("failed to create python venv, error: ~p", [Error]),
          {error, {failed_create_venv, Error}}
      end
  end.

install_deps(VenvDirectory, Requirements, Pyterm) ->
  case run_command(?PIP_PATH(VenvDirectory) ++ " install " ++ Pyterm) of
    ok ->
      case filelib:is_file(Requirements) of
        false ->
          ?LOGWARNING("requirements file ~s not found, skipping", [Requirements]),
          ok;
        true ->
          ?LOGINFO("installing python requirements from: ~s", [Requirements]),
          case run_command(?PIP_PATH(VenvDirectory) ++ " install -r " ++ Requirements) of
            ok ->
              ok;
            {error, Error} ->
              ?LOGERROR("failed to install python requirements: ~p", [Error]),
              {error, {pip_deps_install_failed, Error}}
          end
      end;
    {error, Reason} ->
      ?LOGERROR("failed to install pyrlang-term: ~p", [Reason]),
      {error, {pip_install_failed, Reason}}
  end.

run_command(Command) ->
  Port =
    open_port(
      {spawn, Command},
      [exit_status, stderr_to_stdout, binary, {line, 1024}]
    ),
  collect_port_output(Port).

collect_port_output(Port) ->
  receive
    {Port, {data, {eol, Line}}} ->
      ?LOGINFO("~ts", [Line]),
      collect_port_output(Port);
    {Port, {data, {noeol, Line}}} ->
      ?LOGINFO("~ts", [Line]),
      collect_port_output(Port);
    {Port, {exit_status, 0}} ->
      ok;
    {Port, {exit_status, Code}} ->
      {error, {exit_code, Code}}
  end.
