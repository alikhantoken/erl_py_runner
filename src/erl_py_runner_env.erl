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
      ?LOGINFO("venv already exists, path: ~p", [VenvDirectory]),
      ok;
    false ->
      ?LOGINFO("creating venv: ~ts, python: ~p", [VenvDirectory, Python]),
      run_command(Python ++ " -m venv " ++ VenvDirectory)
  end.

install_deps(VenvDirectory, Requirements, Pyterm) ->
  run_command(?PIP_PATH(VenvDirectory) ++ " install --upgrade pip setuptools wheel"),
  run_command(?PIP_PATH(VenvDirectory) ++ " install " ++ Pyterm),
  case filelib:is_file(Requirements) of
    false ->
      ?LOGWARNING("requirements file ~s not found, skipping", [Requirements]);
    true ->
      ?LOGINFO("installing python requirements from: ~s", [Requirements]),
      run_command(?PIP_PATH(VenvDirectory) ++ " install -r " ++ Requirements)
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
