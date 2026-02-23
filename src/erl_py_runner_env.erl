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
  venv_dir := VenvDirectory,
  requirements := Requirements
}) ->
  Script = filename:join(code:priv_dir(?APP_NAME), "install.sh"),
  run_command(Script, [VenvDirectory, Requirements]).
  
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