%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-ifndef(ERL_PY_RUNNER_WORKER).
-define(ERL_PY_RUNNER_WORKER, 1).

-record(data, {
  timeout,
  port,
  allowed_modules = all
}).

%% Worker name in the supervision tree.
-define(WORKER_NAME, "erl_py_runner_worker_").

%% Available commands that are used to communicate with python.
-define(COMMAND_EXECUTE(Code, Arguments, State), {exec, Code, Arguments, State}).
-define(COMMAND_INIT(Modules), {init, Modules}).
-define(COMMAND_REPLY(RequestID, Reply), {call_reply, RequestID, Reply}).
-define(COMMAND_LOAD_LIBRARY(Name, Code), {load_library, Name, Code}).
-define(COMMAND_DELETE_LIBRARY(Name), {delete_library, Name}).

%% Gen server call requests.
-define(CALL_RUN(Code, Arguments, State), {run, Code, Arguments, State}).
-define(CALL_LOAD_LIBRARY(Name, Code), {load_library, Name, Code}).
-define(CALL_DELETE_LIBRARY(Name), {delete_library, Name}).
-define(CALL_INFO, info).

%% Timeouts for each API public function.
-define(TIMEOUT_RUN, 60000).
-define(TIMEOUT_LOAD_LIBRARY, 180000).
-define(TIMEOUT_INFO, 30000).

-define(PACKET_SIZE, 4).
-define(MONOTONIC_MS, erlang:monotonic_time(millisecond)).
-define(PORT_INFO_KEYS, [connected, id, input, output, memory, os_pid, queue_size]).

-endif.
