%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-ifndef(ERL_PY_RUNNER_WORKER).
-define(ERL_PY_RUNNER_WORKER, 1).

%% Worker name in the supervision tree.
-define(WORKER_NAME, "erl_py_runner_worker_").

%% Available commands that are used to communicate with python.
-define(COMMAND_EXECUTE(Code, Arguments, State), {exec, Code, Arguments, State}).
-define(COMMAND_INIT(Options),                   {init, Options}).
-define(COMMAND_REPLY(RequestID, Reply),         {call_reply, RequestID, Reply}).
-define(COMMAND_LOAD_LIBRARY(Name, Code, Hash, ExpectedVersion, Version), {load_library, Name, Code, Hash, ExpectedVersion, Version}).
-define(COMMAND_DELETE_LIBRARY(Name, Hash, ExpectedVersion, Version),     {delete_library, Name, Hash, ExpectedVersion, Version}).

%% Gen server call requests.
-define(CALL_RUN(Code, Arguments, State, Deadline),    {run, Code, Arguments, State, Deadline}).
-define(CALL_LOAD_LIBRARY(Library, ExpectedVersion),   {load_library, Library, ExpectedVersion}).
-define(CALL_DELETE_LIBRARY(Library, ExpectedVersion), {delete_library, Library, ExpectedVersion}).
-define(CALL_INFO, info).

-define(PORT_INFO_KEYS, [connected, id, input, output, memory, os_pid, queue_size]).
-define(TIMEOUT_LOAD_LIBRARY_CALL, 180000).
-define(PACKET_SIZE, 4).

-record(data, {
  exec_timeout,
  operation_timeout,
  port,
  os_pid,
  allowed_modules = all
}).

-endif.
