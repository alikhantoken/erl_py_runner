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

%% Available commands that are used to communicate with python.
-define(COMMAND_EXECUTE(Code, Arguments), {exec, Code, Arguments}).
-define(COMMAND_INIT(Modules), {init, Modules}).
-define(COMMAND_REPLY(RequestID, Reply), {call_reply, RequestID, Reply}).

%% Gen server call requests.
-define(CALL_RUN(Code, Arguments), {run, Code, Arguments}).
-define(CALL_INFO, info).

%% Worker name in the supervision tree.
-define(WORKER_NAME, "erl_py_runner_worker_").

-endif.
