%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-ifndef(ERL_PY_RUNNER_WORKER).
-define(ERL_PY_RUNNER_WORKER, 1).

-record(state, {
  timeout,
  port,
  allowed_modules = all
}).

-define(COMMAND_EXECUTE(Code, Arguments), {exec, Code, Arguments}).
-define(COMMAND_INIT(Modules), {init, Modules}).
-define(COMMAND_REPLY(RequestID, Reply), {call_reply, RequestID, Reply}).

-define(WORKER_NAME, "erl_py_runner_worker_").

-endif.
