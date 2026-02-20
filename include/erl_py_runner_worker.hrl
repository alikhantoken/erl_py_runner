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

-define(CALL_REQUEST, call_request).
-define(CALL_RESPONSE, call_response).

-define(CALL_STATUS_OK, ok).
-define(CALL_STATUS_ERROR, error).

-define(WORKER_NAME, "erl_py_runner_worker_").

-endif.
