%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-ifndef(ERL_PY_RUNNER_POOL).
-define(ERL_PY_RUNNER_POOL, 1).

-define(GET_WORKER,        get_worker).
-define(WORKER_START(PID), {worker_start, PID}).
-define(WORKER_READY(PID), {worker_ready, PID}).

-define(IDLE_WORKERS_TAB, erl_py_runner_pool_idle).

-record(pool, {
  max_pending,
  pending,
  pending_size,
  worker_monitors,
  caller_monitors
}).

%% Gen server call requests.
-define(CALL_GET_WORKERS, get_workers).

-define(TIMEOUT_GET_WORKERS, 5000).

-endif.