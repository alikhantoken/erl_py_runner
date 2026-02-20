%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-ifndef(ERL_PY_RUNNER_POOL).
-define(ERL_PY_RUNNER_POOL, 1).

-define(GET_WORKER, get_worker).
-define(WORKER_START, worker_started).
-define(WORKER_READY, worker_ready).

-record(pool, {
  idle,
  pending
}).

-endif.
