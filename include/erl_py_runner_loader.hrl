%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-ifndef(ERL_PY_RUNNER_LOADER).
-define(ERL_PY_RUNNER_LOADER, 1).

-define(LIBRARIES_TAB, erl_py_runner_libraries).

%% Timeout for broadcasting a library to all existing workers.
-define(TIMEOUT_LOAD_LIBRARY, 180000).

%% Gen server call requests.
-define(CALL_LOAD_LIBRARY(Name, Code), {load_library, Name, Code}).
-define(CALL_DELETE_LIBRARY(Name),     {delete_library, Name}).

-record(loader, {
  version_counter
}).

-endif.
