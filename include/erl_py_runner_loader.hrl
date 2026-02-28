%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-ifndef(ERL_PY_RUNNER_LOADER).
-define(ERL_PY_RUNNER_LOADER, 1).

-record(loader, {
  libraries
}).

%% Timeout for broadcasting a library to all existing workers.
-define(TIMEOUT_LOAD_LIBRARY, 180000).
-define(TIMEOUT_GET_LIBRARIES, 5000).

%% Gen server call requests.
-define(CALL_LOAD_LIBRARY(Name, Code), {load_library, Name, Code}).
-define(CALL_DELETE_LIBRARY(Name),     {delete_library, Name}).
-define(CALL_GET_LIBRARIES,            get_libraries).

-endif.
