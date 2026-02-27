%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-ifndef(ERL_PY_RUNNER).
-define(ERL_PY_RUNNER, 1).

-define(APP_NAME, erl_py_runner).

-define(WORKER_DEFAULT_POOL_SHUTDOWN, 10000).
-define(WORKER_DEFAULT_LOADER_SHUTDOWN, 10000).
-define(WORKER_DEFAULT_SUP_INTENSITY, 5).
-define(WORKER_DEFAULT_SUP_PERIOD, 60).
-define(SUP_DEFAULT_INTENSITY, 10).
-define(SUP_DEFAULT_PERIOD, 60).

-define(MFA_METADATA, #{
  mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
  line => ?LINE
}).

-define(LOGERROR(Text),          logger:error(Text, [], ?MFA_METADATA)).
-define(LOGERROR(Text,Params),   logger:error(Text, Params, ?MFA_METADATA)).
-define(LOGWARNING(Text),        logger:warning(Text, [], ?MFA_METADATA)).
-define(LOGWARNING(Text,Params), logger:warning(Text, Params, ?MFA_METADATA)).
-define(LOGINFO(Text),           logger:info(Text, [], ?MFA_METADATA)).
-define(LOGINFO(Text,Params),    logger:info(Text, Params, ?MFA_METADATA)).
-define(LOGDEBUG(Text),          logger:debug(Text, [], ?MFA_METADATA)).
-define(LOGDEBUG(Text,Params),   logger:debug(Text, Params, ?MFA_METADATA)).

-define(ENV(Key@, Default@), application:get_env(erl_py_runner, Key@, Default@)).
-define(ENV(Key@), application:get_env(erl_py_runner, Key@)).

-define(PIP_PATH(VenvDirectory@), filename:join([VenvDirectory@, "bin", "pip"])).
-define(PYTHON_PATH(VenvDirectory@), filename:join([VenvDirectory@, "bin", "python3"])).

-endif.
