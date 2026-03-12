%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner_worker).
-behaviour(gen_server).

-include("erl_py_runner.hrl").
-include("erl_py_runner_worker.hrl").

%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([
  start_child/2,
  start_link/1,
  run/2, run/3, run/4,
  load_library/3,
  delete_library/3,
  info/1
]).

%%% +--------------------------------------------------------------+
%%% |                     Gen Server Callbacks                     |
%%% +--------------------------------------------------------------+

-export([
  init/1,
  handle_call/3,
  handle_info/2,
  handle_cast/2,
  terminate/2
]).

%%% +--------------------------------------------------------------+
%%% |                          Public API                          |
%%% +--------------------------------------------------------------+

start_child(Number, Config) ->
  supervisor:start_child(erl_py_runner_worker_sup, #{
    id => worker_id(Number),
    start => {?MODULE, start_link, [Config]},
    restart => permanent,
    shutdown => 60000,
    type => worker
  }).

start_link(Config) ->
  gen_server:start_link(?MODULE, Config, []).

run(Code, Arguments) ->
  {ok, #{config := #{exec_timeout := Timeout}}} = ?ENV(worker),
  run(Code, Arguments, _State = undefined, Timeout).

run(Code, Arguments, State) ->
  {ok, #{config := #{exec_timeout := Timeout}}} = ?ENV(worker),
  run(Code, Arguments, State, Timeout).
  
run(Code, Arguments, State, Timeout) ->
  Deadline = ?DEADLINE(Timeout),
  case erl_py_runner_pool:get_worker(Timeout) of
    {ok, Worker} ->
      gen_server:call(Worker, ?CALL_RUN(Code, Arguments, State, Deadline), ?REMAINING_CALL(Deadline));
    {error, _} = Error ->
      Error
  end.

load_library(Worker, #library{} = Library, ExpectedVersion) ->
  gen_server:call(Worker, ?CALL_LOAD_LIBRARY(Library, ExpectedVersion), ?TIMEOUT_LOAD_LIBRARY_CALL).

delete_library(Worker, #library{} = Library, ExpectedVersion) ->
  gen_server:call(Worker, ?CALL_DELETE_LIBRARY(Library, ExpectedVersion), ?TIMEOUT_LOAD_LIBRARY_CALL).

info(Worker) ->
  gen_server:call(Worker, ?CALL_INFO, ?TIMEOUT_INFO).

%%% +--------------------------------------------------------------+
%%% |                     Gen Server Callbacks                     |
%%% +--------------------------------------------------------------+

init(#{
  runner := Runner,
  exec_timeout := ExecTimeout,
  operation_timeout := OperationTimeout,
  restart_delay := RestartDelay,
  python_modules := AllowedPythonModules,
  erlang_modules := AllowedErlangModules
}) ->
  ok = delay_restart(RestartDelay),
  Port =
    erlang:open_port(
      {spawn, Runner},
      [{packet, ?PACKET_SIZE}, binary, exit_status]
    ),
  OsPid =
    case erlang:port_info(Port, os_pid) of
      {os_pid, PID} -> PID;
      undefined -> undefined
    end,
  Data = #data{
    port = Port,
    os_pid = OsPid,
    exec_timeout = ExecTimeout,
    operation_timeout = OperationTimeout,
    allowed_modules =
      case AllowedErlangModules of
        Modules when is_list(Modules) -> maps:from_keys(Modules, true);
        all -> all;
        _Other -> #{}
      end
  },
  ok = handshake(Data, #{modules => AllowedPythonModules}),
  {ok, Libraries} = erl_py_runner_loader:get_libraries_meta(),
  case reload_libraries(Data, Libraries) of
    ok ->
      erl_py_runner_pool:send_worker_start(self()),
      {ok, Data};
    {error, Reason} ->
      ?LOGERROR("failed to init worker due to library reload fail: ~p", [Reason]),
      {stop, {library_init_failed, Reason}}
  end.

handle_call(
  ?CALL_RUN(Code, Arguments, State, Deadline),
  _Caller,
  #data{port = Port} = Data
) ->
  case ?IS_EXPIRED(Deadline) of
    true ->
      erl_py_runner_pool:send_worker_ready(self()),
      {reply, {error, timeout}, Data};
    false ->
      send_port_command(Port, ?COMMAND_EXECUTE(Code, Arguments, State)),
      case wait_port_response(Data, Deadline) of
        {stop, port_timeout} ->
          {stop, port_timeout, {error, timeout}, Data};
        {stop, Reason} ->
          {stop, Reason, {error, Reason}, Data};
        Result ->
          erl_py_runner_pool:send_worker_ready(self()),
          {reply, Result, Data}
      end
  end;

handle_call(
  ?CALL_LOAD_LIBRARY(
    #library{name = Name, code = Code, hash = Hash, version = Version},
    ExpectedVersion
  ),
  _Caller,
  #data{port = Port, operation_timeout = Timeout} = Data
) ->
  send_port_command(Port, ?COMMAND_LOAD_LIBRARY(Name, Code, Hash, ExpectedVersion, Version)),
  case wait_port_response(Data, ?DEADLINE(Timeout)) of
    {stop, port_timeout} ->
      {stop, port_timeout, {error, timeout}, Data};
    {stop, Reason} ->
      {stop, Reason, {error, Reason}, Data};
    Result ->
      {reply, Result, Data}
  end;

handle_call(
  ?CALL_DELETE_LIBRARY(
    #library{name = Name, hash = Hash, version = Version},
    ExpectedVersion
  ),
  _Caller,
  #data{port = Port, operation_timeout = Timeout} = Data
) ->
  send_port_command(Port, ?COMMAND_DELETE_LIBRARY(Name, Hash, ExpectedVersion, Version)),
  case wait_port_response(Data, ?DEADLINE(Timeout)) of
    {stop, port_timeout} ->
      {stop, port_timeout, {error, timeout}, Data};
    {stop, Reason} ->
      {stop, Reason, {error, Reason}, Data};
    Result ->
      {reply, Result, Data}
  end;

handle_call(
  ?CALL_INFO,
  _Caller,
  #data{port = Port} = Data
) ->
  {reply, collect_port_info(Port), Data}.

handle_info(
  {Port, {exit_status, StatusCode}},
  #data{port = Port} = Data
) ->
  ?LOGERROR("received exit from port: ~p, status code: ~p", [Port, StatusCode]),
  {stop, {port_exit, StatusCode}, Data};

handle_info(Unexpected, Data) ->
  ?LOGWARNING("received unexpected message: ~p, ignored", [Unexpected]),
  {noreply, Data}.
  
handle_cast(Unexpected, Data) ->
  ?LOGWARNING("received unexpected message: ~p", [Unexpected]),
  {noreply, Data}.

terminate(_Reason, #data{port = Port, os_pid = OsPid}) ->
  catch erlang:port_close(Port),
  kill_os_process(OsPid),
  ok.

%%% +--------------------------------------------------------------+
%%% |                       Internal Functions                     |
%%% +--------------------------------------------------------------+

worker_id(Number) ->
  erlang:list_to_atom(?WORKER_NAME ++ erlang:integer_to_list(Number)).

handshake(
  #data{
    port = Port,
    operation_timeout = Timeout
  } = Data,
  Options
) ->
  send_port_command(Port, ?COMMAND_INIT(Options)),
  case wait_port_response(Data, ?DEADLINE(Timeout)) of
    ok -> ok;
    {ok, Response} -> exit({init_unexpected, Response});
    {error, Error} -> exit({init_failed, Error});
    {stop, Reason} -> exit(Reason)
  end.

wait_port_response(
  #data{port = Port} = Data,
  Deadline
) ->
  Remaining = ?REMAINING(Deadline),
  receive
    {Port, {data, Binary}} ->
      Term =
        try erlang:binary_to_term(Binary, [safe])
        catch _:_ -> {error, invalid_term_received}
      end,
      case Term of
        ok ->
          ok;
        {ok, {Result, State}} ->
          {ok, Result, State};
        {log, Level, Message} ->
          handle_log(Level, Message),
          wait_port_response(Data, Deadline);
        {call, RequestID, Module, Function, Arguments} ->
          handle_callback(RequestID, Module, Function, Arguments, Data),
          wait_port_response(Data, Deadline);
        {error, Error} ->
          {error, Error}
      end;
    {Port, {exit_status, StatusCode}} ->
      ?LOGERROR("port exited during execution: ~p, status code: ~p", [Port, StatusCode]),
      {stop, {port_exit, {status_code, StatusCode}}}
  after
    Remaining ->
      ?LOGERROR("port deadline exceeded, closing port: ~p", [Port]),
      catch erlang:port_close(Port),
      {stop, port_timeout}
  end.

handle_callback(
  RequestID,
  erl_py_runner_loader,
  get_library_meta,
  [Name],
  #data{port = Port}
) ->
  send_port_command(Port, ?COMMAND_REPLY(RequestID, erl_py_runner_loader:get_library_meta(Name)));

handle_callback(
  RequestID,
  Module,
  Function,
  Arguments,
  #data{port = Port, allowed_modules = AllowedModules}
) ->
  Response =
    try
      is_module_allowed(Module, AllowedModules),
      is_function_exported(Module, Function, Arguments),
      ?COMMAND_REPLY(RequestID, {ok, erlang:apply(Module, Function, Arguments)})
    catch
      throw:module_not_allowed ->
        ?COMMAND_REPLY(RequestID, {error, <<"module is not allowed">>});
      throw:function_not_found ->
        ?COMMAND_REPLY(RequestID, {error, <<"function is not found">>});
      throw:function_arguments_not_list ->
        ?COMMAND_REPLY(RequestID, {error, <<"function arguments is not list type">>});
      _Class:Error ->
        ?COMMAND_REPLY(RequestID, {error, iolist_to_binary(io_lib:format("~p", [Error]))})
    end,
  send_port_command(Port, Response).

handle_log(Level, Message) ->
  try ?LOG(Level, Message) catch _:_ -> ok end.

send_port_command(Port, Term) ->
  try
    erlang:port_command(Port, erlang:term_to_binary(Term))
  catch _Class:Error ->
    ?LOGWARNING("port command failed, closing port: ~p", [Error]),
    exit(port_command_aborted)
  end.

is_module_allowed(_Module, all) ->
  ok;
is_module_allowed(Module, AllowedModules) ->
  case AllowedModules of
    #{Module := true} -> ok;
    _ -> throw(module_not_allowed)
  end.

is_function_exported(Module, Function, Arguments) when is_list(Arguments) ->
  case code:ensure_loaded(Module) of
    {module, _} -> ok;
    _ -> throw(module_not_found)
  end,
  case erlang:function_exported(Module, Function, length(Arguments)) of
    true -> ok;
    false -> throw(function_not_found)
  end;
is_function_exported(_Module, _Function, _Arguments) ->
  throw(function_arguments_not_list).

collect_port_info(Port) ->
  lists:foldl(
    fun(Key, Acc) ->
      case erlang:port_info(Port, Key) of
        {Key, Value} -> Acc#{Key => Value};
        undefined    -> Acc
      end
    end,
    #{},
    ?PORT_INFO_KEYS
  ).

reload_libraries(#data{operation_timeout = Timeout} = Data, Libraries) ->
  reload_libraries(Data, Libraries, ?DEADLINE(Timeout)).
reload_libraries(_Data, [], _Deadline) ->
  ok;
reload_libraries(
  #data{port = Port} = Data,
  [#library{name = Name, code = Code, hash = Hash, version = Version} | Rest],
  Deadline
) ->
  send_port_command(Port, ?COMMAND_LOAD_LIBRARY(Name, Code, Hash, 0, Version)),
  case wait_port_response(Data, Deadline) of
    ok ->
      reload_libraries(Data, Rest, Deadline);
    {error, Error} ->
      {error, Error};
    {stop, Reason} ->
      {error, Reason}
  end.

kill_os_process(undefined) ->
  ok;
kill_os_process(OsPid) ->
  spawn(
    fun() ->
      os:cmd("kill -15 " ++ erlang:integer_to_list(OsPid))
    end
  ),
  ok.

delay_restart(0) ->
  ok;
delay_restart(Delay) ->
  timer:sleep(Delay),
  ok.