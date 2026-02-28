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
  delete_library/2,
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
  run(Code, Arguments, _State = undefined, ?TIMEOUT_RUN).

run(Code, Arguments, State) ->
  run(Code, Arguments, State, ?TIMEOUT_RUN).
  
run(Code, Arguments, State, Timeout) ->
  Deadline = ?MONOTONIC_MS + Timeout,
  case erl_py_runner_pool:get_worker(Timeout) of
    {ok, Worker} ->
      Remaining = max(1, Deadline - ?MONOTONIC_MS),
      gen_server:call(Worker, ?CALL_RUN(Code, Arguments, State), Remaining);
    {error, _} = Error ->
      Error
  end.

load_library(Worker, Name, Code) ->
  gen_server:call(Worker, ?CALL_LOAD_LIBRARY(Name, Code), ?TIMEOUT_LOAD_LIBRARY).

delete_library(Worker, Name) ->
  gen_server:call(Worker, ?CALL_DELETE_LIBRARY(Name), ?TIMEOUT_LOAD_LIBRARY).

info(Worker) ->
  gen_server:call(Worker, ?CALL_INFO, ?TIMEOUT_INFO).

%%% +--------------------------------------------------------------+
%%% |                     Gen Server Callbacks                     |
%%% +--------------------------------------------------------------+

init(#{
  runner := Runner,
  timeout := Timeout,
  python_modules := AllowedPythonModules,
  erlang_modules := AllowedErlangModules
}) ->
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
  Data =
    #data{
      port = Port,
      os_pid = OsPid,
      timeout = Timeout,
      allowed_modules =
        case AllowedErlangModules of
          Modules when is_list(Modules) -> maps:from_keys(Modules, true);
          all -> all;
          _Other -> #{}
        end
    },
  ok = handshake(Data, AllowedPythonModules),
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  case reload_libraries(Data, Libraries) of
    ok ->
      erl_py_runner_pool:send_worker_start(self()),
      {ok, Data};
    {error, Reason} ->
      ?LOGERROR("failed to init worker due to library reload fail: ~p", [Reason]),
      {stop, {library_init_failed, Reason}}
  end.

handle_call(
  ?CALL_RUN(Code, Arguments, State),
  _Caller,
  #data{port = Port, timeout = Timeout} = Data
) ->
  send_port_command(Port, ?COMMAND_EXECUTE(Code, Arguments, State)),
  Deadline = ?MONOTONIC_MS + Timeout,
  Result = wait_port_response(Data, Deadline),
  erl_py_runner_pool:send_worker_ready(self()),
  {reply, Result, Data};

handle_call(
  ?CALL_LOAD_LIBRARY(Name, Code),
  _Caller,
  #data{port = Port, timeout = Timeout} = Data
) ->
  send_port_command(Port, ?COMMAND_LOAD_LIBRARY(Name, Code)),
  Deadline = ?MONOTONIC_MS + Timeout,
  Result = wait_port_response(Data, Deadline),
  {reply, Result, Data};

handle_call(
  ?CALL_DELETE_LIBRARY(Name),
  _Caller,
  #data{port = Port, timeout = Timeout} = Data
) ->
  send_port_command(Port, ?COMMAND_DELETE_LIBRARY(Name)),
  Deadline = ?MONOTONIC_MS + Timeout,
  Result = wait_port_response(Data, Deadline),
  {reply, Result, Data};

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
    timeout = Timeout
  } = Data,
  AllowedPythonModules
) ->
  send_port_command(Port, ?COMMAND_INIT(AllowedPythonModules)),
  Deadline = ?MONOTONIC_MS + Timeout,
  case wait_port_response(Data, Deadline) of
    ok -> ok;
    {ok, Response} -> exit({init_unexpected, Response});
    {error, Error} -> exit({init_failed, Error})
  end.

wait_port_response(
  #data{port = Port} = Data,
  Deadline
) ->
  Remaining = max(0, Deadline - ?MONOTONIC_MS),
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
        {call, RequestID, Module, Function, Arguments} ->
          handle_callback(RequestID, Module, Function, Arguments, Data),
          wait_port_response(Data, Deadline);
        {error, Error} ->
          {error, Error}
      end;
    {Port, {exit_status, StatusCode}} ->
      ?LOGERROR("port exited during execution: ~p, status code: ~p", [Port, StatusCode]),
      exit({port_exit, {status_code, StatusCode}})
  after
    Remaining ->
      ?LOGERROR("port timeout after ~p ms, closing port: ~p", [Remaining, Port]),
      catch erlang:port_close(Port),
      exit(port_timeout)
  end.

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

send_port_command(Port, Term) ->
  case erlang:port_command(Port, erlang:term_to_binary(Term), [nosuspend]) of
    true ->
      ok;
    false ->
      catch erlang:port_close(Port),
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
  code:ensure_loaded(Module),
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

reload_libraries(_Data, []) ->
  ok;
reload_libraries(#data{port = Port, timeout = Timeout} = Data, [{Name, Code} | Rest]) ->
  send_port_command(Port, ?COMMAND_LOAD_LIBRARY(Name, Code)),
  Deadline = ?MONOTONIC_MS + Timeout,
  case wait_port_response(Data, Deadline) of
    ok ->
      reload_libraries(Data, Rest);
    {error, Error} ->
      {error, Error}
  end.

kill_os_process(undefined) ->
  ok;
kill_os_process(OsPid) ->
  os:cmd("kill -15 " ++ erlang:integer_to_list(OsPid)),
  ok.