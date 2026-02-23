%%% +--------------------------------------------------------------+
%%% | Copyright (c) 2026. All Rights Reserved.                     |
%%% | Author: Tokenov Alikhan, alikhantokenov@gmail.com            |
%%% +--------------------------------------------------------------+

-module(erl_py_runner_worker).

-include("erl_py_runner.hrl").
-include("erl_py_runner_worker.hrl").

%%% +--------------------------------------------------------------+
%%% |                              API                             |
%%% +--------------------------------------------------------------+

-export([
  start_child/2,
  start/1,
  run/2, run/3
]).

%%% +--------------------------------------------------------------+
%%% |                         Implementation                       |
%%% +--------------------------------------------------------------+

start_child(Number, Config) ->
  {ok, PID} =
    supervisor:start_child(erl_py_runner_worker_sup, #{
      id => new_worker_id(Number),
      start => {erl_py_runner_worker, start, [Config]},
      restart => permanent,
      shutdown => 60000,
      type => worker
    }),
  PID.
  
start(Config) ->
  PID =
    spawn_link(
      fun() ->
        % We need this flag to be able to close the port before process shutdown.
        process_flag(trap_exit, true),
        State = init(Config),
        erl_py_runner_pool:send_worker_start(self()),
        loop(State)
      end),
  {ok, PID}.

run(Code, Arguments) ->
  run(Code, Arguments, _DefaultTimeout = 60000).
run(Code, Arguments, Timeout) ->
  Worker = erl_py_runner_pool:get_worker(Timeout),
  do_run(Worker, Code, Arguments, Timeout).

%%% +--------------------------------------------------------------+
%%% |                       Internal functions                     |
%%% +--------------------------------------------------------------+

new_worker_id(Number) ->
  erlang:list_to_atom(?WORKER_NAME ++ erlang:integer_to_list(Number)).
  
send_ok(Caller, CallRef, Output) ->
  Caller ! {complete, CallRef, Output}.

send_error(Caller, CallRef, Error) ->
  Caller ! {error, CallRef, Error}.

do_run(Worker, Code, Arguments, Timeout) ->
  MonitorRef = erlang:monitor(process, Worker),
  CallRef = erlang:make_ref(),
  Worker ! {run, self(), CallRef, Code, Arguments},
  receive
    {complete, CallRef, Output} ->
      erlang:demonitor(MonitorRef, [flush]),
      {ok, Output};
    {error, CallRef, Error} ->
      erlang:demonitor(MonitorRef, [flush]),
      {error, Error};
    {'DOWN', MonitorRef, process, Worker, Reason} ->
      {error, Reason}
  after
    Timeout ->
      erlang:demonitor(MonitorRef, [flush]),
      {error, timeout}
  end.

init(#{
  runner := Runner,
  timeout := Timeout,
  python_modules := AllowedPythonModules,
  erlang_modules := AllowedErlangModules
}) ->
  Port =
    erlang:open_port(
      {spawn, Runner},
      [{packet, 4}, binary, exit_status]
    ),
  handshake(Port, Timeout, AllowedPythonModules),
  #state{
    port = Port,
    timeout = Timeout,
    allowed_modules =
      case AllowedErlangModules of
        Modules when is_list(Modules) -> maps:from_keys(AllowedErlangModules, true);
        all -> all;
        _Other -> #{}
      end
  }.

handshake(Port, Timeout, AllowedModules) ->
  try_port_command(Port, ?COMMAND_INIT(AllowedModules)),
  receive
    {Port, {data, Response}} ->
      case erlang:binary_to_term(Response) of
        ok ->
          ok;
        {error, Error} ->
          ?LOGERROR("received error from port during initialization: ~p, error: ~p", [
            Port,
            Error
          ]),
          erlang:port_close(Port),
          exit({initialization_fail, Error})
      end;
    {Port, {exit_status, StatusCode}} ->
      ?LOGERROR("received exit from port: ~p, status code: ~p", [Port, StatusCode]),
      exit({port_exit, {status_code, StatusCode}})
  after
    Timeout ->
      erlang:port_close(Port),
      exit(initialization_timeout)
  end.
  
loop(#state{port = Port} = State) ->
  receive
    {run, Caller, CallRef, Code, Arguments} ->
      case send_run_command(Code, Arguments, State) of
        {ok, Output} ->
          send_ok(Caller, CallRef, Output);
        {error, Error} ->
          ?LOGERROR("failed to execute script on port: ~p, caller: ~p, error: ~p", [
            Port,
            Caller,
            Error
          ]),
          send_error(Caller, CallRef, Error)
      end,
      erl_py_runner_pool:send_worker_ready(self()),
      loop(State);
    {Port, {exit_status, StatusCode}} ->
      ?LOGERROR("received exit from port: ~p, status code: ~p", [Port, StatusCode]),
      exit({port_exit, {status_code, StatusCode}});
    {'EXIT', From, Reason} ->
      ?LOGERROR("received exit from linked: ~p, reason: ~p", [From, Reason]),
      erlang:port_close(Port),
      exit(Reason);
    Unexpected ->
      ?LOGWARNING("received unexpected message: ~p, ignored", [Unexpected]),
      loop(State)
  end.
 
send_run_command(
  Code,
  Arguments,
  #state{
    port = Port,
    timeout = Timeout
  } = State
) ->
  Deadline = erlang:monotonic_time(millisecond) + Timeout,
  try_port_command(Port, ?COMMAND_EXECUTE(Code, Arguments)),
  wait_data(State, Deadline).
  
wait_data(
  #state{port = Port} = State,
  Deadline
) ->
  Remaining = Deadline - erlang:monotonic_time(millisecond),
  Timeout = max(0, Remaining),
  receive
    {Port, {data, Binary}} ->
      Term =
        try erlang:binary_to_term(Binary)
        catch _:_ -> {error, invalid_term_received}
        end,
      case Term of
        {ok, Response} ->
          {ok, Response};
        {call, RequestID, Module, Function, Arguments} ->
          handle_call(RequestID, Module, Function, Arguments, State),
          wait_data(State, Deadline);
        {error, Error} ->
          {error, Error}
      end;
    {Port, {exit_status, StatusCode}} ->
      ?LOGERROR("received exit from port: ~p, status code: ~p", [Port, StatusCode]),
      exit({port_exit, {status_code, StatusCode}})
  after
    Timeout ->
      ?LOGERROR("port timeout after ~p ms, closing port: ~p", [Timeout, Port]),
      erlang:port_close(Port),
      exit(port_timeout)
  end.

%%% +--------------------------------------------------------------+
%%% |                     Erlang callback protocol                  |
%%% +--------------------------------------------------------------+

handle_call(
  RequestID,
  Module,
  Function,
  Arguments,
  #state{
    port = Port,
    allowed_modules = AllowedModules
  }
) ->
  Response =
    try
      is_module_allowed(Module, AllowedModules),
      is_function_available(Module, Function, Arguments),
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
  try_port_command(Port, Response).

try_port_command(Port, Term) ->
  case erlang:port_command(Port, erlang:term_to_binary(Term), [nosuspend]) of
    true ->
      ok;
    false ->
      erlang:port_close(Port),
      exit(port_command_aborted)
  end.

is_module_allowed(_Module, all) ->
  ok;
is_module_allowed(Module, AllowedModules) ->
  case AllowedModules of
    #{Module := true} ->
      ok;
    _Other ->
      throw(module_not_allowed)
  end.
  
is_function_available(Module, Function, Arguments) when is_list(Arguments) ->
  code:ensure_loaded(Module),
  case erlang:function_exported(Module, Function, length(Arguments)) of
    true ->
      ok;
    false ->
      throw(function_not_found)
  end;
is_function_available(_Module, _Function, _Arguments) ->
  throw(function_arguments_not_list).