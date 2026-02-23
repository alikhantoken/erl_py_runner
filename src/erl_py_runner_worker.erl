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

do_run(Worker, Code, Arguments, Timeout) ->
  Ref = erlang:monitor(process, Worker),
  Worker ! {run, self(), Code, Arguments},
  receive
    {complete, Worker, Output} ->
      erlang:demonitor(Ref, [flush]),
      {ok, Output};
    {error, Worker, Error} ->
      erlang:demonitor(Ref, [flush]),
      {error, Error};
    {'DOWN', Ref, process, Worker, Reason} ->
      {error, Reason}
  after Timeout ->
    erlang:demonitor(Ref, [flush]),
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
  Payload = erlang:term_to_binary({init, AllowedModules}),
  erlang:port_command(Port, Payload, [nosuspend]),
  receive
    {Port, {data, Response}} ->
      case erlang:binary_to_term(Response) of
        ?CALL_STATUS_OK ->
          ok;
        #{status := ?CALL_STATUS_ERROR, error := Error} ->
          ?LOGERROR("received error from port during initialization: ~p, error: ~p", [
            Port,
            Error
          ]),
          erlang:port_close(Port),
          exit({initialization_fail, Error});
        _Response ->
          ?LOGINFO("[debug] RESPONSE: ~p", [erlang:binary_to_term(Response)])
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
    {run, Caller, Code, Arguments} ->
      case send_data(Code, Arguments, State) of
        {ok, Output} ->
          send_complete(Caller, Output);
        {error, Reason} ->
          ?LOGERROR("failed to execute script on port: ~p, caller: ~p, reason: ~p", [
            Port,
            Caller,
            Reason
          ]),
          send_error(Caller, Reason)
      end,
      erl_py_runner_pool:send_worker_ready(self()),
      loop(State);
    {Port, {exit_status, StatusCode}} ->
      ?LOGERROR("received exit from port: ~p, status code: ~p", [Port, StatusCode]),
      exit({port_exit, {status_code, StatusCode}});
    {'EXIT', From, Reason} ->
      ?LOGERROR("received exit from: ~p, reason: ~p", [From, Reason]),
      erlang:port_close(Port),
      exit(Reason);
    Unexpected ->
      ?LOGWARNING("received unexpected message: ~p, ignored", [Unexpected]),
      loop(State)
  end.
  
send_data(
  Code,
  Arguments,
  #state{port = Port} = State
) ->
  case send_command(Port, {exec, Code, Arguments}) of
    ok -> wait_data(State);
    error -> {error, aborted}
  end;
send_data(_Code, _Arguments, _State) ->
  {error, invalid_inputs}.

wait_data(#state{port = Port, timeout = Timeout} = State) ->
  receive
    {Port, {data, Data}} ->
      case erlang:binary_to_term(Data) of
        {ok, Response} ->
          {ok, Response};
        {call, RequestID, Module, Function, Arguments} ->
          handle_call(State, RequestID, Module, Function, Arguments),
          wait_data(State);
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
  #state{
    port = Port,
    allowed_modules = AllowedModules
  },
  RequestID,
  Module,
  Function,
  Arguments
) ->
  Response =
    try
      % TODO: Check if module & function exists
      check_module(Module, AllowedModules),
      {call_reply, RequestID, {ok, erlang:apply(Module, Function, Arguments)}}
    catch
      throw:{ErrorType, ErrorMessage}
        when ErrorType =:= not_found orelse ErrorType =:= not_allowed ->
        {call_reply, RequestID, {error, ErrorMessage}};
      _Class:Error ->
        {call_reply, RequestID, {error, iolist_to_binary(io_lib:format("~p", [Error]))}}
    end,
  send_command(Port, Response).

new_worker_id(Number) ->
  erlang:list_to_atom(?WORKER_NAME ++ erlang:integer_to_list(Number)).
  
send_complete(PID, Output) ->
  PID ! {complete, self(), Output}.
  
send_error(PID, Error) ->
  PID ! {error, self(), Error}.

send_command(Port, Data) ->
  case erlang:port_command(Port, erlang:term_to_binary(Data), [nosuspend]) of
    true -> ok;
    false -> exit(port_command)
  end.

check_module(_Module, all) ->
  ok;
check_module(Module, AllowedModules) ->
  case AllowedModules of
    #{Module := true} ->
      ok;
    _Other ->
      ErrorMessage = iolist_to_binary(io_lib:format("module ~ts is not allowed", [Module])),
      throw({not_allowed, ErrorMessage})
  end.