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
  run/1, run/2
]).

%%% +--------------------------------------------------------------+
%%% |                         Implementation                       |
%%% +--------------------------------------------------------------+

start_child(Number, Config) ->
  {ok, PID} =
    supervisor:start_child(erl_py_runner_worker_sup, #{
      id => get_id(Number),
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
        process_flag(trap_exit, true),
        State = init(Config),
        erl_py_runner_pool:send_worker_start(self()),
        loop(State)
      end),
  {ok, PID}.

run(Data) ->
  run(Data, _DefaultTimeout = 60000).
run(Data, Timeout) ->
  Worker = erl_py_runner_pool:get_worker(Timeout),
  do_run(Worker, Data, Timeout).

%%% +--------------------------------------------------------------+
%%% |                       Internal functions                     |
%%% +--------------------------------------------------------------+

do_run(Worker, Data, Timeout) ->
  Ref = erlang:monitor(process, Worker),
  Worker ! {run, self(), Data},
  receive
    {complete, Worker, Output} ->
      erlang:demonitor(Ref, [flush]),
      {ok, Output};
    {error, Worker, Reason} ->
      erlang:demonitor(Ref, [flush]),
      {error, Reason};
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
  init_port(Port, Timeout, AllowedPythonModules),
  #state{
    port = Port,
    timeout = Timeout,
    allowed_modules = AllowedErlangModules
  }.

init_port(Port, Timeout, AllowedModules) ->
  Payload =
    erlang:term_to_binary(#{
      type => init,
      allowed_modules => AllowedModules
    }),
  erlang:port_command(Port, Payload, [nosuspend]),
  receive
    {Port, {data, Response}} ->
      case erlang:binary_to_term(Response) of
        #{status := ?CALL_STATUS_OK} ->
          ok;
        #{status := ?CALL_STATUS_ERROR, error := Reason} ->
          erlang:port_close(Port),
          exit({initialization_fail, Reason})
      end
  after
    Timeout ->
      erlang:port_close(Port),
      exit(initialization_timeout)
  end.
  
loop(#state{port = Port} = State) ->
  receive
    {run, Caller, Data} ->
      case send_data(State, Data) of
        {ok, Output} ->
          Caller ! {complete, self(), Output};
        {error, Reason} ->
          ?LOGERROR("failed send data to port: ~p, caller: ~p, reason: ~p", [
            Port,
            Caller,
            Reason
          ]),
          Caller ! {error, self(), Reason}
      end,
      erl_py_runner_pool:send_worker_ready(self()),
      loop(State);
    {'EXIT', Port, Reason} ->
      ?LOGERROR("received exit from port, reason: ~p", [Reason]),
      exit(Reason);
    {'EXIT', From, Reason} ->
      ?LOGERROR("received exit from: ~p, reason: ~p", [From, Reason]),
      erlang:port_close(Port),
      exit(Reason);
    Unexpected ->
      ?LOGWARNING("received unexpected message: ~p", [Unexpected]),
      loop(State)
  end.
  
send_data(
  #state{port = Port} = State,
  #{code := _, arguments := _} = Inputs
) ->
  case erlang:port_command(Port, erlang:term_to_binary(Inputs), [nosuspend]) of
    true -> wait_data(State);
    false -> {error, aborted}
  end;
send_data(_State, _Inputs) ->
  {error, invalid_inputs}.

wait_data(#state{port = Port, timeout = Timeout} = State) ->
  receive
    {Port, {data, Data}} ->
      case erlang:binary_to_term(Data) of
        #{type := ?CALL_REQUEST} = CallRequest ->
          handle_call(State, CallRequest),
          wait_data(State);
        Response ->
          {ok, Response}
      end;
    {'EXIT', Port, Reason} ->
      ?LOGERROR("received exit from port: ~p, reason: ~p", [Port, Reason]),
      exit(Reason)
  after
    Timeout ->
      ?LOGERROR("port timeout after ~p ms, closing port: ~p", [Timeout, Port]),
      erlang:port_close(Port),
      exit(timeout)
  end.

%%% +--------------------------------------------------------------+
%%% |                     Erlang callback protocol                  |
%%% +--------------------------------------------------------------+

handle_call(
  #state{
    port = Port,
    allowed_modules = AllowedModules
  },
  #{
    request_id := RequestID,
    module := Module,
    function := Function,
    args := Args
  }
) ->
  Response =
    try
      check_module(Module, AllowedModules),
      #{
        status => ?CALL_STATUS_OK,
        type => ?CALL_RESPONSE,
        request_id => RequestID,
        result => Module:Function(Args)
      }
    catch
      throw:{ErrorType, ErrorMessage}
        when ErrorType =:= not_found orelse ErrorType =:= not_allowed ->
        #{
          status => ?CALL_STATUS_ERROR,
          type => ?CALL_RESPONSE,
          request_id => RequestID,
          error => ErrorMessage
        };
      _Class:Error ->
        #{
          status => ?CALL_STATUS_ERROR,
          type => ?CALL_RESPONSE,
          request_id => RequestID,
          error => iolist_to_binary(io_lib:format("~p", [Error]))
        }
    end,
  erlang:port_command(Port, erlang:term_to_binary(Response), [nosuspend]).

check_module(_Module, all) ->
  ok;
check_module(Module, AllowedModules) ->
  case lists:member(Module, AllowedModules) of
    true ->
      ok;
    false ->
      ErrorMessage = iolist_to_binary(io_lib:format("module ~ts is not allowed", [Module])),
      throw({not_allowed, ErrorMessage})
  end.

get_id(Index) ->
  erlang:list_to_atom(?WORKER_NAME ++ erlang:integer_to_list(Index)).