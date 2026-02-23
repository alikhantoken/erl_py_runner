%%% +--------------------------------------------------------------+
%%% | Common Test suite for erl_py_runner                         |
%%% +--------------------------------------------------------------+

-module(erl_py_runner_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%% +--------------------------------------------------------------+
%%% |                     Suite configuration                      |
%%% +--------------------------------------------------------------+

suite() ->
  [{timetrap, {seconds, 120}}].

all() ->
  [{group, basic_runs},
   {group, python_modules},
   {group, erlang_call_requests},
   {group, parallel_execution},
   {group, error_handling}].

groups() ->
  [{basic_runs, [sequence], [
    simple_result,
    arithmetic_calculation,
    string_concatenation,
    list_operation,
    dictionary_operation,
    non_as_result
  ]},
    {python_modules, [sequence], [
      import_allowed_module,
      import_allowed_json_module,
      import_disallowed_module,
      import_disallowed_subprocess,
      disallowed_file_read
    ]},
    {erlang_call_requests, [sequence], [
      call_math_module,
      call_lists_module,
      call_erlang_module,
      call_disallowed_module,
      call_invalid_function,
      call_bad_arity
    ]},
    {parallel_execution, [sequence], [
      parallel_basic,
      parallel_arguments,
      parallel_mix
    ]},
    {error_handling, [sequence], [
      python_runtime,
      python_syntax
    ]}].

%%% +--------------------------------------------------------------+
%%% |                   Suite init / cleanup                       |
%%% +--------------------------------------------------------------+

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(erl_py_runner),
  Config.

end_per_suite(_Config) ->
  application:stop(erl_py_runner),
  ok.

init_per_group(_GroupName, Config) ->
  Config.

end_per_group(_GroupName, _Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

%%% +--------------------------------------------------------------+
%%% |                 Group: basic_runs                         |
%%% +--------------------------------------------------------------+

simple_result(_Config) ->
  Code = <<"result = 42">>,
  {ok, Result} = erl_py_runner:run(Code, #{}),
  ?assertEqual(42, Result).

arithmetic_calculation(_Config) ->
  Code = <<"result = arguments['a'] + arguments['b']">>,
  Arguments = #{a => 5, b => 10},
  {ok, Result} = erl_py_runner:run(Code, Arguments),
  ?assertEqual(15, Result).

string_concatenation(_Config) ->
  Code = <<"result = arguments['first'].decode('utf-8') + ' ' + arguments['second'].decode('utf-8')">>,
  Arguments = #{first => <<"hello">>, second => <<"world">>},
  {ok, Result} = erl_py_runner:run(Code, Arguments),
  ?assertEqual("hello world", Result).

list_operation(_Config) ->
  Code = <<"result = list(range(1, 6))">>,
  {ok, Result} = erl_py_runner:run(Code, #{}),
  ?assertEqual([1, 2, 3, 4, 5], Result).

dictionary_operation(_Config) ->
  Code = <<"result = {'name': 'Bobby', 'age': 777}">>,
  {ok, Result} = erl_py_runner:run(Code, #{}),
  ?assertEqual(#{"name" => "Bobby", "age" => 777}, Result).

non_as_result(_Config) ->
  Code = <<"pass">>,
  {ok, Result} = erl_py_runner:run(Code, #{}),
  ?assertEqual(undefined, Result).

%%% +--------------------------------------------------------------+
%%% |                 Group: python_modules                        |
%%% +--------------------------------------------------------------+

import_allowed_module(_Config) ->
  Code = <<"import math\nresult = math.sqrt(16)">>,
  {ok, Result} = erl_py_runner:run(Code, #{}),
  ?assertEqual(4.0, Result).

import_allowed_json_module(_Config) ->
  Code = <<"import json\nresult = json.dumps({'key': 'value'}).encode('utf-8')">>,
  {ok, Result} = erl_py_runner:run(Code, #{}),
  ?assertEqual(true, is_binary(Result)),
  ?assertEqual(#{<<"key">> => <<"value">>}, json:decode(Result)).

import_disallowed_module(_Config) ->
  Code = <<"import os\nresult = os.getcwd()">>,
  {error, _} = erl_py_runner:run(Code, #{}).

import_disallowed_subprocess(_Config) ->
  Code = <<"import subprocess\nresult = subprocess.run(['ls'])">>,
  {error, _} = erl_py_runner:run(Code, #{}).

disallowed_file_read(_Config) ->
  Code = <<"f = open('/etc/passwd')\nresult = f.read()">>,
  {error, _} = erl_py_runner:run(Code, #{}).

%%% +--------------------------------------------------------------+
%%% |                 Group: erlang_call_requests                      |
%%% +--------------------------------------------------------------+

call_math_module(_Config) ->
  Code = <<"result = erlang.call('math', 'sqrt', [144])">>,
  {ok, Result} = erl_py_runner:run(Code, #{}),
  ?assertEqual(12.0, Result).

call_lists_module(_Config) ->
  Code = <<"result = erlang.call('lists', 'reverse', [[1, 2, 3]])">>,
  {ok, Result} = erl_py_runner:run(Code, #{}),
  ?assertEqual([3, 2, 1], Result).

call_erlang_module(_Config) ->
  Code = <<"result = erlang.call('erlang', 'length', [[1, 2, 3]])">>,
  {ok, Result} = erl_py_runner:run(Code, #{}),
  ?assertEqual(3, Result).

call_disallowed_module(_Config) ->
  Code = <<"result = erlang.call('file', 'read_file', ['/etc/passwd'])">>,
  {error, _} = erl_py_runner:run(Code, #{}).

call_invalid_function(_Config) ->
  Code = <<"result = erlang.call('math', 'nonexistent_func', [1])">>,
  {error, _} = erl_py_runner:run(Code, #{}).

call_bad_arity(_Config) ->
  Code = <<"result = erlang.call('math', 'sqrt', [1, 2])">>,
  {error, _} = erl_py_runner:run(Code, #{}).

%%% +--------------------------------------------------------------+
%%% |                 Group: parallel_execution                    |
%%% +--------------------------------------------------------------+

parallel_basic(_Config) ->
  N = 20,
  Results =
    parallel_run(N,
      fun(_I) ->
        {<<"result = 42">>, #{}}
      end),
  lists:foreach(
    fun({ok, 42}) -> ok end,
    Results
  ).

parallel_arguments(_Config) ->
  N = 20,
  Results =
    parallel_run(N,
      fun(I) ->
        {<<"result = arguments['index'] * 2">>, #{index => I}}
      end),
  lists:foreach(
    fun({I, {ok, R}}) ->
      Expected = I * 2,
      Expected = R
    end,
    lists:zip(lists:seq(1, N), Results)
  ).

parallel_mix(_Config) ->
  N = 20,
  Results =
    parallel_run(N,
      fun
        (I) when I rem 2 =:= 0 ->
          {<<"result = arguments['index']">>, #{index => I}};
        (I) ->
          {<<"raise ValueError('odd index')">>, #{index => I}}
      end),
  lists:foreach(
    fun({I, Result}) ->
      case I rem 2 of
        0 ->
          {ok, I} = Result;
        1 ->
          {error, _} = Result
      end
    end,
    lists:zip(lists:seq(1, N), Results)
  ).

%%% +--------------------------------------------------------------+
%%% |                 Group: error_handling                        |
%%% +--------------------------------------------------------------+

python_runtime(_Config) ->
  Code = <<"result = 1 / 0">>,
  {error, _} = erl_py_runner:run(Code, #{}).

python_syntax(_Config) ->
  Code = <<"def f(\nresult = 1">>,
  {error, _} = erl_py_runner:run(Code, #{}).

%%% +--------------------------------------------------------------+
%%% |                         Helpers                              |
%%% +--------------------------------------------------------------+

parallel_run(N, Fun) ->
  Parent = self(),
  PIDs =
    [spawn_link(
      fun() ->
        {Code, Arguments} = Fun(I),
        Result = erl_py_runner:run(Code, Arguments),
        Parent ! {self(), Result}
      end) || I <- lists:seq(1, N)],
  [receive {PID, Result} -> Result end || PID <- PIDs].