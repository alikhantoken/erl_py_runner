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
   {group, state_management},
   {group, python_modules},
   {group, erlang_call_requests},
   {group, parallel_execution},
   {group, error_handling},
   {group, edge_cases}].

groups() ->
  [{basic_runs, [sequence], [
    simple_result,
    arithmetic_calculation,
    string_concatenation,
    binary_arguments,
    list_operation,
    dictionary_operation,
    none_as_result,
    nested_data_structures,
    empty_code,
    large_payload
  ]},
  {state_management, [sequence], [
    state_default,
    state_initial,
    state_modify,
    state_type_integer,
    state_type_map,
    state_type_list,
    state_type_binary,
    state_no_modify,
    state_set_none,
    state_complex_modify
  ]},
  {python_modules, [sequence], [
    import_allowed_math,
    import_allowed_re,
    import_allowed_datetime,
    import_allowed_json,
    import_disallowed_os,
    import_disallowed_subprocess,
    import_disallowed_sys,
    import_disallowed_socket,
    disallowed_open_builtin,
    disallowed_eval_builtin,
    disallowed_exec_builtin,
    disallowed_compile_builtin,
    disallowed_import_builtin,
    disallowed_exit_builtin
  ]},
  {erlang_call_requests, [sequence], [
    call_math_sqrt,
    call_lists_reverse,
    call_lists_sort,
    call_maps_get,
    call_string_uppercase,
    call_disallowed_module_file,
    call_disallowed_module_os,
    call_invalid_function,
    call_bad_arity,
    call_no_arguments,
    call_multiple,
    call_result_use
  ]},
  {parallel_execution, [sequence], [
    parallel_basic,
    parallel_arguments,
    parallel_state,
    parallel_mixed,
    parallel_high_concurrency
  ]},
  {error_handling, [sequence], [
    python_division_by_zero_error,
    python_syntax_error,
    python_name_error,
    python_type_error,
    python_key_error,
    python_index_error,
    python_attribute_error,
    python_value_error,
    python_system_exit_error,
    python_recursion_limit_error
  ]},
  {edge_cases, [sequence], [
    empty_arguments,
    unicode_arguments,
    large_arguments,
    boolean_arguments,
    float_arguments,
    negative_numbers_arguments,
    tuple_result,
    nested_state_calls,
    result_state_both_set
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
%%% |                     Group: Basic Usage                       |
%%% +--------------------------------------------------------------+

simple_result(_Config) ->
  ?assertEqual({ok, 42, undefined}, erl_py_runner:run(<<"result = 42">>, #{})).

arithmetic_calculation(_Config) ->
  Code = <<"result = arguments['a'] + arguments['b']">>,
  ?assertEqual({ok, 15, undefined}, erl_py_runner:run(Code, #{a => 5, b => 10})).

string_concatenation(_Config) ->
  Code = <<"result = arguments['first'].decode('utf-8') + ' ' + arguments['second'].decode('utf-8')">>,
  ?assertEqual({ok, "hello world", undefined}, erl_py_runner:run(Code, #{first => <<"hello">>, second => <<"world">>})).

binary_arguments(_Config) ->
  Code = <<"result = arguments['data']">>,
  ?assertEqual({ok, <<"raw bytes">>, undefined}, erl_py_runner:run(Code, #{data => <<"raw bytes">>})).

list_operation(_Config) ->
  ?assertEqual({ok, [1, 2, 3, 4, 5], undefined}, erl_py_runner:run(<<"result = list(range(1, 6))">>, #{})).

dictionary_operation(_Config) ->
  Code = <<"result = {'name': 'Hobbit', 'age': 777}">>,
  {ok, Result, undefined} = erl_py_runner:run(Code, #{}),
  ?assertEqual(#{"name" => "Hobbit", "age" => 777}, Result).

none_as_result(_Config) ->
  ?assertEqual({ok, undefined, undefined}, erl_py_runner:run(<<"pass">>, #{})).

nested_data_structures(_Config) ->
  Code = <<"result = {'items': [1, 2, 3], 'meta': {'count': 3}}">>,
  {ok, Result, _State} = erl_py_runner:run(Code, #{}),
  ?assertEqual(#{"items" => [1, 2, 3], "meta" => #{"count" => 3}}, Result).

empty_code(_Config) ->
  ?assertEqual({ok, undefined, undefined}, erl_py_runner:run(<<"">>, #{})).

large_payload(_Config) ->
  Code = <<"result = list(range(10000))">>,
  {ok, Result, _State} = erl_py_runner:run(Code, #{}),
  ?assertEqual(10000, length(Result)),
  ?assertEqual(0, hd(Result)),
  ?assertEqual(9999, lists:last(Result)).

%%% +--------------------------------------------------------------+
%%% |                   Group: State Management                    |
%%% +--------------------------------------------------------------+

state_default(_Config) ->
  Result = erl_py_runner:run(<<"pass">>, #{}),
  ?assertEqual({ok, undefined, undefined}, Result).

state_initial(_Config) ->
  Code = <<"result = state">>,
  Result = erl_py_runner:run(Code, #{}, 100, 60000),
  ?assertEqual({ok, 100, 100}, Result).

state_modify(_Config) ->
  Code = <<"state = state + 1\nresult = state">>,
  Result = erl_py_runner:run(Code, #{}, 10, 60000),
  ?assertEqual({ok, 11, 11}, Result).

state_type_integer(_Config) ->
  Code = <<"result = state * 2">>,
  Result = erl_py_runner:run(Code, #{}, 11, 60000),
  ?assertEqual({ok, 22, 11}, Result).

state_type_map(_Config) ->
  InitialState = #{counter => 0, name => <<"test">>},
  Code = <<"state['counter'] = state['counter'] + 1\nresult = state['counter']">>,
  Result = erl_py_runner:run(Code, #{}, InitialState, 60000),
  ?assertEqual({ok, 1, #{name => <<"test">>, counter => 1}}, Result).

state_type_list(_Config) ->
  Code = <<"state.append(4000)\nresult = len(state)">>,
  Result = erl_py_runner:run(Code, #{}, [1000, 2000, 3000], 60000),
  ?assertEqual({ok, 4, [1000, 2000, 3000, 4000]}, Result).

state_type_binary(_Config) ->
  Code = <<"result = state">>,
  Result = erl_py_runner:run(Code, #{}, <<"hello">>, 60000),
  ?assertEqual({ok, <<"hello">>, <<"hello">>}, Result).

state_no_modify(_Config) ->
  Code = <<"result = 99">>,
  Result = erl_py_runner:run(Code, #{}, <<"untouched">>, 60000),
  ?assertEqual({ok, 99, <<"untouched">>}, Result).

state_set_none(_Config) ->
  Code = <<"state = None\nresult = 'cleared'">>,
  Result = erl_py_runner:run(Code, #{}, <<"some_state">>, 60000),
  ?assertEqual({ok, "cleared", undefined}, Result).

state_complex_modify(_Config) ->
  InitialState = #{
    items => [],
    total => 0
  },
  Code = <<
    "state['items'] = state['items'] + [arguments['item']]\n"
    "state['total'] = state['total'] + arguments['price']\n"
    "result = state['total']"
  >>,
  {ok, 10, State1} = erl_py_runner:run(Code, #{item => <<"apple">>, price => 10}, InitialState, 60000),
  Result = erl_py_runner:run(Code, #{item => <<"banana">>, price => 15}, State1, 60000),
  ?assertEqual({ok, 25, #{total => 25, items => [<<"apple">>, <<"banana">>]}}, Result).

%%% +--------------------------------------------------------------+
%%% |                    Group: python_modules                     |
%%% +--------------------------------------------------------------+

import_allowed_math(_Config) ->
  Code = <<"import math\nresult = math.sqrt(16)">>,
  ?assertEqual({ok, 4.0, undefined}, erl_py_runner:run(Code, #{})).

import_allowed_re(_Config) ->
  Code = <<"import re\nresult = len(re.findall(r'\\d+', '123 abc 456'))">>,
  ?assertEqual({ok, 2, undefined}, erl_py_runner:run(Code, #{})).

import_allowed_datetime(_Config) ->
  Code = <<"import datetime\nd = datetime.date(2026, 1, 1)\nresult = d.year">>,
  ?assertEqual({ok, 2026, undefined}, erl_py_runner:run(Code, #{})).

import_allowed_json(_Config) ->
  Code = <<"import json\nresult = json.dumps({'key': 'value'}).encode('utf-8')">>,
  {ok, Result, _State} = erl_py_runner:run(Code, #{}),
  ?assert(is_binary(Result)),
  ?assertEqual(#{<<"key">> => <<"value">>}, json:decode(Result)).

import_disallowed_os(_Config) ->
  {error, _} = erl_py_runner:run(<<"import os\nresult = os.getcwd()">>, #{}).

import_disallowed_subprocess(_Config) ->
  {error, _} = erl_py_runner:run(<<"import subprocess\nresult = subprocess.run(['ls'])">>, #{}).

import_disallowed_sys(_Config) ->
  {error, _} = erl_py_runner:run(<<"import sys\nresult = sys.path">>, #{}).

import_disallowed_socket(_Config) ->
  {error, _} = erl_py_runner:run(<<"import socket\nresult = socket.gethostname()">>, #{}).

disallowed_open_builtin(_Config) ->
  {error, _} = erl_py_runner:run(<<"f = open('/etc/passwd')\nresult = f.read()">>, #{}).

disallowed_eval_builtin(_Config) ->
  {error, _} = erl_py_runner:run(<<"result = eval('1 + 1')">>, #{}).

disallowed_exec_builtin(_Config) ->
  {error, _} = erl_py_runner:run(<<"exec('result = 1')">>, #{}).

disallowed_compile_builtin(_Config) ->
  {error, _} = erl_py_runner:run(<<"c = compile('1+1', '<string>', 'eval')\nresult = c">>, #{}).

disallowed_import_builtin(_Config) ->
  {error, _} = erl_py_runner:run(<<"m = __import__('os')\nresult = m.getcwd()">>, #{}).

disallowed_exit_builtin(_Config) ->
  {error, _} = erl_py_runner:run(<<"exit(0)">>, #{}).

%%% +--------------------------------------------------------------+
%%% |                 Group: erlang_call_requests                  |
%%% +--------------------------------------------------------------+

call_math_sqrt(_Config) ->
  Code = <<"result = erlang.call('math', 'sqrt', [144])">>,
  ?assertEqual({ok, 12.0, undefined}, erl_py_runner:run(Code, #{})).

call_lists_reverse(_Config) ->
  Code = <<"result = erlang.call('lists', 'reverse', [[1, 2, 3]])">>,
  ?assertEqual({ok, [3, 2, 1], undefined}, erl_py_runner:run(Code, #{})).

call_lists_sort(_Config) ->
  Code = <<"result = erlang.call('lists', 'sort', [[3, 1, 2]])">>,
  ?assertEqual({ok, [1, 2, 3], undefined}, erl_py_runner:run(Code, #{})).

call_maps_get(_Config) ->
  Code = <<"result = erlang.call('maps', 'get', ['key', {'key': 42}])">>,
  ?assertEqual({ok, 42, undefined}, erl_py_runner:run(Code, #{})).

call_string_uppercase(_Config) ->
  Code = <<"result = erlang.call('string', 'uppercase', ['hello'])">>,
  ?assertEqual({ok, "HELLO", undefined}, erl_py_runner:run(Code, #{})).

call_disallowed_module_file(_Config) ->
  Code = <<"result = erlang.call('file', 'read_file', ['/etc/passwd'])">>,
  {error, _} = erl_py_runner:run(Code, #{}).

call_disallowed_module_os(_Config) ->
  Code = <<"result = erlang.call('os', 'cmd', ['ls'])">>,
  {error, _} = erl_py_runner:run(Code, #{}).

call_invalid_function(_Config) ->
  Code = <<"result = erlang.call('math', 'nonexistent_func', [1])">>,
  {error, _} = erl_py_runner:run(Code, #{}).

call_bad_arity(_Config) ->
  Code = <<"result = erlang.call('math', 'sqrt', [1, 2])">>,
  {error, _} = erl_py_runner:run(Code, #{}).

call_no_arguments(_Config) ->
  Code = <<"result = erlang.call('erlang', 'node', [])">>,
  {ok, Result, _State} = erl_py_runner:run(Code, #{}),
  ?assert(is_atom(Result)).

call_multiple(_Config) ->
  Code = <<
    "a = erlang.call('math', 'sqrt', [9])\n"
    "b = erlang.call('math', 'sqrt', [16])\n"
    "result = a + b"
  >>,
  ?assertEqual({ok, 7.0, undefined}, erl_py_runner:run(Code, #{})).

call_result_use(_Config) ->
  Code = <<"reversed = erlang.call('lists', 'reverse', [[1, 2, 3]])\nresult = reversed[0]">>,
  ?assertEqual({ok, [3], undefined}, erl_py_runner:run(Code, #{})).

%%% +--------------------------------------------------------------+
%%% |                  Group: parallel_execution                   |
%%% +--------------------------------------------------------------+

parallel_basic(_Config) ->
  TotalIterations = 20,
  Results =
    parallel_run(
      TotalIterations,
      fun(_Iteration) ->
        {<<"result = 25">>, #{}, undefined}
      end
    ),
  lists:foreach(fun({ok, 25, _State}) -> ok end, Results).

parallel_arguments(_Config) ->
  TotalIterations = 20,
  Results =
    parallel_run(
      TotalIterations,
      fun(Iteration) ->
        {<<"result = arguments['index'] * 2">>, #{index => Iteration}, undefined}
      end
    ),
  lists:foreach(
    fun({Iteration, {ok, Result, _State}}) ->
      ?assertEqual(Iteration * 2, Result)
    end,
    lists:zip(lists:seq(1, TotalIterations), Results)
  ).

parallel_state(_Config) ->
  TotalIterations = 20,
  Results =
    parallel_run(
      TotalIterations,
      fun(Iteration) ->
        {<<"state = state + arguments['value']\nresult = state">>, #{value => Iteration}, 100}
      end
    ),
  lists:foreach(
    fun({Iteration, {ok, Result, State}}) ->
      Expected = 100 + Iteration,
      ?assertEqual(Expected, Result),
      ?assertEqual(Expected, State)
    end,
    lists:zip(lists:seq(1, TotalIterations), Results)
  ).

parallel_mixed(_Config) ->
  TotalIterations = 20,
  Results =
    parallel_run(
      TotalIterations,
      fun
        (Iteration) when Iteration rem 2 =:= 0 ->
          {<<"result = arguments['index']">>, #{index => Iteration}, undefined};
        (Iteration) ->
          {<<"raise ValueError('odd index')">>, #{index => Iteration}, undefined}
      end
    ),
  lists:foreach(
    fun({Iteration, Result}) ->
      case Iteration rem 2 of
        0 -> {ok, Iteration, _State} = Result;
        1 -> {error, _} = Result
      end
    end,
    lists:zip(lists:seq(1, TotalIterations), Results)
  ).

parallel_high_concurrency(_Config) ->
  TotalIterations = 50,
  Results =
    parallel_run(
      TotalIterations,
      fun(Iteration) ->
        {<<"result = arguments['i']">>, #{i => Iteration}, undefined}
      end
    ),
  ?assertEqual(TotalIterations, length(Results)),
  lists:foreach(
    fun({Iteration, {ok, Result, _State}}) ->
      ?assertEqual(Iteration, Result)
    end,
    lists:zip(lists:seq(1, TotalIterations), Results)
  ).

%%% +--------------------------------------------------------------+
%%% |                    Group: error_handling                     |
%%% +--------------------------------------------------------------+

python_division_by_zero_error(_Config) ->
  {error, _} = erl_py_runner:run(<<"result = 1 / 0">>, #{}).

python_syntax_error(_Config) ->
  {error, _} = erl_py_runner:run(<<"def f(\nresult = 1">>, #{}).

python_name_error(_Config) ->
  {error, _} = erl_py_runner:run(<<"result = undefined_variable">>, #{}).

python_type_error(_Config) ->
  {error, _} = erl_py_runner:run(<<"result = 'string' + 1">>, #{}).

python_key_error(_Config) ->
  {error, _} = erl_py_runner:run(<<"result = arguments['missing']">>, #{}).

python_index_error(_Config) ->
  {error, _} = erl_py_runner:run(<<"result = [1, 2][10]">>, #{}).

python_attribute_error(_Config) ->
  {error, _} = erl_py_runner:run(<<"result = (1).nonexistent">>, #{}).

python_value_error(_Config) ->
  {error, _} = erl_py_runner:run(<<"result = int('not_a_number')">>, #{}).

python_system_exit_error(_Config) ->
  {error, _} = erl_py_runner:run(<<"raise SystemExit(0)">>, #{}).

python_recursion_limit_error(_Config) ->
  Code = <<"
    def recurse(n):
        return recurse(n + 1)
    result = recurse(0)
  ">>,
  {error, _} = erl_py_runner:run(Code, #{}).

%%% +--------------------------------------------------------------+
%%% |                      Group: edge_cases                       |
%%% +--------------------------------------------------------------+

empty_arguments(_Config) ->
  Code = <<"result = len(arguments)">>,
  ?assertEqual({ok, 0, undefined}, erl_py_runner:run(Code, #{})).

unicode_arguments(_Config) ->
  Code = <<"result = arguments['text']">>,
  {ok, Result, _State} = erl_py_runner:run(Code, #{text => <<"hello">>}),
  ?assertEqual(<<"hello">>, Result).

large_arguments(_Config) ->
  LargeList = lists:seq(1, 5000),
  Code = <<"result = len(arguments['data'])">>,
  ?assertEqual({ok, 5000, undefined}, erl_py_runner:run(Code, #{data => LargeList})).

boolean_arguments(_Config) ->
  Code = <<"result = arguments['flag']">>,
  ?assertEqual({ok, true, undefined}, erl_py_runner:run(Code, #{flag => true})),
  ?assertEqual({ok, false, undefined}, erl_py_runner:run(Code, #{flag => false})).

float_arguments(_Config) ->
  Code = <<"result = 0.1 + 0.2">>,
  {ok, Result, undefined} = erl_py_runner:run(Code, #{}),
  ?assert(abs(Result - 0.3) < 0.0001).

negative_numbers_arguments(_Config) ->
  Code = <<"result = arguments['n'] * -1">>,
  ?assertEqual({ok, -7, undefined}, erl_py_runner:run(Code, #{n => 7})).

tuple_result(_Config) ->
  Code = <<"result = (1, 2, 3)">>,
  ?assertEqual({ok, {1, 2, 3}, undefined}, erl_py_runner:run(Code, #{})).

nested_state_calls(_Config) ->
  Code = <<
    "value = erlang.call('math', 'sqrt', [arguments['n']])\n"
    "state = value\n"
    "result = value"
  >>,
  ?assertEqual({ok, 5.0, 5.0}, erl_py_runner:run(Code, #{n => 25}, 0, 60000)).

result_state_both_set(_Config) ->
  Code = <<
    "result = arguments['x'] * 2\n"
    "state = arguments['x'] * 3"
  >>,
  ?assertEqual({ok, 20, 30}, erl_py_runner:run(Code, #{x => 10}, undefined, 60000)).

%%% +--------------------------------------------------------------+
%%% |                          Helpers                             |
%%% +--------------------------------------------------------------+

parallel_run(TotalIterations, Fun) ->
  Parent = self(),
  PIDs =
    [spawn_link(
      fun() ->
        {Code, Arguments, State} = Fun(Iteration),
        Result = erl_py_runner:run(Code, Arguments, State, 60000),
        Parent ! {self(), Result}
      end) || Iteration <- lists:seq(1, TotalIterations)],
  [receive {PID, Result} -> Result end || PID <- PIDs].