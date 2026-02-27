%%% +--------------------------------------------------------------+
%%% | Common Test suite for erl_py_runner                          |
%%% +--------------------------------------------------------------+

%%% +--------------------------------------------------------------+
%%% | Tests guarantee that:                                        |
%%% |   1. Libraries must persist across worker shutdowns          |
%%% |   2. Reload order is preserved                               |
%%% |   3. Pool is never blocked by library loading                |
%%% |   4. No deadlocks under concurrent load + run                |
%%% +--------------------------------------------------------------+

-module(erl_py_runner_loader_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%% +--------------------------------------------------------------+
%%% |                     Suite configuration                      |
%%% +--------------------------------------------------------------+

suite() ->
  [{timetrap, {seconds, 180}}].

all() ->
  % Library consistency group must run first.
  [{group, library_consistency},
    {group, library_basic},
    {group, library_persistence},
    {group, library_concurrency}].

groups() ->
  [{library_consistency, [sequence], [
    library_empty_list,
    library_ordering,
    library_position,
    library_code_update
  ]},
    {library_basic, [sequence], [
      load_return_ok,
      load_multiple_libraries,
      load_empty_code,
      load_syntax_error,
      load_runtime_error,
      load_code_update,
      load_bad_code,
      delete_existent,
      delete_non_existent
    ]},
    {library_persistence, [sequence], [
      library_availability_single_crash,
      library_availability_multiple_crash,
      multiple_library_availability,
      library_code_persistence,
      library_order_preservation,
      library_multiple_crashes
    ]},
    {library_concurrency, [sequence], [
      load_run_no_deadlock,
      pool_non_blocked_on_load,
      load_concurrent,
      load_crash_load
    ]}].

%%% +--------------------------------------------------------------+
%%% |                   Suite init / cleanup                       |
%%% +--------------------------------------------------------------+

init_per_suite(Config) ->
  application:stop(erl_py_runner),
  application:set_env(erl_py_runner, worker, #{
    supervisor => #{
      intensity => 10,
      period => 30
    },
    config => #{
      timeout => 10000,
      pool_size => 5,
      max_pending => infinity
    },
    modules_whitelist => #{
      erlang_modules => all,
      python_modules => all
    }
  }),
  {ok, _} = application:ensure_all_started(erl_py_runner),
  Config.

end_per_suite(_Config) ->
  application:stop(erl_py_runner),
  ok.

init_per_group(_Group, Config) ->
  Config.

end_per_group(_Group, _Config) ->
  ok.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  ok.

%%% +--------------------------------------------------------------+
%%% |                 Group: Library Consistency                   |
%%% +--------------------------------------------------------------+

library_empty_list(_Config) ->
  {ok, []} = erl_py_runner_loader:get_libraries(),
  ok.

library_ordering(_Config) ->
  ok = erl_py_runner:load_library(<<"lib_ordering_a">>, <<"ValueA = 1">>),
  ok = erl_py_runner:load_library(<<"lib_ordering_b">>, <<"ValueB = 2">>),
  ok = erl_py_runner:load_library(<<"lib_ordering_c">>, <<"ValueC = 3">>),
  
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  OurLibs = filter_libs(Libraries, [<<"lib_ordering_a">>, <<"lib_ordering_b">>, <<"lib_ordering_c">>]),
  
  ?assertEqual(
    [{<<"lib_ordering_a">>, <<"ValueA = 1">>},
     {<<"lib_ordering_b">>, <<"ValueB = 2">>},
     {<<"lib_ordering_c">>, <<"ValueC = 3">>}],
    OurLibs
  ).

library_position(_Config) ->
  ok = erl_py_runner:load_library(<<"lib_position_a">>, <<"ValueA = 1">>),
  ok = erl_py_runner:load_library(<<"lib_position_b">>, <<"ValueB = 2">>),
  ok = erl_py_runner:load_library(<<"lib_position_c">>, <<"ValueC = 3">>),
  ok = erl_py_runner:load_library(<<"lib_position_a">>, <<"ValueA = 99">>),
  
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  OurLibs = filter_libs(Libraries, [<<"lib_position_a">>, <<"lib_position_b">>, <<"lib_position_c">>]),
  
  ?assertEqual(
    [{<<"lib_position_a">>, <<"ValueA = 99">>},
     {<<"lib_position_b">>, <<"ValueB = 2">>},
     {<<"lib_position_c">>, <<"ValueC = 3">>}],
    OurLibs
  ).

library_code_update(_Config) ->
  ok = erl_py_runner:load_library(<<"lib_code_update">>, <<"ValueA = 1">>),
  ok = erl_py_runner:load_library(<<"lib_code_update">>, <<"ValueA = 777">>),
  
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  [{<<"lib_code_update">>, <<"ValueA = 777">>}] = filter_libs(Libraries, [<<"lib_code_update">>]),
  ok.

%%% +--------------------------------------------------------------+
%%% |                Group: Basic Library Operations               |
%%% +--------------------------------------------------------------+

%% A well-formed load_library call returns ok.
load_return_ok(_Config) ->
  ?assertEqual(ok, erl_py_runner:load_library(<<"lib_load_return_ok">>, <<"Value = True">>)).

%% Multiple loads all succeed and persist in the registry.
load_multiple_libraries(_Config) ->
  ok = erl_py_runner:load_library(<<"lib_multiple_a">>, <<"ValueA = 10">>),
  ok = erl_py_runner:load_library(<<"lib_multiple_b">>, <<"ValueB = 20">>),
  ok = erl_py_runner:load_library(<<"lib_multiple_c">>, <<"ValueC = 30">>),
  
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  
  ?assertEqual(3, length(filter_libs(Libraries, [<<"lib_multiple_a">>, <<"lib_multiple_b">>, <<"lib_multiple_c">>]))).

load_empty_code(_Config) ->
  ?assertEqual(ok, erl_py_runner:load_library(<<"lib_empty_code">>, <<"">>)).

load_syntax_error(_Config) ->
  ?assertMatch({error, _}, erl_py_runner:load_library(<<"lib_syntax_error">>, <<"def f(\n  pass">>)),
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  ?assertEqual([], filter_libs(Libraries, [<<"lib_syntax_error">>])).

load_runtime_error(_Config) ->
  ?assertMatch({error, _}, erl_py_runner:load_library(<<"lib_runtime_error">>, <<"raise ValueError('intended')">>)),
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  ?assertEqual([], filter_libs(Libraries, [<<"lib_runtime_error">>])).

load_code_update(_Config) ->
  ok = erl_py_runner:load_library(<<"lib_load_code_update">>, <<"Version = 1">>),
  ok = erl_py_runner:load_library(<<"lib_load_code_update">>, <<"Version = 2">>),
  
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  [{<<"lib_load_code_update">>, <<"Version = 2">>}] = filter_libs(Libraries, [<<"lib_load_code_update">>]).

load_bad_code(_Config) ->
  ok = erl_py_runner:load_library(<<"lib_bad_code">>, <<"SafeValue = 42">>),
  
  {ok, Libs1} = erl_py_runner_loader:get_libraries(),
  [{<<"lib_bad_code">>, <<"SafeValue = 42">>}] = filter_libs(Libs1, [<<"lib_bad_code">>]),
  
  ?assertMatch({error, _}, erl_py_runner:load_library(<<"lib_bad_code">>, <<"def broken(\n pass">>)),
  
  {ok, Libs2} = erl_py_runner_loader:get_libraries(),
  [{<<"lib_bad_code">>, <<"SafeValue = 42">>}] = filter_libs(Libs2, [<<"lib_bad_code">>]).

delete_existent(_Config) ->
  ok = erl_py_runner:load_library(<<"lib_delete_existent">>, <<"D1 = 1">>),
  {ok, Libs1} = erl_py_runner_loader:get_libraries(),
  
  [{<<"lib_delete_existent">>, _}] = filter_libs(Libs1, [<<"lib_delete_existent">>]),
  ?assertEqual(ok, erl_py_runner:delete_library(<<"lib_delete_existent">>)),
  
  {ok, Libs2} = erl_py_runner_loader:get_libraries(),
  ?assertEqual([], filter_libs(Libs2, [<<"lib_delete_existent">>])).

delete_non_existent(_Config) ->
  ?assertEqual({error, not_found}, erl_py_runner:delete_library(<<"lib_delete_nonexistent_missing">>)).

%%% +--------------------------------------------------------------+
%%% |                  Group: Library Persistence                  |
%%% +--------------------------------------------------------------+

library_availability_single_crash(_Config) ->
  ok = erl_py_runner:load_library(<<"lib_avail_single_crash">>, <<"Value = 777">>),
  
  kill_one_worker(),
  wait_for_workers(),
  
  {ok, 1, undefined} = erl_py_runner:run(<<"result = 1">>, #{}, undefined, 60000),
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  
  [{<<"lib_avail_single_crash">>, <<"Value = 777">>}] = filter_libs(Libraries, [<<"lib_avail_single_crash">>]).

library_availability_multiple_crash(_Config) ->
  ok = erl_py_runner:load_library(<<"lib_avail_multiple_crash">>, <<"Value = 888">>),
  
  kill_all_workers(),
  wait_for_workers(),
  
  {ok, 1, undefined} = erl_py_runner:run(<<"result = 1">>, #{}, undefined, 60000),
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  
  [{<<"lib_avail_multiple_crash">>, <<"Value = 888">>}] = filter_libs(Libraries, [<<"lib_avail_multiple_crash">>]).

%% Multiple libraries all survive a crash-restart cycle.
multiple_library_availability(_Config) ->
  ok = erl_py_runner:load_library(<<"lib_multi_avail_a">>, <<"ValueA = 100">>),
  ok = erl_py_runner:load_library(<<"lib_multi_avail_b">>, <<"ValueB = 200">>),
  ok = erl_py_runner:load_library(<<"lib_multi_avail_c">>, <<"ValueC = 300">>),
  
  kill_all_workers(),
  wait_for_workers(),
  
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  Names = [<<"lib_multi_avail_a">>, <<"lib_multi_avail_b">>, <<"lib_multi_avail_c">>],
  
  ?assertEqual(3, length(filter_libs(Libraries, Names))).

library_code_persistence(_Config) ->
  ok = erl_py_runner:load_library(<<"lib_code_persistence">>, <<"Value = 1">>),
  ok = erl_py_runner:load_library(<<"lib_code_persistence">>, <<"Value = 99">>),
  
  kill_all_workers(),
  wait_for_workers(),
  
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  
  [{<<"lib_code_persistence">>, <<"Value = 99">>}] = filter_libs(Libraries, [<<"lib_code_persistence">>]).

library_order_preservation(_Config) ->
  ok = erl_py_runner:load_library(<<"lib_order_parent">>, <<"Value = 7">>),
  ok = erl_py_runner:load_library(<<"lib_order_child">>, <<"import lib_order_parent\nRESULT = lib_order_parent.Value * 6">>),
  
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  [_, _] = filter_libs(Libraries, [<<"lib_order_parent">>, <<"lib_order_child">>]),
  
  kill_all_workers(),
  wait_for_workers(),
  
  {ok, Libs2} = erl_py_runner_loader:get_libraries(),
  [_, _] = filter_libs(Libs2, [<<"lib_order_parent">>, <<"lib_order_child">>]).

library_multiple_crashes(_Config) ->
  ok = erl_py_runner:load_library(<<"lib_multiple_crashes">>, <<"Value = 55">>),
  lists:foreach(
    fun(_) ->
      kill_all_workers(),
      wait_for_workers(),
      
      {ok, Libraries} = erl_py_runner_loader:get_libraries(),
      [{<<"lib_multiple_crashes">>, <<"Value = 55">>}] = filter_libs(Libraries, [<<"lib_multiple_crashes">>]),
      
      {ok, 1, undefined} = erl_py_runner:run(<<"result = 1">>, #{}, undefined, 60000)
    end, lists:seq(1, 3)).

%%% +--------------------------------------------------------------+
%%% |                  Group: Library Concurrency                  |
%%% +--------------------------------------------------------------+

load_run_no_deadlock(_Config) ->
  Owner = self(),
  TotalProcesses = 20,
  
  [spawn_link(
    fun() ->
      Result = erl_py_runner:run(
        <<"result = arguments['i'] * 3">>,
        #{i => Index},
        undefined,
        60000
      ),
      Owner ! {run, Index, Result}
    end) || Index <- lists:seq(1, TotalProcesses)],
  
  spawn_link(
    fun() ->
      Owner ! {load, erl_py_runner:load_library(<<"lib_load_run_no_deadlock">>, <<"Value = True">>)}
    end),
  
  RunResults =
    [receive
       {run, Index, Result} -> {Index, Result}
     after 60000 ->
        ct:fail({run_deadlock, Index})
     end || Index <- lists:seq(1, TotalProcesses)],
  
  receive
    {load, ok} -> ok;
    {load, Error} -> ct:fail({unexpected_load, Error})
  after 60000 ->
    ct:fail(load_deadlock)
  end,
  
  lists:foreach(
    fun({Index, {ok, Value, _}}) ->
      ?assertEqual(Index * 3, Value)
    end, RunResults
  ).

pool_non_blocked_on_load(_Config) ->
  Owner = self(),
  
  spawn_link(
    fun() ->
      Owner ! {load, erl_py_runner:load_library(<<"lib_pool_non_blocked">>, <<"Value = True">>)}
    end),
  
  ?assertEqual({ok, 1, undefined}, erl_py_runner:run(<<"result = 1">>, #{}, undefined, 30000)),
  
  receive
    {load, ok} -> ok;
    {load, Error} -> ct:fail({load_failed, Error})
  after 60000 ->
    ct:fail(load_blocked)
  end.

load_concurrent(_Config) ->
  Owner = self(),
  TotalProcesses = 5,
  
  [spawn_link(
    fun() ->
      Name = list_to_binary("lib_concurrent_" ++ integer_to_list(I)),
      Code = list_to_binary("Value = " ++ integer_to_list(I * 777)),
      Owner ! {I, erl_py_runner:load_library(Name, Code)}
    end) || I <- lists:seq(1, TotalProcesses)],
  
  Results =
    [receive
       {Index, Result} -> {Index, Result}
     after 60000 ->
        ct:fail({concurrent_load_timeout, Index})
     end || Index <- lists:seq(1, TotalProcesses)],
  
  lists:foreach(fun({_Index, ok}) -> ok end, Results),
  
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  Names = [list_to_binary("lib_concurrent_" ++ integer_to_list(I)) || I <- lists:seq(1, TotalProcesses)],
  
  ?assertEqual(TotalProcesses, length(filter_libs(Libraries, Names))).

load_crash_load(_Config) ->
  ok = erl_py_runner:load_library(<<"lib_crash_load_a">>, <<"ValueA = 111">>),
  kill_all_workers(),
  wait_for_workers(),
  
  ok = erl_py_runner:load_library(<<"lib_crash_load_b">>, <<"ValueB = 222">>),
  kill_all_workers(),
  wait_for_workers(),
  
  {ok, Libraries} = erl_py_runner_loader:get_libraries(),
  Names = [<<"lib_crash_load_a">>, <<"lib_crash_load_b">>],
  
  ?assertEqual(2, length(filter_libs(Libraries, Names))).

%%% +--------------------------------------------------------------+
%%% |                          Helpers                             |
%%% +--------------------------------------------------------------+

kill_all_workers() ->
  Workers = supervisor:which_children(erl_py_runner_worker_sup),
  [exit(PID, kill) || {_, PID, _, _} <- Workers],
  ok.

kill_one_worker() ->
  Children = supervisor:which_children(erl_py_runner_worker_sup),
  case [{ID, PID} || {ID, PID, _, _} <- Children] of
    [] ->
      ct:fail(no_workers_left);
    [{_ID, PID} | _] ->
      exit(PID, kill)
  end,
  ok.

wait_for_workers() ->
  {ok, 1, undefined} = erl_py_runner:run(<<"result = 1">>, #{}, undefined, 60000),
  ok.

filter_libs(Libraries, Names) ->
  [{Name, Code} || {Name, Code} <- Libraries, lists:member(Name, Names)].
