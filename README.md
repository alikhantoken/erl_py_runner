# Description
An Erlang application that executes custom Python code through Erlang ports.

## Installation
Add to your `rebar.config`:
```erlang
{deps, [
  {erl_py_runner, {git, "https://github.com/alikhantoken/erl_py_runner.git", {branch, "main"}}}
]}.
```
Add `erl_py_runner` to your `.app.src` applications list:
```erlang
{applications, [..., erl_py_runner]}.
```

### Running locally (testing purposes)
```bash
git clone https://github.com/alikhantoken/erl_py_runner.git
cd erl_py_runner
./rebar3 compile
./test_shell
```

## Configuration
All settings go under the `erl_py_runner` application env in `sys.config`.

## Usage
### Running Python Code
```erlang
%% Basic script execution call without timeout
{ok, Response} = erl_py_runner:run(#{
  code => <<"result = arguments['x'] + arguments['y']">>,
  arguments => #{<<"x">> => 10, <<"y">> => 20}
}).
%% Expected response: #{status => ok, result => 30}

%% Basic script execution call with explicit timeout
{ok, Response} = erl_py_runner:run(#{
  code => <<"result = arguments['x'] + arguments['y']">>,
  arguments => #{<<"x">> => 10, <<"y">> => 20}
}, 10000).
```

**Input** is a map with two keys:
- `code` - binary string of `python` source code to execute.
- `arguments` - any type of data that needs to be available inside the script as the `arguments` variable.

**Output**
```erlang
{ok, #{status := ok, result := Result}}     %% Successful code execution
{ok, #{status := error, error := Reason}}   %% Successful code execution, but with raised python exception
{error, timeout}                            %% Code execution failed by timeout
{error, Reason}                             %% Code execution failed by some other reasons
```

### Inside the Python Script
Three variables are injected into the scope of python code execution:
- **`arguments`** - the term sent from erlang.
- **`result`** - set this to the value you want returned (defaults to `None`).
- **`erlang`** - object for calling erlang functions.

```python
import math
result = math.sqrt(arguments['value'])
```

### Calling Erlang from Python
Use the `erlang` object to call whitelisted erlang functions synchronously:
```python
value = erlang.call("my_module", "my_function", AnyErlangCompatibleTerm)
```

The Erlang side invokes `Module:Function(Args)` where `Args` is a variable received from Python call request. It may 
raise `ErlangError`, `ErlangTimeoutError`, `ErlangPortError`.

```erlang
%% Example: Python calling lists:sort/1
{ok, Response} = erl_py_runner:run(#{
  code => <<"result = erlang.call('lists', 'sort', [9, 1, 5])">>,
  arguments => #{}
}).
```

## How It Works
1. On startup, a Python virtual environment is created and dependencies are installed automatically. If any other
dependencies are required, they can be added to `requirements.txt`.
2. A pool of `pool_size` python processes are spawned, each connected to an erlang worker via a port through `stdin` & 
`stdout`.
3. Calling `erl_py_runner:run/1,2` dispatches the request to an idle worker. If all are busy, the caller blocks until one frees up or the timeout expires.
4. The worker sends code & arguments to Python, which executes it in `exec()` and returns the result.
5. During execution, python can use erlang functions by initiating call requests back synchronously to erlang port.

## Message Protocol
Workers and Python processes communicate via **4-byte big-endian length-prefixed ETF** messages over stdin & stdout. Conversion between erlang terms and python terms is achieved via https://github.com/Pyrlang/Term.

```
┌──────────────────┬─────────────────────┐
│ 4 bytes (uint32) │    N bytes (ETF)    │
│  payload length  │    encoded term     │
└──────────────────┴─────────────────────┘
```
Max message size: **10 MB**. 

### Message Types

| Direction        | Type                           | Format |
|------------------|--------------------------------|---|
| Erlang to Python | Initialization                 | `#{type => init, allowed_modules => [...]}` |
| Python to  Erlang  | Initialization acknowledgement | `#{status => ok}` |
| Erlang to  Python  | Code Execution                 | `#{code => Code, arguments => Args}` |
| Python to  Erlang  | Code Result                    | `#{status => ok, result => Value}` or `#{status => error, error => Reason}` |
| Python to  Erlang  | Call Request                   | `#{type => call_request, request_id => Id, module => M, function => F, args => A}` |
| Erlang to  Python  | Call Response                  | `#{type => call_response, request_id => Id, status => ok/error, result/error => ...}` |

## License

MIT see [LICENSE](LICENSE).
