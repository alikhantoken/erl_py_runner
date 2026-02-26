# Description
An Erlang application that executes custom Python code through **Erlang ports**.  
Each Python script runs in a separate OS process, talking to an Erlang worker over stdin / stdout with length-prefixed ETF packets (messages).

**Why isolated port processes instead of NIFs?**  
NIFs run inside the BEAM process. Any bug or crash in a NIF can block the scheduler or bring down the entire Virtual Machine.  
By using **Erlang Ports**, Python runs in its own process. 
If it crashes, only that worker and its port are affected. 
You're getting **full isolation** and **controllable environment** without risking the whole Erlang Node.

**Python Runtime**  
By default the application uses **system `python3`** (interpreter on your `PATH`). 
No extra installation step is required beyond having `python3` and the `pyrlang-term` package.  
If you prefer a controlled environment, you can set `environment.python => environment` so the app creates a **virtual environment** under `priv` and installs dependencies via `priv/install.sh`.  

## Requirements

- **Erlang / OTP** - compatible with the project’s OTP version (see `rebar.config`).
- **Python 3** - `PATH` as `python3` (used by default when `environment.python` set to `system`).
- **Python Dependency:** [Pyrlang/Term](https://github.com/Pyrlang/Term) - ETF encoding / decoding between Erlang and Python. With system Python, install it yourself (e.g. `pip install pyrlang-term`). 
- **Optional (Virtual Environment Mode):** `environment.python => environment` requires `python3` and `venv` to create the virtual environment; optional `priv/python/requirements.txt` for extra pip packages.

## Project specifics
- **Pool of workers:** Configurable number of Erlang worker processes, each owning one long-lived Python process (port). Requests are distributed to idle workers; optional `max_pending` caps queued requests.
- **Protocol:** 4-byte big-endian length-prefixed ETF over stdin/stdout; max message size 10 MB. Message types: init, exec, result (ok/error), and synchronous Erlang call requests from Python.
- **Bidirectional calls:** Python scripts can call whitelisted Erlang functions synchronously via the injected `erlang` object; the worker handles the request and sends the response back through the port.

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
All settings go under the `erl_py_runner` application env in `sys.config`. The configuration is split into two groups: `environment` and `worker`.

```erlang
[
  {erl_py_runner, [
    {environment, #{
      python => system,
      requirements => "python/requirements.txt",
      runner => "python/runner.py",
      venv_dir => ".venv"
    }},
    {worker, #{
      supervisor => #{
        intensity => 5,
        period => 30
      },
      config => #{
        timeout => 60000,
        pool_size => 3,
        max_pending => infinity
      },
      modules_whitelist => #{
        erlang_modules => [math, lists, maps, binary, string],
        python_modules => [<<"math">>, <<"re">>, <<"datetime">>, <<"json">>]
      }
    }}
  ]}
].
```

| Key | Description | Default |
|-----|-------------|---------|
| `environment.python` | `system` - use system `python3` (no installation); `environment` - create venv and install deps via `priv/install.sh` | `system` |
| `environment.venv_dir` | Path to the Python virtual environment (relative to `priv/`); used only when `python => environment` | `".venv"` |
| `environment.runner` | Path to the Python runner script (relative to `priv/`) | `"python/runner.py"` |
| `environment.requirements` | Path to `requirements.txt` (relative to `priv/`); used only when `python => environment` | `"python/requirements.txt"` |
| `worker.config.pool_size` | Number of Python worker processes | `3` |
| `worker.config.timeout` | Default execution timeout in milliseconds | `60000` |
| `worker.config.max_pending` | Max queued requests when all workers are busy | `infinity` |
| `worker.modules_whitelist.erlang_modules` | Erlang modules callable from Python (`all` to allow any) | `[math, lists, maps, binary, string]` |
| `worker.modules_whitelist.python_modules` | Python modules importable in scripts (`all` to allow any) | `[<<"math">>, <<"re">>, <<"datetime">>, <<"json">>]` |

Any key omitted from the user config falls back to its default value.

## Usage
### Running Python Code
```erlang
%% Basic execution with default timeout (60 seconds)
{ok, Result, State} = erl_py_runner:run(
  <<"result = arguments['x'] + arguments['y']">>,
  #{<<"x">> => 10, <<"y">> => 20}
).
%% Result => 30

%% Execution with explicit timeout (10 seconds)
{ok, Result, State} = erl_py_runner:run(
  <<"result = arguments['x'] + arguments['y']">>,
  #{<<"x">> => 10, <<"y">> => 20},
  10000
).
```

**`erl_py_runner:run/2,3`**
```erlang
-spec run(Code :: binary(), Arguments :: term(), State :: term()) -> {ok, term(), term()} | {error, term()}.
-spec run(Code :: binary(), Arguments :: term(), State :: term(), Timeout :: timeout()) -> {ok, term(), term()} | {error, term()}.
```

- `Code` - binary string of Python source code to execute.
- `Arguments` - any Erlang term, available inside the script as the `arguments` variable.
- `State` - any term which can be used to hold state between executions.
- `Timeout` - optional, defaults to `60000` ms.

**Output**
```erlang
{ok, Result}    %% Successful code execution, `Result` is the value of the `result` variable
{error, Reason} %% Python exception, timeout or port failure
```

**`erl_py_runner:restart/0`** - Stops and restarts the entire application.

### Inside the Python Script
Three variables are injected into the execution scope:
- **`arguments`** - the Erlang term sent as the second argument to `run/2,3`.
- **`result`** - set this to the value you want returned (defaults to `None`).
- **`erlang`** - object for calling Erlang functions back synchronously.

```python
import math
result = math.sqrt(arguments['value'])
```

Dangerous builtins (`eval`, `exec`, `open`, `compile`, `__import__`, `input`, `exit`, `quit`, `breakpoint`) are removed from the execution scope. Imports are restricted to the configured `python_modules` whitelist.

### Calling Erlang from Python
Use the `erlang` object to call whitelisted Erlang functions synchronously:
```python
value = erlang.call("module", "function", [arg1, arg2, ...])
```

The third argument is a **list of arguments** passed to `erlang:apply(Module, Function, Arguments)`. It may raise `ErlangError`, `ErlangTimeoutError`, or `ErlangPortError`.

```erlang
%% Python calling lists:sort/1
{ok, Result} = erl_py_runner:run(
  <<"result = erlang.call('lists', 'sort', [[9, 1, 5]])">>,
  #{}
).
%% Result => [1, 5, 9]
```

## How It Works
1. **Python mode** - **Default: `environment.python => system`** - the runner uses **system `python3`** from your PATH; no venv or install step. Ensure `python3` and `pyrlang-term` are available. **Optional: `environment.python => environment`** - a virtual environment is created at `priv/.venv` and dependencies are installed via `priv/install.sh`; you can add extra packages in `priv/python/requirements.txt`.
2. A pool of `pool_size` Python processes are spawned, each connected to an Erlang worker via a port through `stdin` & `stdout`.
3. Calling `erl_py_runner:run/2,3` dispatches the request to an idle worker. If all are busy, the caller blocks until one frees up or the timeout expires.
4. The worker sends code & arguments to Python, which executes it in `exec()` and returns the result.
5. During execution, Python can call Erlang functions by sending call requests back synchronously through the port.

## Message Protocol
Workers and Python processes communicate via **4-byte big-endian length-prefixed ETF** messages over stdin & stdout. Conversion between Erlang terms and Python terms is achieved via https://github.com/Pyrlang/Term.

```
┌──────────────────┬─────────────────────┐
│ 4 bytes (uint32) │    N bytes (ETF)    │
│  payload length  │    encoded term     │
└──────────────────┴─────────────────────┘
```
Max message size: **10 MB**. 

### Message Types

All messages are **ETF-encoded tuples**.

| Direction | Type | Format |
|-----------|------|--------|
| Erlang → Python | Initialization | `{init, AllowedPythonModules}` |
| Python → Erlang | Init acknowledgement | `ok` |
| Erlang → Python | Code Execution | `{exec, Code, Arguments}` |
| Python → Erlang | Success Result | `{ok, Result}` |
| Python → Erlang | Error Result | `{error, ErrorString}` |
| Python → Erlang | Call Request | `{call, RequestID, Module, Function, Arguments}` |
| Erlang → Python | Call Response | `{call_reply, RequestID, {ok, Result} \| {error, Error}}` |

## License

MIT see [LICENSE](LICENSE).
