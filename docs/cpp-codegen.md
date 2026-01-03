# C++ Code Generation

## Mapping SystemVerilog to C++

| SystemVerilog     | C++                                          |
| ----------------- | -------------------------------------------- |
| `module`          | class inheriting from `Module`               |
| `parameter`       | constructor argument                         |
| `parameter type`  | template parameter                           |
| variables         | member variables                             |
| `initial`         | coroutine method returning `Task`            |
| `always @(...)`   | coroutine method returning `Task`            |
| `always_comb`     | regular method, re-evaluated on input change |
| `always_ff`       | coroutine with clock edge sensitivity        |
| `generate for/if` | constructor logic                            |
| non-blocking `<=` | deferred update via SDK                      |
| `#delay`          | `co_await delay(n)`                          |
| `@(posedge clk)`  | `co_await event(clk, Edge::Posedge)`         |

## Coroutine Model

`initial` and `always` blocks are coroutines that can suspend on timing or events.

```
Task initial_0() {
  a = 10;
  co_await delay(5);
  b = 20;
}
```

Coroutines suspend on:

- `delay(n)` - resume after n time units
- `event(signal, edge)` - resume on signal edge

## Scheduler

Simple priority queue ordered by simulation time:

1. Run all initial coroutines to first suspend
2. Collect modified variables
3. Wake coroutines sensitive to those variables
4. When nothing ready, advance time to next scheduled event
5. Repeat until done

No IO runtime needed (Asio, etc). Simulation is compute-bound, not IO-bound.
All waiting is logical (simulation time), not real time.

## Generated Code Structure

```
module Test #(parameter WIDTH = 8) (input clk);
  int a, b;
  initial begin
    a = 10;
    #5;
    b = 20;
  end
endmodule
```

Becomes:

```
class Test : public Module {
  int a, b;

  Test(Signal& clk, int WIDTH = 8) : clk_(clk), WIDTH_(WIDTH) {
    register_initial(&Test::initial_0);
  }

  Task initial_0() {
    a = 10;
    co_await delay(5);
    b = 20;
  }
};
```

## SDK Interface

Core types:

- `Task` - coroutine return type, wraps coroutine handle
- `Module` - base class for generated modules
- `Scheduler` - runs simulation, manages time and events
- `Signal<T>` - observable value with change detection

Key APIs:

- `delay(n)` - awaitable, resumes after n time units
- `event(signal, edge)` - awaitable, resumes on signal edge
- `nba_assign(target, value)` - schedule non-blocking assignment

Scheduler loop (pseudocode):

```
while not done:
  run ready coroutines until all suspend
  apply NBA updates
  check sensitivities, wake triggered coroutines
  if nothing ready:
    advance time to next scheduled event
```
