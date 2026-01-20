# Runtime (Simulation Engine)

The runtime provides scheduling and execution infrastructure for SystemVerilog simulation. Both the MIR interpreter and LLVM backend use the same runtime API.

## Design Goals

1. **Backend-agnostic API** - Same interface for MIR interpreter and LLVM codegen
2. **IEEE 1800 compliant scheduling** - Stratified event scheduler (Active -> Inactive -> NBA)
3. **Process suspension model** - Processes yield to engine, engine resumes them

## Architecture

```
+------------------+     +------------------+
|  MIR Interpreter |     |  LLVM Codegen    |
| (RunUntilSuspend)|     |  (extern "C")    |
+--------+---------+     +--------+---------+
         |                        |
         v                        v
+------------------------------------------+
|           Runtime Engine API             |
|  ScheduleInitial, Delay, Subscribe, Run  |
+------------------------------------------+
```

## Suspension Model

Processes run until they hit a scheduling terminator, then yield control to the engine. The interpreter returns a `SuspendReason` variant:

| Reason            | MIR Terminator       | Engine Action                                |
| ----------------- | -------------------- | -------------------------------------------- |
| `SuspendFinished` | `kFinish`, `kReturn` | Remove from scheduling                       |
| `SuspendDelay`    | `kDelay`             | Add to delay queue at `current_time + ticks` |
| `SuspendWait`     | `kWait`              | Add to signal's waiter list                  |
| `SuspendRepeat`   | `kRepeat`            | Reschedule to next time slot                 |

## IEEE 1800 Regions

The engine implements stratified scheduling per IEEE 1800-2023 Section 4.4:

```
Time Slot N:
  +-> Active    (blocking assignments, $display)
  |   Inactive  (#0 delays, same time slot)
  +-- NBA       (nonblocking assignment commits)
      [repeat until all empty]

Advance to Time Slot N+1
```

Key behaviors:

- Active region loops until empty before moving to Inactive
- Inactive events move to Active (not executed directly)
- NBA commits all deferred writes, may wake triggered processes
- Time advances only when all regions are empty

## Engine API

**Process lifecycle:**

- `ScheduleInitial(handle)` - Schedule process to start at time 0
- `Delay(handle, resume, ticks)` - Reschedule after N ticks
- `DelayZero(handle, resume)` - Reschedule to inactive region (same time)
- `Subscribe(handle, resume, signal, edge)` - Wait for signal edge

**Simulation control:**

- `Run(max_time)` - Execute until completion or time limit
- `CurrentTime()` - Get current simulation time

**Edge types:** `kPosedge`, `kNegedge`, `kAnyChange`

## LLVM Integration

LLVM-generated code calls the runtime via C ABI:

```cpp
// Generated code calls these (extern "C" or C++ with stable ABI)
void lyra_delay(ProcessState* state, uint64_t ticks);
void lyra_subscribe(ProcessState* state, SignalId signal, EdgeKind edge);
```

Process state is passed by pointer; generated code stores/restores locals to the state struct at suspension points.

## Legacy Reference

The legacy Lyra codebase (`lyra-2`) has a complete simulation engine:

| Component          | Location                            | Relevant For                             |
| ------------------ | ----------------------------------- | ---------------------------------------- |
| `Scheduler`        | `sdk/scheduler.hpp`                 | Stratified regions, coroutine awaitables |
| `SimulationRunner` | `interpreter/simulation_runner.cpp` | Event loop, region execution             |
| `TriggerManager`   | `interpreter/trigger_manager.cpp`   | Edge detection, waiter management        |

Key patterns from legacy:

- **Centralized ownership**: SimulationRunner owns ProcessFrames; queues hold handles
- **Effect recording**: Processes record side effects; scheduler processes them after
- **Two-phase NBA**: Active phase records updates, NBA phase commits

## Not In Scope

- VPI/DPI interface
- Multi-threaded simulation
- Coverage collection
