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
  FlushSignalUpdates after Active/Inactive
  FlushSignalUpdates after NBA
  if next-delta queue not empty: repeat Active/Inactive/NBA in Time Slot N

Advance to Time Slot N+1
```

Key behaviors:

- Active region loops until empty before moving to Inactive
- Inactive events move to Active (not executed directly)
- FlushSignalUpdates runs after Active/Inactive and after NBA
- NBA commits all deferred writes, may wake triggered processes
- Time advances only when no more delta work remains in the current slot

## Engine API

**Process lifecycle:**

- `ScheduleInitial(handle)` - Schedule process to start at time 0
- `Delay(handle, resume, ticks)` - Reschedule after N ticks
- `DelayZero(handle, resume)` - Reschedule to inactive region (same time)
- `Subscribe(handle, resume, signal, edge)` - Wait for signal edge
- `Subscribe(handle, resume, signal, edge, byte_offset, byte_size)` - Wait on an observation sub-range

**Simulation control:**

- `Run(max_time)` - Execute until completion or time limit
- `CurrentTime()` - Get current simulation time

**Edge types:** `kPosedge`, `kNegedge`, `kAnyChange`
Current runtime constraint: edge subscriptions use a single sampled bit (`byte_offset=0`, `byte_size=1`, bit 0).

## LLVM Integration

LLVM-generated code calls the runtime via C ABI:

```cpp
// Generated code suspends processes through these C ABI helpers.
void LyraSuspendDelay(void* state, uint64_t ticks, uint32_t resume_block);
void LyraSuspendWait(
    void* state, uint32_t resume_block, const void* triggers,
    uint32_t num_triggers);
```

Process state is passed by pointer; generated code stores/restores locals to the state struct at suspension points.

## Change Propagation

The engine tracks which slots changed (dirty ranges) and which bytes each process observes (observation ranges), then uses range overlap filtering and snapshot comparison to determine wakeups. See [change-propagation.md](change-propagation.md) for the full pipeline, contracts, and byte-range precision.

## Not In Scope

- VPI/DPI interface
- Multi-threaded simulation
- Coverage collection
