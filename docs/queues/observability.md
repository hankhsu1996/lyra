# Observability

Debugging, tracing, and inspection tooling for simulation correctness and scheduling efficiency.

## North Star

When a design produces wrong output or converges slowly, the developer can quickly identify the root cause without manual $display bisection.

Three distinct layers, each answering different debugging questions:

- **Scheduler observability** -- which processes wake, why, and whether the wake was productive
- **Value-change observability** -- which signals changed, when, and to what value
- **Waveform observability** -- full signal history viewable in GTKWave for side-by-side comparison

## Principles

- Structured data first, presentation second
- Low overhead when disabled
- Selective / scoped tracing (usable on large designs)

## Current Status

Activation trace (O1) is implemented behind `--trace-activations`. Aggregate counters exist for runtime stats and a point-in-time SIGUSR1 snapshot. Two tiers: core stats (`-vv`, always collected) cover activation shape, fixpoint iterations, and NBA; detailed stats (`-vvv`, opt-in) cover per-element connection, comb, subscription, and wakeup accounting.

Common failure modes that lack tooling today:

- Wrong output, unknown cause
- Hard-to-localize divergence vs reference simulator

## Implemented

### O1: Activation trace

**Status:** Implemented.

**Flag:** `--trace-activations`

Bounded ring buffer (8192 events) of structured activation events on Engine, with live text output to stderr.

Two event kinds:

- **WakeEvent** -- time, delta, process_id, cause (edge/change/container/delay/delay_zero/initial/repeat), trigger slot, resume block
- **RunEvent** -- time, delta, process_id, distinct design slots newly dirtied, plus causal wake metadata (cause, trigger slot)

Wake events are emitted at one semantic point: when an activation becomes runnable (enters the active region), not at producer-side queue insertion. This ensures correct time/delta for all wake causes, including delayed and next-delta events. For subscription-driven wakes, dedup happens at enqueue time; the trace shows what the scheduler actually decided, not every subscription that fired.

Dirty counts (`slots_dirtied`) measure design-slot productivity only: distinct slots dirtied through `MarkSlotDirty` / `MarkDirtyRange`, deduplicated at slot granularity via generation counters within one activation. External/container heap-relative dirty notifications are excluded by design.

`--trace-activations` enables both the retained ring buffer and live stderr output together.

Output format (one line per event, machine-parsable prefix):

```
[act] t=100 d=0 wake always_ff@top.u_reg cause=edge resume=5 slot=3
[act] t=100 d=0 run  always_ff@top.u_reg dirtied=2 resume=5
```

## Architecture

The signal-change observability stack shares a common architecture. The center is a **scoped signal-change stream**, not VCD and not engine-owned old-value storage.

### Layering

```
Runtime core            -- identify dirty traced slots, snapshot, emit
Trace metadata          -- signal identity, names, widths, formatting class
Trace selection         -- per-slot enable/disable, owned by Engine
TraceManager            -- sink fanout, dispatch
Sinks                   -- text formatting, VCD serialization, sink-local state
```

### Architectural rules

- Runtime emits new values only. No engine-owned old-value storage.
- Sinks own presentation-specific state (e.g., last-emitted value for `old -> new` display).
- Trace metadata (`TraceSignalMetaRegistry`) is separate from `SlotMetaRegistry` (storage layout vs presentation identity).
- Selection (`TraceSelectionRegistry`) is owned by `Engine`, not `TraceManager`. Producer-side filtering in `FlushDirtySlotsToTrace` skips deselected slots before snapshotting.
- `TraceManager` is a streaming dispatcher only -- no selection policy, no event retention.

### Event stream semantics

`TimeAdvance` is an end-of-time-slot committed context marker, emitted once per time slot at flush time. It carries the final delta cycle count after convergence, not the delta at which any particular value was last written. `TimeAdvance` is emitted for every time slot, including slots with no dirty signals, so sinks can track time progression without value changes.

`ValueChange` events follow immediately after the `TimeAdvance` for their time slot. They represent the final committed state after all delta cycles complete.

### Components

- **`TraceManager`** -- Streaming fanout dispatcher with pluggable sinks. A built-in `SummaryTraceSink` provides `--trace-summary` output. External sinks added via `AddSink()` share the same dispatch path.
- **`TraceSignalMetaRegistry`** -- Dense, immutable slot-to-signal-identity table built at compile time. Each entry has hierarchical name, bit width, and trace kind. Engine owns the registry and passes a non-owning pointer to `TraceManager` for sink formatting context.
- **`TraceSelectionRegistry`** -- Dense per-slot enable/disable, owned by `Engine`. Default is all-selected when trace metadata is present. Future `$dumpvars(level, scope)` and CLI scoping compile down to selection mutations.
- **`TextTraceSink`** -- Consumes `TimeAdvance` + `ValueChange` and emits compact one-line value-change output via `--trace-signals[=FILE]`.
- **`FlushDirtySlotsToTrace`** -- Producer path: iterates dirty slots at end of time slot, snapshots current values via `SnapshotSlotValue`, emits `ValueChange(slot_id, new_value)`.

### CLI flags

Every `--trace-*` flag is self-contained and output-describing. `TraceManager` is enabled implicitly by any trace-output flag.

- `--trace-summary` -- summary output (gated by `kEnableTraceSummary`)
- `--trace-signals[=FILE]` -- text signal trace to stdout or file (gated by `kEnableSignalTrace`)
- `--trace-activations` -- scheduler activation trace (separate from value-change trace)

### Text signal trace output

End-of-time-slot committed value trace. One line per value change, time and final delta from the committed flush:

```
t=0 d=0 top.clk = 1'b0
t=100 d=3 top.data = 8'hff
```

Value rendering: 1-bit as `1'b0`/`1'b1`, wider packed as `N'h<hex>`, strings as quoted. No old-value state. Richer rendering requires future `TraceValue` model expansion, not sink-side metadata coupling.

## Active Gaps

### Phase 5: O2 -- VCD waveform sink

**Question:** What is the full signal history, viewable alongside a reference simulator?

A `VcdTraceSink` implementing `TraceSink`. Header emission from trace metadata, initial dump for selected signals, subsequent value changes from the existing stream. VCD-specific state (identifier allocation, file format rules) stays in the sink.

**First milestone:** `$dumpfile` / `$dumpvars` producing valid VCD readable in GTKWave.

Standard RTL debugging workflow. Enables side-by-side comparison with Verilator traces.

### Phase 6: Control surface

User-facing options: `--trace-vcd=FILE`, later `$dumpfile` / `$dumpvars` SystemVerilog task support. Not blocked on full SV dump task semantics -- the runtime core already supports it by this point.
