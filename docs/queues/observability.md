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

## Active Gaps

### Architecture

The signal-change observability stack (O3 text trace, O2 VCD) shares a common architecture. The center is a **scoped signal-change stream**, not VCD and not engine-owned old-value storage.

#### Existing infrastructure

The core runtime change bus already exists:

- `TraceManager` dispatches events to pluggable `TraceSink` implementations
- `FlushDirtySlotsToTrace` iterates dirty slots at end of time slot, snapshots current values, emits `ValueChange(slot_id, new_value)`
- `TraceEvent` vocabulary: `TimeAdvance`, `ValueChange`, `MemoryDirty`
- `SnapshotSlotValue` handles all storage kinds (packed2, packed4, string, handle, aggregate)

#### Layering

```
Runtime core            -- identify dirty traced slots, snapshot, emit
Trace metadata          -- signal identity, names, widths, formatting class
TraceManager            -- sink fanout, selection/filtering, dispatch
Sinks                   -- text formatting, VCD serialization, sink-local state
```

Key architectural rules:

- Runtime emits new values only. No engine-owned old-value storage.
- Sinks own presentation-specific state (e.g., last-emitted value for `old -> new` display).
- Trace metadata is separate from `SlotMetaRegistry` (storage layout vs presentation identity).
- No core event retention in `TraceManager`. Retaining sinks are add-on test/debug tooling.

#### Missing pieces

1. **~~Streaming fanout~~** -- Done. `TraceManager` is now a streaming dispatcher. Core event retention removed. Summary is a built-in `SummaryTraceSink`. Retaining sinks can be added later via `AddSink` if needed for test/debug tooling.
2. **Trace signal metadata** -- no slot_id to hierarchical signal name mapping exists in the runtime. Both text and VCD sinks need `TraceSignalInfo` (name, bit width, trace kind).
3. **Trace selection/scoping** -- currently all-or-nothing. Need per-signal enable with path to `$dumpvars(level, scope)`.
4. **Output sinks** -- no text formatter, no VCD writer.

### Implementation phases

Priority order reflects dependency chain, not user-facing priority.

#### Phase 1: Streaming TraceManager (done)

`TraceManager` is a streaming fanout dispatcher. Core event retention removed. A built-in `SummaryTraceSink` provides `--trace` summary output. External sinks added via `AddSink()` share the same dispatch path. Producer path (`FlushDirtySlotsToTrace`) unchanged.

#### Phase 2: Trace signal metadata

Add a dedicated trace metadata table built at compile/lowering time:

- `slot_id` to hierarchical signal name
- bit width / printable width
- trace kind (wire, reg, string, etc.)

Separate from `SlotMetaRegistry` (which owns byte layout for the scheduler). This is the hardest design task -- getting signal identity ownership clean makes everything else straightforward.

#### Phase 3: Trace selection/filtering

Add per-signal trace enable. Start with CLI-wide "trace everything" but infrastructure supports per-signal enablement from day one. Producer only snapshots/emits selected signals. Gives a clean path to `$dumpvars(level, scope)` later.

#### Phase 4: O3 -- Text change trace sink

**Question:** Which signals changed, when, and to what value?

First real user-facing signal trace feature. A `TextTraceSink` consuming `TimeAdvance` + `ValueChange`, outputting one line per selected signal change. If `old -> new` display is wanted, the text sink keeps previous emitted value for traced signals only (sink-local state, not engine state).

**Flag:** `--signal-trace`

Validates the full architecture: metadata, scoping, streaming.

#### Phase 5: O2 -- VCD waveform sink

**Question:** What is the full signal history, viewable alongside a reference simulator?

A `VcdTraceSink` implementing `TraceSink`. Header emission from trace metadata, initial dump for selected signals, subsequent value changes from the existing stream. VCD-specific state (identifier allocation, file format rules) stays in the sink.

**First milestone:** `$dumpfile` / `$dumpvars` producing valid VCD readable in GTKWave.

Standard RTL debugging workflow. Enables side-by-side comparison with Verilator traces.

#### Phase 6: Control surface

User-facing options: `--trace-format=text|vcd`, `--trace-file=...`, later `$dumpfile` / `$dumpvars` SystemVerilog task support. Not blocked on full SV dump task semantics -- the runtime core already supports it by this point.
