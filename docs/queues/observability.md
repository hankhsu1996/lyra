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

Activation trace (O1) is implemented behind `--trace-activations`. Aggregate counters exist for propagation stats (`-vv`) and a point-in-time SIGUSR1 snapshot.

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

Priority order: O3, O2.

### O3: Signal change trace

**Question:** Which signals changed, when, and to what value?

**Core artifact:** Runtime change hooks that capture per-signal value transitions.

**First milestone:** Text trace behind `--signal-trace`.

Complementary to VCD, not a weaker substitute: easy to diff, easy to grep, useful in CI, cheaper to inspect for targeted bugs.

### O2: VCD waveform dump ($dumpfile / $dumpvars)

**Question:** What is the full signal history, viewable alongside a reference simulator?

**Core artifact:** VCD file writer with runtime change recording hooks.

**First milestone:** `$dumpfile` / `$dumpvars` producing valid VCD readable in GTKWave.

Standard RTL debugging workflow. Enables side-by-side comparison with Verilator traces to find the exact cycle and signal where behavior diverges.
