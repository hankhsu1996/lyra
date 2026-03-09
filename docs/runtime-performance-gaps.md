#Runtime Performance Gaps

Canonical working queue for closing the simulation performance gap between Lyra and Verilator. Remove items from this document once they land on main.

## North Star

Lyra's runtime engine should achieve simulation throughput within 10x of Verilator for clocked designs. The current gap is 2-165x depending on design complexity.

Strategy: preserve the general event-driven engine (IEEE 1800 semantics), but eliminate accidental overhead first, then introduce specialized fast paths for restricted design classes later.

The ordering principle:

1. Make runtime representation dense and stable (replaces accidental dynamic-structure overhead)
2. Eliminate per-activation overhead on top of the dense foundation
3. Only then introduce clocked-design fast paths / static schedules

Do not let persistent subscriptions, topological ordering, or other optimizations become piecemeal special cases sprinkled into the current engine. The clean path: first make runtime representation dense and stable, then add persistent ownership where semantics are unchanged, only then introduce specialized scheduling tiers with explicit eligibility.

## Measurements

Benchmark data from nightly run (2026-03-08, commit `aa2d213`). Pipeline: 8-stage pipe, 10K cycles. All times are simulation only (compile excluded).

| Design       | AOT (s) | Verilator (s) | Ratio | Notes                                   |
| ------------ | ------- | ------------- | ----- | --------------------------------------- |
| hello        | 0.0025  | 0.0013        | 2x    | Startup overhead only                   |
| stress-array | 0.0033  | 0.0013        | 2.5x  | Memory workload                         |
| riscv-cpu    | 0.0059  | 0.0012        | 5x    | Real design, ~1116 processes            |
| pipeline     | 0.46    | 0.0028        | 165x  | Clock-heavy, exposes scheduler overhead |

Pipeline per-cycle cost: 0.023ms/cycle (Lyra) vs 0.3us/cycle (Verilator).

Previous baseline (before G1+G2): pipeline 8.33s (2000x), riscv-cpu 0.12s (60x).

## Gap Inventory

Each gap has: description, hot path impact, fix direction.

### G3: Small-buffer NBA storage

Each non-blocking assignment allocates two `std::vector<uint8_t>` (value + mask) via `nba_queue_.push_back()`. For the pipeline benchmark: ~16 NBA entries per clock edge x 20K edges = ~320K vector allocations.

- Hot path cost: 2 heap allocs per NBA entry

Fix direction: inline small-buffer optimization with spill path. Most NBA writes are 4-8 bytes. A fixed-size inline buffer (like `SubscriptionNode::snapshot_inline`) eliminates the heap allocation for the common case. Heap spill only for uncommon larger values.

Do not use arena allocation as the first approach -- it creates lifetime/reset complexity that is not justified when inline storage captures most writes.

### G4: Activation queue and ready-set audit

Not yet profiled. The activation queue structure, enqueue dedup path (`is_enqueued` checks), per-region queue overhead, and resume-point representation may hide significant cost.

Areas to audit:

- Ready queue push/pop representation
- `is_enqueued` dedup checks in `FlushSignalUpdates`
- Region transitions (move semantics, clearing)
- `ScheduledEvent` struct layout and padding

Expected output:

- Queue representation cost breakdown
- Enqueue dedup overhead measurement
- Proposed replacement shape if needed

Status: ready for profiling now that G1 dense tables are in place.

### G5: Dirty-slot propagation representation

Not yet audited in detail. The `UpdateSet` drives both subscription wakeup and connection/comb propagation. For clock-heavy designs, dirty-set operations (mark, scan, clear, range overlap checks) may be a major cost.

Areas to audit:

- Are dirty sets bitsets, vectors, or something else?
- How often are sparse sets scanned repeatedly?
- What is the cost of `DeltaDirtySlots()`, `DeltaRangesFor()`, `ClearDelta()`?
- Is the range overlap check (`Overlaps()`) on the hot path?

Expected output:

- Current dirty-set representation summary
- Measured hot operations with cost breakdown
- Whether representation change is needed before G7

Status: needs dedicated audit.

### G6: Runtime execution model still uses setjmp/longjmp-based trap escape

**Gap:** Process trap handling uses `setjmp` / `longjmp` nonlocal escape at activation boundaries. This was introduced for simulator-side loop-budget enforcement, not as a SystemVerilog semantic construct. It leaves process execution on an older control-flow model that does not match the project's preferred explicit-ABI architecture.

**Target shape:** Process execution reports an explicit outcome (`kOk` / `kTrap`) through its ABI, with trap payload transport separated from engine policy. This removes nonlocal jump control flow from the hot path and aligns runtime execution with a `Result` / `expected`-like model:

- Process functions return a small named status enum (`ProcessExitCode`)
- `LyraTrap` captures a payload into TLS and returns (no longjmp)
- Caller consumes payload exactly once via `ConsumeTlsTrap()`
- Engine owns simulation policy (`HandleTrap`), not trap transport

**Why it matters:** Cleaner control-flow boundaries, cleaner separation of transport vs policy, and removal of hot-path `setjmp` overhead (~200K calls for pipeline benchmark).

Status: in progress.

### G7: Acyclic connection/comb scheduling

`FlushAndPropagateConnections()` iterates to fixpoint: propagate connections, evaluate comb kernels, check for new dirty marks, repeat. For designs with cascading connections, this runs multiple iterations per delta cycle.

- Hot path cost: 2-3 iterations per clock edge, each scanning dirty slots

Fix direction: compile-time topological ordering for provably acyclic connection/comb subgraphs. The connection and comb kernel dependency graph is known at compile time. For provably acyclic subgraphs, a static topological order can replace repeated local fixpoint iteration with a single ordered pass for that subgraph.

Any region not proven eligible stays on the existing iterative path. This is the safe, bounded subset of static scheduling. It does not change the execution model.

Explicitly out of scope: broader cycle-based scheduling, replacing the event-driven model for clocked designs, or any change that alters IEEE 1800 event semantics. That is a separate long-term track (see below).

### G8: Minor hot-path cleanup

Small overhead items that should be bundled into a single cleanup pass:

- `runner_` is `std::function` (virtual dispatch + possible heap alloc). Replace with direct function pointer -- the runner is set once at construction and never changes.
- Atomic stores for scheduler observability on every activation. Consider whether these can be conditional or sampled.

These are real but small. Do not let them distract from the big wins.

## Prioritized Working Queue

### Tier 1: Remove accidental overhead in the current model (remaining)

1. **G3: Small-buffer NBA storage** -- Eliminates ~320K heap allocations. Inline buffer for common case, heap spill for large values.

### Tier 2:

Audit and cleanup after reprofile

        2. *
        *G4 : Activation queue audit* * --Profile ready -
    queue overhead,
    enqueue dedup,
    region transitions.

            3. *
            *G6 : Trap overhead* * --Replace setjmp /
            longjmp with explicit return -code ABI.

            4. *
            *G8 : Minor hot -
        path cleanup * *--Bundle small items(std::function, atomics) into one cleanup pass.

### Tier 3: Structural execution improvements

These change how evaluation works, not just how fast the current model runs. Require careful semantic analysis.

5. **G5: Dirty propagation audit** -- Understand UpdateSet representation cost before changing it.

6. **G7: Acyclic connection/comb scheduling** -- Static evaluation order for provably acyclic subgraphs. Does not change event semantics.

### Separate long-term track: clocked-design fast path

Not part of this queue. A separate architectural direction that should be discussed independently once the generic engine overhead is reduced.

The idea: preserve the general event-driven engine for full IEEE 1800 compliance, but add a compiled fast path for a restricted design class (purely clocked, no timing, no inter-delta feedback). This is what Verilator does -- but Verilator gives up generality entirely.

This is a different execution model, not an optimization of the current one. It needs its own design document with explicit eligibility criteria, semantic boundaries, and fallback rules.

## Reprofiling Gates

### After G3

- Rerun pipeline, riscv-cpu, stress-array benchmarks
- Compare per-cycle cost, NBA count
- Update measurements table
- Decide if remaining gap justifies tier 2/3 work

### After G4/G5 audits

- Decide whether queue/dirty representation changes are justified before G6/G7
- If remaining gap is within 10x target, defer tier 3

## Startup / Deployment Overhead

Separate from simulation throughput. Tracked here for completeness but not part of the throughput gap.

**Dynamic library startup (AOT)**: AOT binary links to `liblyra_runtime.so` (6MB). Dynamic linker loads and relocates it on every execution. Verilator statically links everything into a ~200KB binary. Adds ~1.7ms startup overhead. Fix direction: static linking option for AOT binaries.

## Completed

| Gap | Description                                             | PR   | Impact                                       |
| --- | ------------------------------------------------------- | ---- | -------------------------------------------- |
| G1  | Dense runtime tables (hash maps to flat vectors)        | #476 | Foundation for G2                            |
| G2  | Persistent wait-site installation (compiled wait plans) | #478 | Pipeline 2000x -> 165x, RISC-V CPU 60x -> 5x |
