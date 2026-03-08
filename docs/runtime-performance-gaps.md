# Runtime Performance Gaps

Canonical working queue for closing the simulation performance gap between Lyra and Verilator.

## North Star

Lyra's runtime engine should achieve simulation throughput within 10x of Verilator for clocked designs. The current gap is 50-2000x depending on design complexity.

Strategy: preserve the general event-driven engine (IEEE 1800 semantics), but eliminate accidental overhead first, then introduce specialized fast paths for restricted design classes later.

The ordering principle:

1. Make runtime representation dense and stable (replaces accidental dynamic-structure overhead)
2. Eliminate per-activation overhead on top of the dense foundation
3. Only then introduce clocked-design fast paths / static schedules

Do not let persistent subscriptions, topological ordering, or other optimizations become piecemeal special cases sprinkled into the current engine. The clean path: first make runtime representation dense and stable, then add persistent ownership where semantics are unchanged, only then introduce specialized scheduling tiers with explicit eligibility.

## Measurements

Benchmark data from CI nightly run (2026-03-08). Pipeline: 8-stage pipe, 10K cycles. All times are simulation only (compile excluded).

| Design       | AOT (s) | Verilator (s) | Ratio | Notes                                   |
| ------------ | ------- | ------------- | ----- | --------------------------------------- |
| hello        | 0.005   | 0.002         | 3x    | Startup overhead only                   |
| stress-array | 0.007   | 0.002         | 4x    | Memory workload                         |
| riscv-cpu    | 0.12    | 0.002         | 60x   | Real design, ~1116 processes            |
| pipeline     | 8.33    | 0.004         | 2000x | Clock-heavy, exposes scheduler overhead |

Pipeline per-cycle cost: 0.43ms/cycle (Lyra) vs 0.3us/cycle (Verilator).

## Gap Inventory

Each gap has: description, hot path impact, fix direction.

### G1: Dense runtime tables

The runtime uses `absl::flat_hash_map` for all core lookups: process state, signal waiters, connection triggers, comb kernel triggers. These are keyed by dense integer IDs (`process_id`, `slot_id`) where flat array indexing would be O(1) with perfect cache locality.

This is not a micro-optimization -- it is a runtime representation cleanup. Making the runtime dense makes every other optimization (G2 persistent subscriptions, G5 dirty propagation) cleaner and faster, because they can all assume array-indexed access.

Specific maps to convert:

- `process_states_` to `std::vector<ProcessState>` indexed by `process_id`
- `signal_waiters_` to `std::vector<SignalWaiters>` indexed by `slot_id`
- `conn_trigger_map_` to flat vector indexed by trigger slot
- `comb_trigger_map_` to flat vector indexed by trigger slot

Construction-time sizing and invariants:

- Process-state table sized once from total process count (known at `LyraRunSimulation` entry)
- Signal waiter table sized once from total slot count (known from `SlotMetaRegistry`)
- Trigger tables sized once from compiled trigger metadata (connection descriptors, comb kernel word table)
- IDs must remain dense and stable for the lifetime of the engine -- no dynamic ID allocation after init

### G2: Persistent process-owned subscriptions

Every process activation clears all subscriptions and re-subscribes. For an `always_ff @(posedge clk)` block, this means: hash map lookups to find signal waiters, linked list unlink, FreeNode cleanup, then fresh Subscribe with hash map insert, snapshot copy, linked list link.

- Hot path cost: ~10 clear + 10 re-subscribe per clock edge

This is probably the biggest single win. The fix direction must be a clean ownership model, not an ad-hoc caching layer on top of the current subscription machinery.

The clean shape:

- Each process owns stable subscription node storage
- Waiting reuses owned nodes when the wait set is unchanged
- Engine-side waiter lists only relink/unlink those stable nodes
- Subscription rebuild happens only when the wait signature changes

Explicitly separate two classes:

- **Fixed wait sets** (`always_ff @(posedge clk)`) -- should become nearly zero-overhead after initial setup
- **Data-dependent wait sets** (variable-index edge triggers, late-bound rebinding) -- may still need rebuild, but these are the minority

Success criterion: for fixed wait-set processes, steady-state suspend/resume must perform zero heap allocation and zero subscription reconstruction. The engine reuses the existing nodes in place.

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

Status: needs profiling after G1 dense tables are in place.

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

### G6: Trap overhead (setjmp per activation)

Every `RunOneActivation` calls `setjmp()` to establish a trap frame for loop budget enforcement. This saves/restores all callee-saved registers. ~200K calls for pipeline benchmark.

- Hot path cost: 1 setjmp per process activation

Fix direction: amortize trap scope to time-slot or delta-cycle level, or use a different trap mechanism (return-code-based).

Caution: trap-scope widening crosses semantic boundaries. Nested runtime callbacks, long-running combinational work, and partial progress assumptions all create correctness risk. Do not start this before reprofile after tier 1 confirms it is still material.

Status: measure after G1-G3 are complete.

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

### Tier 1: Remove accidental overhead in the current model

These are clean local wins that do not change semantics or architecture. Combined, they should reduce the per-cycle cost significantly.

1. **G1: Dense runtime tables** -- Foundation for everything else. Makes all lookups O(1) with cache-friendly access. Unblocks cleaner implementation of G2.

2. **G2: Persistent process-owned subscriptions** -- Biggest single win. Eliminates the clear/re-subscribe cycle. Must be a clean ownership model, not a cache layer.

3. **G3: Small-buffer NBA storage** -- Eliminates ~320K heap allocations. Inline buffer for common case, heap spill for large values.

### Tier 2: Audit and cleanup after reprofile

After tier 1, reprofile to see what remains. These items need measurement to confirm they are material.

4. **G4: Activation queue audit** -- Profile ready-queue overhead, enqueue dedup, region transitions.

5. **G6: Trap overhead remeasure** -- Confirm setjmp is still material after tier 1. Only then consider amortization.

6. **G8: Minor hot-path cleanup** -- Bundle small items (std::function, atomics) into one cleanup pass.

### Tier 3: Structural execution improvements

These change how evaluation works, not just how fast the current model runs. Require careful semantic analysis.

7. **G5: Dirty propagation audit** -- Understand UpdateSet representation cost before changing it.

8. **G7: Acyclic connection/comb scheduling** -- Static evaluation order for provably acyclic subgraphs. Does not change event semantics.

### Separate long-term track: clocked-design fast path

Not part of this queue. A separate architectural direction that should be discussed independently once the generic engine overhead is reduced.

The idea: preserve the general event-driven engine for full IEEE 1800 compliance, but add a compiled fast path for a restricted design class (purely clocked, no timing, no inter-delta feedback). This is what Verilator does -- but Verilator gives up generality entirely.

This is a different execution model, not an optimization of the current one. It needs its own design document with explicit eligibility criteria, semantic boundaries, and fallback rules.

## Reprofiling Gates

### After Tier 1

- Rerun pipeline, riscv-cpu, stress-array benchmarks
- Compare per-cycle cost, activation count, NBA count, subscription operations
- Update measurements table with new numbers

### After G4/G5 audits

- Decide whether queue/dirty representation changes are justified before G6/G7
- If remaining gap is within 10x target, defer tier 3

## Startup / Deployment Overhead

Separate from simulation throughput. Tracked here for completeness but not part of the throughput gap.

**Dynamic library startup (AOT)**: AOT binary links to `liblyra_runtime.so` (6MB). Dynamic linker loads and relocates it on every execution. Verilator statically links everything into a ~200KB binary. Adds ~1.7ms startup overhead. Fix direction: static linking option for AOT binaries.
