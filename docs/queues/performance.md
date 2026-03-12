# Runtime Performance Gaps

Canonical working queue for closing the simulation performance gap between Lyra and Verilator.

## North Star

Achieve simulation throughput within 10x of Verilator for clocked designs. Current gap: 1-15x depending on design complexity.

Strategy: preserve the general event-driven engine (IEEE 1800 semantics), eliminate accidental overhead first, then introduce specialized fast paths later.

Ordering principle:

1. Make runtime representation dense and stable
2. Eliminate per-activation overhead (dispatch, subscription, dirty tracking)
3. Only then introduce clocked-design fast paths / static schedules

## Measurements

Post-G6, built with `-c opt`. Pipeline: 8-stage pipe, 10K cycles. Simulation time only.

| Design       | AOT (s) | Verilator (s) | Ratio |
| ------------ | ------- | ------------- | ----- |
| hello        | 0.0012  | 0.0012        | 1.1x  |
| stress-array | 0.0014  | 0.0013        | 1.1x  |
| riscv-cpu    | 0.0024  | 0.0014        | 1.7x  |
| pipeline     | 0.028   | 0.0025        | 11x   |

Previous baselines (pipeline / riscv-cpu):

- Before G1+G2 (`-O0`): 2000x / 60x
- After G3+G6 (`-c opt`): 19x / 1.1x
- After G9 (`-c opt`): 15x / 1.7x
- After G5a (`-c opt`): 12x / 1.7x
- After G5c (`-c opt`): 11x / 1.7x (current)

All measurements use `-c opt`. See `docs/profiling.md` for why this is mandatory.

## Callgrind Profile

Post-G6 with `-c opt`, 446M instructions total (down from 456M pre-G6, 515M pre-G5c, 583M pre-G5a, 730M pre-G9). See `docs/profiling.md` for methodology.

**Profiling target:** Always profile the AOT binary directly (`out/Top`), not `lyra run`. AOT mode forks a child process for the simulation; callgrind only profiles the process it launches. Profiling `lyra run` shows the compiler (~208M instructions) while the simulation (~446M) runs in an untraced child. See `docs/profiling.md` "AOT architecture and profiling implications" for details.

Top self-cost functions, sorted by instruction count:

| Function                     | Self Ir | Self % | Description                          |
| ---------------------------- | ------- | ------ | ------------------------------------ |
| FlushSignalUpdates           | 65.8M   | 14.7%  | Edge/change detection + snapshot upd |
| FlushAndPropagateConnections | 46.8M   | 10.5%  | Connection fixpoint memcpy loop      |
| TouchSlot                    | 32.4M   | 7.3%   | Dirty-slot dedup + range tracking    |
| LyraSuspendWait              | 25.5M   | 5.7%   | SuspendRecord fill (codegen ABI)     |
| ExecuteRegion                | 22.9M   | 5.1%   | Active region dispatch loop          |
| memcpy (libc)                | 20.6M   | 4.6%   | Shared: conn/NBA/snapshot            |
| AotProcessDispatch           | 20.1M   | 4.5%   | Function pointer trampoline          |
| ReconcilePostActivation      | 18.0M   | 4.0%   | Post-activation dispatch + validate  |
| ScheduleNba                  | 16.7M   | 3.7%   | NBA push operations                  |
| body_1_proc_0                | 16.4M   | 3.7%   | Emitted process code                 |
| memset (libc)                | 13.2M   | 3.0%   | Shared: ClearDelta/init              |
| LyraMarkDirty                | 13.2M   | 3.0%   | Codegen -> TouchSlot trampoline      |
| memcmp (libc)                | 12.9M   | 2.9%   | Shared: flush/conn comparison        |
| ClearDelta                   | 11.5M   | 2.6%   | Per-region range/kind reset          |
| free/malloc (libc)           | 15.1M   | 3.4%   | Heap operations combined             |
| SmallByteBuffer (move/dtor)  | 12.6M   | 2.8%   | NBA queue entry lifecycle            |
| RefreshInstalledSnapshots    | 6.6M    | 1.5%   | Snapshot re-capture (guarded)        |
| NeedsSnapshotRefresh         | 6.1M    | 1.4%   | Delta-dirty guard for refresh        |

Grouped by area (inclusive, do not sum across rows):

| Area                       | Inclusive | Self % | Description                        |
| -------------------------- | --------- | ------ | ---------------------------------- |
| Process dispatch           | 48.4%     | 9.6%   | Region loop + dispatch trampoline  |
| Connection/comb fixpoint   | 48.0%     | 10.5%  | Propagation loop + memcmp          |
| Signal flush               | 19.3%     | 14.7%  | Subscription traversal + snapshots |
| Post-activation reconcile  | 9.4%      | 6.9%   | Reconcile + snapshot refresh       |
| Dirty tracking (TouchSlot) | 7.3%      | 7.3%   | Slot dedup + kind/epoch join       |
| Wait suspension            | 6.6%      | 5.7%   | Process suspend/resume path        |
| NBA queue                  | 8.8%      | 3.7%   | NBA push operations                |
| ClearDelta                 | 2.6%      | 2.6%   | Per-region range/kind reset        |

## Gap Inventory

### G4b: Per-activation atomic stores

**Cost:** Not re-measured post-G5c; was 4.2% self pre-G5a.

**Fix:** Make conditional (only store when signal handler is registered) or sample every Nth activation.

### G5: Dirty-slot propagation cost (partially resolved)

**Was:** ~19% combined self (MarkDirtyRange 9.3%, RangeSet::Insert 5.1%, SlotMetaRegistry::Get 4.6%).

**G5 fast path applied (G5a):** Full-extent terminal state bypasses RangeSet::Insert entirely; TouchSlot inlined in header; SlotMetaRegistry::Get inlined with cold throw. Result: 583M -> 515M instructions (-12%). Three separate functions (MarkDirtyRange, RangeSet::Insert, SlotMetaRegistry::Get) collapsed into one (TouchSlot). Post-G5c measurement: TouchSlot at 7.1% self.

**Remaining sub-items:**

- External range lookup uses `absl::flat_hash_map` on every dirty mark
- `ClearDelta` resets per-slot vectors on every region boundary (2.5% self)
- RangeSet linear scan still runs for precise (sub-slot) dirty marks, though these are rare in current benchmarks

### G7: Connection/comb fixpoint region

**Cost:** 44.6% inclusive (overlaps significantly with G5).

**Key findings:**

- Connections are structurally full-slot today -- range filtering would be dead code
- Comb kernel trigger precision is plumbed end-to-end (PR #497) but `comb_narrow=0` on benchmarks because sensitivity analysis produces full-slot observations for current design patterns

**Comb self-trigger oscillation (fixed, PR TBD):** PR #509 added `comb_write_capture_` to bypass `delta_seen_` dedup for cross-kernel visibility. This exposed a latent bug: `always_comb` blocks with write-then-read of the same variable (e.g. `a = f(x); a = a + g(x)`) create a self-edge in the trigger map. Intermediate writes differ from the final value, causing permanent oscillation. Runtime fix: per-iteration worklist dedup + pre-comb snapshot comparison to suppress net-zero self-triggers. This is a bucket fix. The root cause is that `CollectSensitivity()` does not exclude variables always written before being read (IEEE 1800 `always_comb` implicit sensitivity rule). See `docs/comb-sensitivity-design.md` for the planned compiler-level fix.

**Remaining directions:**

- Measure fixpoint iteration behavior and propagation fanout on real designs
- Assembly-time topological ordering for acyclic realized propagation subgraphs
- Relax connection kernelization to allow sub-slot connections
- Compiler: must-def analysis for `always_comb` sensitivity (removes false self-edges at the source)

## Prioritized Working Queue

Ranked by measured profile data (pipeline, post-G6, `-c opt`, 446M instructions).

### Tier 1: Highest impact

1. **Signal flush tuning** -- 14.7% self (65.8M Ir). FlushSignalUpdates is the single largest self-cost function. G5b/G5c cleaned up the subscription data model. Remaining: per-kind flush tuning, dirty-range filtering precision.
2. **G7: Connection/comb fixpoint** -- 48.0% inclusive (10.5% self). Remaining: measure iteration behavior, topo ordering, sub-slot connections.
3. **Process dispatch** -- 9.6% self (22.9M + 20.1M). Region loop + dispatch trampoline overhead.

### Tier 2: Moderate impact

4. **ReconcilePostActivation overhead** -- 4.0% self (18.0M Ir). Validation + WaitSiteRegistry::Get (6.1M Ir) on every activation. Could cache descriptor or reduce validation.
5. **G5 remaining: ClearDelta + external ranges** -- 2.6% self for ClearDelta; external range hash map overhead.
6. **NBA queue lifecycle** -- SmallByteBuffer move/dtor at 2.8% self + 3.4% malloc/free. Allocation pressure from NBA entry construction.

### Observability

Propagation counters for connections, comb kernels, and fixpoint iteration counts are in place. Future: dirty-mark precision counters and counter-based regression tests.

### Clocked-design fast path (long-term)

Not part of this queue. A separate execution model for restricted designs (purely clocked, no timing, no inter-delta feedback). Needs its own design document once generic engine overhead is reduced.

## Startup / Deployment Overhead

Separate from simulation throughput. AOT binary links `liblyra_runtime.so` (6MB) dynamically vs Verilator's ~200KB static binary. Adds ~1.7ms startup. Fix: static linking option.

## Completed

| Gap | Description                         | PR   | Impact                                                              |
| --- | ----------------------------------- | ---- | ------------------------------------------------------------------- |
| G1  | Dense runtime tables                | #476 | Foundation for G2                                                   |
| G2  | Compiled wait-site plans            | #478 | Pipeline 2000x -> 165x, RISC-V CPU 60x -> 5x                        |
| G3  | Small-buffer NBA storage            | #484 | Eliminates NBA heap allocs (common case)                            |
| G4  | Explicit process dispatch ABI       |      | Code quality; no measurable perf impact                             |
| G6  | Pointer-out process ABI             | #481 | Removes nonlocal jump from hot path                                 |
| G9  | Static event-loop back-edge rewrite | #493 | Pipeline 19x -> 15x, -21% instructions                              |
| G7b | Comb kernel trigger precision       | #497 | Infrastructure only; comb_narrow=0 on benchmarks                    |
| G5a | Dirty-slot full-extent fast path    | #501 | Pipeline 15x -> 12x, 583M -> 515M (-12%)                            |
| G5b | Dense typed subscriber storage      | #503 | Structural: typed hot-path vectors, cold pools                      |
| G5c | Compile-time trigger descriptors    | #506 | 515M -> 456M (-11.4%), canonical install path                       |
| G6  | Snapshot refresh guard              |      | Skip RefreshInstalledSnapshots when no observed slot is delta-dirty |
