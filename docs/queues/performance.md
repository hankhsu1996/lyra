# Runtime Performance Gaps

Canonical working queue for closing the simulation performance gap between Lyra and Verilator.

## North Star

Achieve simulation throughput within 10x of Verilator for clocked designs.

Strategy: preserve the general event-driven engine (IEEE 1800 semantics), eliminate accidental overhead first, then introduce specialized fast paths later.

Ordering principle:

1. Make runtime representation dense and stable
2. Eliminate per-activation overhead (dispatch, subscription, dirty tracking)
3. Only then introduce clocked-design fast paths / static schedules

## Measurements

Built with `-c opt`. Pipeline: 8-stage pipe, 10K cycles. Simulation time only.

Throughput table reflects the best pre-regression state (post-G11, pre-G7c). The callgrind profile below reflects current head (post-G7c), which includes a +65% Ir regression from the comb self-trigger bucket fix.

TODO: Re-measure wall-clock throughput after G7d (compiler must-def) lands and the bucket fix overhead is eliminated.

| Design       | AOT (s) | Verilator (s) | Ratio |
| ------------ | ------- | ------------- | ----- |
| hello        | 0.0012  | 0.0012        | 1.1x  |
| stress-array | 0.0014  | 0.0013        | 1.1x  |
| riscv-cpu    | 0.0024  | 0.0014        | 1.7x  |
| pipeline     | 0.028   | 0.0025        | 11x   |

Pipeline Ir timeline (`-c opt`, callgrind on AOT binary):

| State                     | Total Ir | Delta   |
| ------------------------- | -------- | ------- |
| Pre-G9                    | 730M     |         |
| Post-G9                   | 583M     | -20%    |
| Post-G5a                  | 515M     | -12%    |
| Post-G5c                  | 456M     | -11%    |
| Post-G6b (snapshot guard) | 446M     | -2%     |
| Post-G11 (flush fuse)     | ~446M    | neutral |
| Post-G7c (bucket fix)     | 737M     | +65%    |

All measurements use `-c opt`. See `docs/profiling.md` for why this is mandatory.

## Callgrind Profile

Post-G7c with `-c opt`, 737M instructions total. The +65% regression from the 446M pre-G7c baseline is caused by the runtime bucket fix for comb self-trigger oscillation (PR #516). See G7 section for details.

**Profiling target:** Always profile the AOT binary directly (`out/Top`), not `lyra run`. See `docs/profiling.md` for details.

Top self-cost functions, sorted by instruction count:

| Function                     | Self Ir | Self % | Description                         |
| ---------------------------- | ------- | ------ | ----------------------------------- |
| FlushAndPropagateConnections | 125.0M  | 17.0%  | Fixpoint loop + snapshot compare    |
| malloc (libc)                | 48.2M   | 6.5%   | Per-call scratch vector alloc       |
| MarkSlotDirty                | 35.2M   | 4.8%   | Dirty mark entry point              |
| TouchSlot                    | 33.0M   | 4.5%   | Dirty-slot dedup + range tracking   |
| FlushDirtySlot               | 30.9M   | 4.2%   | Per-slot subscription dispatch      |
| free (libc)                  | 30.5M   | 4.1%   | Per-call scratch vector dealloc     |
| memcpy (libc)                | 29.7M   | 4.0%   | Connection/NBA/snapshot copy        |
| LyraSuspendWait              | 26.1M   | 3.5%   | Suspend record fill (codegen ABI)   |
| ExecuteRegion                | 22.8M   | 3.1%   | Active region dispatch loop         |
| memset (libc)                | 22.0M   | 3.0%   | Delta clear + vector zero-init      |
| ReconcilePostActivation      | 20.6M   | 2.8%   | Post-activation validation          |
| AotProcessDispatch           | 20.1M   | 2.7%   | Process function pointer trampoline |
| operator new                 | 17.4M   | 2.4%   | Heap alloc (scratch + NBA)          |
| FlushSlotEdgeSubs            | 17.3M   | 2.3%   | Edge subscription traversal         |
| body_1_proc_0                | 17.3M   | 2.3%   | Emitted process code                |
| ScheduleNba                  | 16.7M   | 2.3%   | NBA push                            |
| memcmp (libc)                | 15.4M   | 2.1%   | Flush/connection/snapshot compare   |
| FlushSignalUpdates           | 14.9M   | 2.0%   | Flush orchestrator (fused in G11)   |
| EnqueueProcessWakeup         | 14.1M   | 1.9%   | Wakeup queue push                   |
| FlushSlotChangeSubs          | 13.2M   | 1.8%   | Change subscription traversal       |
| ClearDelta                   | 11.5M   | 1.6%   | Per-region range/kind reset         |

**Propagation stats (pipeline, 10K cycles):** 100,021 calls, 133,402 total iterations (avg 1.33/call), max 4 iterations per call. Iteration count is mild -- the cost is per-call scratch allocation, not excessive iteration.

## Gap Inventory

### G7: Connection/comb fixpoint region

The single largest performance problem. Three sub-items with distinct ownership.

**G7a: fixpoint infrastructure.**

Structural facts:

- Connections are structurally full-slot today -- range filtering would be dead code
- Comb kernel trigger precision is plumbed end-to-end (PR #497) but `comb_narrow=0` on benchmarks because sensitivity analysis produces full-slot observations

Baseline: FlushAndPropagateConnections at 46.8M self (10.5%) pre-G7c.

**G7c: runtime self-trigger mitigation (PR #516, temporary).**

PR #509 added `comb_write_capture_` for correct cross-kernel comb visibility. This exposed a latent bug: `always_comb` blocks with write-then-read of the same variable create a self-edge in the trigger map, causing permanent oscillation. Runtime fix: per-iteration worklist dedup + pre-comb snapshot comparison to suppress net-zero self-triggers.

Regression: 446M -> 737M (+65%). The fix allocates scratch vectors (`pending_seen`, `snapshot_index`, each slot_count-sized) on every one of ~100K calls per simulation, plus per-iteration `next_pending`. This shows up as 48M malloc + 31M free in the profile.

**G7d: compiler must-def sensitivity exclusion (root cause fix).**

The root cause is that `CollectSensitivity()` does not implement IEEE 1800 must-def exclusion -- variables always written before being read in an `always_comb` block should be excluded from the implicit sensitivity list. Fixing this at the compiler level eliminates the false self-edges, which should make the runtime snapshot path cold or unnecessary for normal designs. See `docs/comb-sensitivity-design.md`.

**Deferred G7 directions (not current priorities):**

- Assembly-time topological ordering for acyclic realized propagation subgraphs
- Relax connection kernelization to allow sub-slot connections

### G4b: Per-activation atomic stores

**Cost:** Not re-measured post-G5c; was 4.2% self pre-G5a.

**Fix:** Make conditional (only store when signal handler is registered) or sample every Nth activation.

### G5: Dirty-slot propagation cost (partially resolved)

**Was:** ~19% combined self (MarkDirtyRange 9.3%, RangeSet::Insert 5.1%, SlotMetaRegistry::Get 4.6%).

**G5 fast path applied (G5a):** Full-extent terminal state bypasses RangeSet::Insert entirely; TouchSlot inlined in header; SlotMetaRegistry::Get inlined with cold throw. Result: 583M -> 515M instructions (-12%). Three separate functions collapsed into one (TouchSlot). Post-G7c measurement: TouchSlot at 4.5% self.

**Remaining sub-items:**

- External range lookup uses `absl::flat_hash_map` on every dirty mark
- `ClearDelta` resets per-slot vectors on every region boundary (1.6% self)
- RangeSet linear scan still runs for precise (sub-slot) dirty marks, though these are rare in current benchmarks

## Prioritized Working Queue

Ranked by measured profile data (pipeline, post-G7c, `-c opt`, 737M instructions).

### Tier 0: Regression recovery

1. **G7d: Compiler must-def sensitivity exclusion** -- eliminates false comb self-edges. Should make the runtime snapshot path cold or unnecessary for normal designs, recovering roughly 200M Ir attributable to the G7c mitigation. Phase 1 (simple variables + path-sensitive must-def) covers the pipeline fixture pattern. See `docs/comb-sensitivity-design.md`.

### Tier 1: Highest impact (after regression recovery)

2. **Process dispatch** -- 5.8% self (22.8M + 20.1M). Region loop + dispatch trampoline overhead.
3. **Signal flush** -- FlushDirtySlot 4.2% + FlushSlotEdgeSubs 2.3% + FlushSlotChangeSubs 1.8%. Post-fuse (PR #511), cost is dispersed across per-kind helpers. Remaining: dirty-range filtering precision, per-kind tuning.
4. **ReconcilePostActivation overhead** -- 2.8% self (20.6M Ir). Validation + WaitSiteRegistry::Get on every activation.

### Tier 2: Moderate impact

5. **G5 remaining: ClearDelta + external ranges** -- 1.6% self for ClearDelta; external range hash map overhead.
6. **NBA queue lifecycle** -- SmallByteBuffer + operator new/delete. Allocation pressure from NBA entry construction.

### Observability

Propagation counters for connections, comb kernels, and fixpoint iteration counts are in place. Future: dirty-mark precision counters and counter-based regression tests.

### Clocked-design fast path (long-term)

Not part of this queue. A separate execution model for restricted designs (purely clocked, no timing, no inter-delta feedback). Needs its own design document once generic engine overhead is reduced.

## Startup / Deployment Overhead

Separate from simulation throughput. AOT binary links `liblyra_runtime.so` (6MB) dynamically vs Verilator's ~200KB static binary. Adds ~1.7ms startup. Fix: static linking option.

## Completed

| Gap | Description                        | PR   | Impact                                                                |
| --- | ---------------------------------- | ---- | --------------------------------------------------------------------- |
| G1  | Dense runtime tables               | #476 | Foundation for G2                                                     |
| G2  | Compiled wait-site plans           | #478 | Pipeline 2000x -> 165x, RISC-V CPU 60x -> 5x                          |
| G3  | Small-buffer NBA storage           | #484 | Eliminates NBA heap allocs (common case)                              |
| G4  | Explicit process dispatch ABI      |      | Code quality; no measurable perf impact                               |
| G6a | Pointer-out process ABI            | #481 | Removes nonlocal jump from hot path                                   |
| G9  | Static event-loop back-edge        | #493 | Pipeline 19x -> 15x, -21% instructions                                |
| G7b | Comb kernel trigger precision      | #497 | Infrastructure only; comb_narrow=0 on benchmarks                      |
| G5a | Dirty-slot full-extent fast path   | #501 | Pipeline 15x -> 12x, 583M -> 515M (-12%)                              |
| G5b | Dense typed subscriber storage     | #503 | Structural: typed hot-path vectors, cold pools                        |
| G5c | Compile-time trigger descriptors   | #506 | 515M -> 456M (-11.4%), canonical install path                         |
| G6b | Snapshot refresh guard             |      | Skip RefreshInstalledSnapshots when no observed slot is delta-dirty   |
| G11 | Fused FlushSignalUpdates traversal | #511 | 65.8M -> 14.9M self; dispersed into per-kind helpers                  |
| G7c | Comb self-trigger bucket fix       | #516 | Correctness fix; +65% Ir regression (446M -> 737M) from scratch alloc |
