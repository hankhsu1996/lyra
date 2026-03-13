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

Throughput table reflects post-G7f state (fixpoint workspace hoisting). The callgrind profile below also reflects post-G7f.

| Design       | AOT (s) | Verilator (s) | Ratio |
| ------------ | ------- | ------------- | ----- |
| hello        | 0.0014  | 0.0012        | 1.2x  |
| stress-array | 0.0016  | 0.0014        | 1.1x  |
| riscv-cpu    | 0.0015  | 0.0014        | 1.1x  |
| pipeline     | 0.030   | 0.0028        | 11x   |

Pipeline Ir timeline (`-c opt`, callgrind on AOT binary):

| State                      | Total Ir | Delta   |
| -------------------------- | -------- | ------- |
| Pre-G9                     | 730M     |         |
| Post-G9                    | 583M     | -20%    |
| Post-G5a                   | 515M     | -12%    |
| Post-G5c                   | 456M     | -11%    |
| Post-G6b (snapshot guard)  | 446M     | -2%     |
| Post-G11 (flush fuse)      | ~446M    | neutral |
| Post-G7c (bucket fix)      | 737M     | +65%    |
| Post-G7d (must-def Ph1)    | 634M     | -14%    |
| Post-G7e (self-edge gate)  | 525M     | -17%    |
| Post-G7f (workspace hoist) | 471M     | -10%    |

All measurements use `-c opt`. See `docs/profiling.md` for why this is mandatory.

## Callgrind Profile

Post-G7f with `-c opt`, 471M instructions total. Down from 525M post-G7e (-10%). Change: hoisted remaining per-call stack vectors (`pending`, `next_pending`, `comb_writes`, snapshot vectors) into a persistent `FixpointWorkspace` struct on Engine. All fixpoint scratch storage is now engine-owned with capacity-preserving reuse via `clear()` and `swap()`.

FlushAndPropagateConnections dropped from 70.7M to 53.1M (-25%). malloc dropped from 22.0M to 8.3M (-62%); \_int_free from 27.1M to 9.9M (-63%); free from 14.0M to 5.1M (-64%). Remaining allocator cost is from NBA queue (SmallByteBuffer) and other non-fixpoint paths.

**Profiling target:** Always profile the AOT binary directly (`out/Top`), not `lyra run`. See `docs/profiling.md` for details.

Top self-cost functions, sorted by instruction count:

| Function                     | Self Ir | Self % | Description                         |
| ---------------------------- | ------- | ------ | ----------------------------------- |
| FlushAndPropagateConnections | 53.1M   | 11.3%  | Fixpoint loop (workspace reuse)     |
| FlushDirtySlot               | 31.5M   | 6.7%   | Per-slot subscription dispatch      |
| TouchSlot                    | 27.4M   | 5.8%   | Dirty-slot dedup + range tracking   |
| ExecuteRegion                | 24.4M   | 5.2%   | Active region dispatch loop         |
| MarkSlotDirty                | 23.8M   | 5.1%   | Dirty mark entry point              |
| LyraSuspendWait              | 20.4M   | 4.3%   | Suspend record fill (codegen ABI)   |
| ReconcilePostActivation      | 20.3M   | 4.3%   | Post-activation validation          |
| AotProcessDispatch           | 20.1M   | 4.3%   | Process function pointer trampoline |
| memcpy (libc)                | 18.5M   | 3.9%   | Connection/NBA copy                 |
| ScheduleNba                  | 16.7M   | 3.6%   | NBA push                            |
| FlushSlotEdgeSubs            | 16.6M   | 3.5%   | Edge subscription traversal         |
| EnqueueProcessWakeup         | 16.0M   | 3.4%   | Wakeup queue push                   |
| FlushSignalUpdates           | 14.7M   | 3.1%   | Flush orchestrator                  |
| memset (libc)                | 13.2M   | 2.8%   | Delta clear + vector zero-init      |
| memcmp (libc)                | 11.9M   | 2.5%   | Flush/connection compare            |
| ClearDelta                   | 11.3M   | 2.4%   | Per-region range/kind reset         |
| FlushSlotChangeSubs          | 11.1M   | 2.4%   | Change subscription traversal       |
| \_int_free (libc)            | 9.9M    | 2.1%   | Vector/buffer dealloc               |
| body_1_proc_0                | 9.0M    | 1.9%   | Emitted process code                |
| SmallByteBuffer (move ctor)  | 8.3M    | 1.8%   | NBA buffer move                     |
| malloc (libc)                | 8.3M    | 1.8%   | Vector/buffer alloc                 |

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

Regression: 446M -> 737M (+65%). The fix allocates scratch vectors (`pending_seen`, `snapshot_index`, each slot*count-sized) on every one of ~100K calls per simulation, plus per-iteration `next_pending`. G7d+G7e recovered most of this: `pending_seen*`and`snapshot*index*`hoisted to persistent members, snapshot logic gated per kernel. Remaining gap (525M vs 446M) is from per-call`pending`/`next_pending`/`comb_writes` vectors.

**G7d: compiler must-def sensitivity exclusion (Phase 1 done).**

Phase 1 implements whole-variable must-def exclusion with path-sensitive control flow (sequential, if/else, case). Forward dataflow analysis on MIR CFG suppresses reads of signals that are always written before being read. Result: 737M -> 634M Ir (-14%) on pipeline fixture. See `docs/comb-sensitivity-design.md`.

**G7e: per-kernel self-edge gating + scratch hoisting (done).**

Two changes: (1) hoist `pending_seen_` and `snapshot_index_` from per-call stack locals to persistent Engine members; (2) compute per-kernel self-edge metadata at compile time (write-set vs trigger-set overlap, slot-granular), gate snapshot capture/comparison per kernel at runtime with global OR for fast skip. Pipeline has zero self-edge kernels, so all snapshot infrastructure is skipped. Result: 634M -> 525M Ir (-17%).

**G7f: fixpoint workspace hoisting (done).**

Hoist remaining per-call stack vectors (`pending`, `next_pending`, `comb_writes`, plus snapshot vectors) into a persistent `FixpointWorkspace` struct on Engine. All fixpoint scratch storage now uses capacity-preserving reuse (`clear()` + `swap()`). Result: 525M -> 471M Ir (-10%). malloc dropped 62%, \_int_free dropped 63%. FlushAndPropagateConnections self cost 70.7M -> 53.1M (-25%). Pipeline ratio improved from 16x to 11x vs Verilator. The G7c regression (446M -> 737M) is largely recovered: 471M is below 525M (pre-hoist) but still above the 446M pre-G7c baseline.

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

Ranked by measured profile data (pipeline, post-G7f, `-c opt`, 471M instructions).

### Tier 1: Highest impact

1. **FlushAndPropagateConnections remaining overhead** -- 53.1M (11.3%). Workspace allocation eliminated; remaining cost is fixpoint iteration logic (connection memcmp, trigger map traversal, worklist management). Further gains require algorithmic changes (topological ordering, narrower triggers).
2. **Process dispatch** -- 9.5% self (24.4M + 20.1M). Region loop + dispatch trampoline overhead.
3. **Signal flush** -- FlushDirtySlot 6.7% + FlushSlotEdgeSubs 3.5% + FlushSlotChangeSubs 2.4%. Per-kind subscription helpers.
4. **ReconcilePostActivation overhead** -- 4.3% self (20.3M Ir). Validation + WaitSiteRegistry::Get on every activation.

### Tier 2: Moderate impact

5. **G5 remaining: ClearDelta + external ranges** -- External range hash map overhead.
6. **NBA queue lifecycle** -- SmallByteBuffer move ctor 1.8% + operator new/delete. Combined allocator cost (malloc + \_int_free + free) now 5.7% total, down from 12.1%.

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
| G7d | Must-def sensitivity (Phase 1)     | #520 | Whole-variable must-def exclusion; 737M -> 634M (-14%)                |
| G7e | Self-edge gating + scratch hoist   | #521 | Per-kernel metadata + hoisted allocs; 634M -> 525M (-17%)             |
| G7f | Fixpoint workspace hoisting        | #523 | All fixpoint vectors persistent; 525M -> 471M (-10%), 16x -> 11x      |
