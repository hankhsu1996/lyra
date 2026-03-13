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

Throughput table reflects post-G7e state (per-kernel self-edge gating). The callgrind profile below also reflects post-G7e.

| Design       | AOT (s) | Verilator (s) | Ratio |
| ------------ | ------- | ------------- | ----- |
| hello        | 0.0012  | 0.0012        | 1.1x  |
| stress-array | 0.0014  | 0.0013        | 1.1x  |
| riscv-cpu    | 0.0024  | 0.0014        | 1.7x  |
| pipeline     | 0.039   | 0.0025        | 16x   |

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
| Post-G7d (must-def Ph1)   | 634M     | -14%    |
| Post-G7e (self-edge gate) | 525M     | -17%    |

All measurements use `-c opt`. See `docs/profiling.md` for why this is mandatory.

## Callgrind Profile

Post-G7e with `-c opt`, 525M instructions total. Down from 634M post-G7d (-17%). Two changes: (1) hoisted per-call scratch allocations (`pending_seen_`, `snapshot_index_`) to persistent Engine members; (2) per-kernel self-edge metadata gates snapshot capture/comparison per kernel at runtime. Pipeline has zero self-edge kernels, so all snapshot infrastructure is skipped entirely.

FlushAndPropagateConnections dropped from 102.4M to 70.7M (-31%). malloc/free dropped from 91.7M combined to 63M (-31%). Remaining FlushAndPropagateConnections cost is from per-call `pending`/`next_pending`/`comb_writes` vectors and comb kernel evaluation itself.

**Profiling target:** Always profile the AOT binary directly (`out/Top`), not `lyra run`. See `docs/profiling.md` for details.

Top self-cost functions, sorted by instruction count:

| Function                     | Self Ir | Self % | Description                         |
| ---------------------------- | ------- | ------ | ----------------------------------- |
| FlushAndPropagateConnections | 70.7M   | 13.5%  | Fixpoint loop (no snapshot)         |
| FlushDirtySlot               | 28.6M   | 5.5%   | Per-slot subscription dispatch      |
| TouchSlot                    | 27.4M   | 5.2%   | Dirty-slot dedup + range tracking   |
| _int_free (libc)             | 27.1M   | 5.2%   | Per-call vector dealloc             |
| MarkSlotDirty                | 25.2M   | 4.8%   | Dirty mark entry point              |
| ExecuteRegion                | 22.8M   | 4.3%   | Active region dispatch loop         |
| malloc (libc)                | 22.0M   | 4.2%   | Per-call vector alloc               |
| memcpy (libc)                | 20.7M   | 3.9%   | Connection/NBA copy                 |
| LyraSuspendWait              | 20.4M   | 3.9%   | Suspend record fill (codegen ABI)   |
| ReconcilePostActivation      | 20.3M   | 3.9%   | Post-activation validation          |
| AotProcessDispatch           | 20.1M   | 3.8%   | Process function pointer trampoline |
| ScheduleNba                  | 16.7M   | 3.2%   | NBA push                            |
| FlushSlotEdgeSubs            | 16.1M   | 3.1%   | Edge subscription traversal         |
| FlushSignalUpdates           | 14.7M   | 2.8%   | Flush orchestrator                  |
| EnqueueProcessWakeup         | 14.1M   | 2.7%   | Wakeup queue push                   |
| free (libc)                  | 14.0M   | 2.7%   | General free                        |
| memset (libc)                | 13.2M   | 2.5%   | Delta clear + vector zero-init      |
| memcmp (libc)                | 12.0M   | 2.3%   | Flush/connection compare            |
| ClearDelta                   | 11.3M   | 2.2%   | Per-region range/kind reset         |
| FlushSlotChangeSubs          | 10.6M   | 2.0%   | Change subscription traversal       |
| body_1_proc_0                | 9.0M    | 1.7%   | Emitted process code                |

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

Regression: 446M -> 737M (+65%). The fix allocates scratch vectors (`pending_seen`, `snapshot_index`, each slot_count-sized) on every one of ~100K calls per simulation, plus per-iteration `next_pending`. G7d+G7e recovered most of this: `pending_seen_` and `snapshot_index_` hoisted to persistent members, snapshot logic gated per kernel. Remaining gap (525M vs 446M) is from per-call `pending`/`next_pending`/`comb_writes` vectors.

**G7d: compiler must-def sensitivity exclusion (Phase 1 done).**

Phase 1 implements whole-variable must-def exclusion with path-sensitive control flow (sequential, if/else, case). Forward dataflow analysis on MIR CFG suppresses reads of signals that are always written before being read. Result: 737M -> 634M Ir (-14%) on pipeline fixture. See `docs/comb-sensitivity-design.md`.

**G7e: per-kernel self-edge gating + scratch hoisting (done).**

Two changes: (1) hoist `pending_seen_` and `snapshot_index_` from per-call stack locals to persistent Engine members; (2) compute per-kernel self-edge metadata at compile time (write-set vs trigger-set overlap, slot-granular), gate snapshot capture/comparison per kernel at runtime with global OR for fast skip. Pipeline has zero self-edge kernels, so all snapshot infrastructure is skipped. Result: 634M -> 525M Ir (-17%). Remaining gap vs 446M pre-G7c baseline is from per-call `pending`/`next_pending`/`comb_writes` vector allocations (~63M malloc/free).

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

Ranked by measured profile data (pipeline, post-G7e, `-c opt`, 525M instructions).

### Tier 1: Highest impact

1. **FlushAndPropagateConnections remaining overhead** -- 70.7M (13.5%). With snapshot gating done, remaining cost is per-call vector allocations (`pending`, `next_pending`, `comb_writes`) and comb evaluation. Hoisting these to persistent members would save ~30M in malloc/free.
2. **Process dispatch** -- 8.1% self (22.8M + 20.1M). Region loop + dispatch trampoline overhead.
3. **Signal flush** -- FlushDirtySlot 5.5% + FlushSlotEdgeSubs 3.1% + FlushSlotChangeSubs 2.0%. Per-kind subscription helpers.
4. **ReconcilePostActivation overhead** -- 3.9% self (20.3M Ir). Validation + WaitSiteRegistry::Get on every activation.

### Tier 2: Moderate impact

5. **G5 remaining: ClearDelta + external ranges** -- External range hash map overhead.
6. **NBA queue lifecycle** -- SmallByteBuffer + operator new/delete.

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
| G7e | Self-edge gating + scratch hoist   |      | Per-kernel metadata + hoisted allocs; 634M -> 525M (-17%)             |
