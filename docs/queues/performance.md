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

All measurements use `-c opt` callgrind on AOT binary. See `docs/profiling.md` for methodology.

### Fixture sizes

| Fixture              | Family     | Parameters                 |
| -------------------- | ---------- | -------------------------- |
| unpacked-array-read  | storage    | 32768 elements, 4096 iters |
| unpacked-array-write | storage    | 32768 elements, 4096 iters |
| clock-pipeline       | scheduling | 8-stage pipe, 500K cycles  |
| fanout-comb          | scheduling | 64-way fanout, 500K cycles |
| nba-heavy            | scheduling | 32 regs, 500K cycles       |
| edge-sub-dense       | scheduling | 128 procs, 500K cycles     |

### Current baselines (post dirty-path-thinning PR, 2026-03-14)

| Fixture             | Total Ir | Top buckets                                                      |
| ------------------- | -------- | ---------------------------------------------------------------- |
| unpacked-array-read | 632M     | MarkSlotDirty 51%, generated code 40%, LyraMarkDirty 9%          |
| clock-pipeline      | 21.94B   | fixpoint 12%, flush 17%, dirty-mark 11%, dispatch 9%, suspend 5% |

## Gap Inventory

### G7: Connection/comb fixpoint region

The single largest performance problem on the clock-pipeline fixture.

**G7a: fixpoint infrastructure.**

- Connections are structurally full-slot today -- range filtering would be dead code
- Comb kernel trigger precision is plumbed end-to-end (PR #497) but `comb_narrow=0` on benchmarks because sensitivity analysis produces full-slot observations

**Deferred G7 directions (not current priorities):**

- Assembly-time topological ordering for acyclic realized propagation subgraphs
- Relax connection kernelization to allow sub-slot connections

### G4b: Per-activation atomic stores

Make conditional (only store when signal handler is registered) or sample every Nth activation.

### G5: Dirty-slot propagation cost (partially resolved)

Full-extent fast path (G5a), dense subscriber storage (G5b), and compile-time trigger descriptors (G5c) resolved the bulk of this. Remaining:

- External range lookup uses `absl::flat_hash_map` on every dirty mark
- `ClearDelta` resets per-slot vectors on every region boundary
- RangeSet linear scan still runs for precise (sub-slot) dirty marks, though these are rare in current benchmarks

## Prioritized Working Queue

Ranked by relative profile weight (clock-pipeline fixture). Re-profile to get current absolute numbers.

### Tier 1: Highest impact

1. **FlushAndPropagateConnections remaining overhead** -- fixpoint iteration logic (connection memcmp, trigger map traversal, worklist management). Further gains require algorithmic changes (topological ordering, narrower triggers).
2. **Process dispatch** -- region loop + dispatch trampoline overhead.
3. **Signal flush** -- per-kind subscription helpers (FlushDirtySlot, FlushSlotEdgeGroups, FlushSlotChangeSubs).
4. **ReconcilePostActivation overhead** -- validation + WaitSiteRegistry::Get on every activation.

### Tier 2: Moderate impact

5. **G5 remaining: ClearDelta + external ranges** -- external range hash map overhead.
6. **NBA queue lifecycle** -- SmallByteBuffer move + allocator cost.

### Observability

Propagation counters for connections, comb kernels, and fixpoint iteration counts are in place. Future: dirty-mark precision counters and counter-based regression tests.

### Clocked-design fast path (long-term)

Not part of this queue. A separate execution model for restricted designs (purely clocked, no timing, no inter-delta feedback). Needs its own design document once generic engine overhead is reduced.

### AOT ThinLTO cross-TU inlining (deferred)

Cross-TU inlining between generated code and runtime via ThinLTO. Prototyped ~6% Ir reduction but deferred: requires solving LLVM version coherence (embedded LLVM vs system clang/lld) and a proper link-mode abstraction (explicit opt-in, coherent toolchain validation, no ambient auto-switching).

## Startup / Deployment Overhead

Separate from simulation throughput. AOT binary links `liblyra_runtime.so` (6MB) dynamically vs Verilator's ~200KB static binary. Adds ~1.7ms startup. Fix: static linking option.

## Completed

| Gap | Description                        | PR   | Impact                                                                                          |
| --- | ---------------------------------- | ---- | ----------------------------------------------------------------------------------------------- |
| G1  | Dense runtime tables               | #476 | Foundation for G2                                                                               |
| G2  | Compiled wait-site plans           | #478 | Eliminated interpreter overhead                                                                 |
| G3  | Small-buffer NBA storage           | #484 | Eliminates NBA heap allocs (common case)                                                        |
| G4  | Explicit process dispatch ABI      |      | Code quality; no measurable perf impact                                                         |
| G6a | Pointer-out process ABI            | #481 | Removes nonlocal jump from hot path                                                             |
| G9  | Static event-loop back-edge        | #493 | Eliminated per-cycle region scheduling overhead                                                 |
| G7b | Comb kernel trigger precision      | #497 | Infrastructure only; comb_narrow=0 on benchmarks                                                |
| G5a | Dirty-slot full-extent fast path   | #501 | Bypass RangeSet for full-slot marks; inline TouchSlot                                           |
| G5b | Dense typed subscriber storage     | #503 | Typed hot-path vectors, cold pools                                                              |
| G5c | Compile-time trigger descriptors   | #506 | Canonical install path, eliminated runtime descriptor alloc                                     |
| G6b | Snapshot refresh guard             |      | Skip snapshot refresh when no observed slot is delta-dirty                                      |
| G11 | Fused FlushSignalUpdates traversal | #511 | Single-pass flush; dispersed into per-kind helpers                                              |
| G7c | Comb self-trigger bucket fix       | #516 | Correctness fix; worklist dedup + snapshot comparison                                           |
| G7d | Must-def sensitivity (Phase 1)     | #520 | Whole-variable must-def exclusion in comb sensitivity                                           |
| G7e | Self-edge gating + scratch hoist   | #521 | Per-kernel self-edge metadata + hoisted scratch allocs                                          |
| G7f | Fixpoint workspace hoisting        | #523 | All fixpoint vectors persistent with capacity-preserving reuse                                  |
| G12 | Polarity-aware edge dispatch       |      | Observation-point groups + direction-aware flush; edge_sub_checks 11M -> 5.5M on clock-pipeline |
