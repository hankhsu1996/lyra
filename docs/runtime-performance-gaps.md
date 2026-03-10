# Runtime Performance Gaps

Canonical working queue for closing the simulation performance gap between Lyra and Verilator. Remove items from this document once they land on main.

## North Star

Lyra's runtime engine should achieve simulation throughput within 10x of Verilator for clocked designs. The current gap is 1-19x depending on design complexity.

Strategy: preserve the general event-driven engine (IEEE 1800 semantics), but eliminate accidental overhead first, then introduce specialized fast paths for restricted design classes later.

The ordering principle:

1. Make runtime representation dense and stable (replaces accidental dynamic-structure overhead)
2. Eliminate per-activation overhead: dispatch cost, subscription reinstall, dirty-tracking machinery
3. Only then introduce clocked-design fast paths / static schedules

Do not let persistent subscriptions, topological ordering, or other optimizations become piecemeal special cases sprinkled into the current engine. The clean path: first make runtime representation dense and stable, then add persistent ownership where semantics are unchanged, only then introduce specialized scheduling tiers with explicit eligibility.

## Measurements

Benchmark data (2026-03-09, post-G4, built with `-c opt`). Pipeline: 8-stage pipe, 10K cycles. All times are simulation only (compile excluded).

| Design       | AOT (s) | Verilator (s) | Ratio | Notes                                   |
| ------------ | ------- | ------------- | ----- | --------------------------------------- |
| hello        | 0.0015  | 0.0020        | 0.8x  | Startup overhead only                   |
| stress-array | 0.0022  | 0.0013        | 1.7x  | Memory workload                         |
| riscv-cpu    | 0.0018  | 0.0017        | 1.1x  | Real design, ~1116 processes            |
| pipeline     | 0.050   | 0.0028        | 19x   | Clock-heavy, exposes scheduler overhead |

Pipeline per-cycle cost: 5us/cycle (Lyra) vs 0.3us/cycle (Verilator).

Previous baselines:

- Before G1+G2, unoptimized (`-O0`): pipeline 8.33s / 2000x, riscv-cpu 0.12s / 60x
- After G3+G6, unoptimized (`-O0`): pipeline 0.39s / 139x, riscv-cpu 0.0054s / 4.4x
- After G3+G6, optimized (`-c opt`): pipeline 0.050s / 19x, riscv-cpu 0.0018s / 1.1x (current)

**Note:** All measurements before this table were taken at `-O0` (Bazel fastbuild default), which inflates runtime by ~5-8x due to missing inlining of STL code. The 139x -> 19x drop for pipeline is entirely from measuring with the correct optimization level. See `docs/profiling.md` for why `-c opt` is mandatory for profiling and benchmarking.

## Callgrind Profile

Pipeline benchmark profiled at G4 with `-c opt` (730 million instructions total). See `docs/profiling.md` for methodology.

Inclusive percentages are for hotspot ranking, not additive budgeting. Parent/child paths overlap -- a function's inclusive cost contains its callees' costs. Do not sum inclusive percentages across rows to estimate total overhead.

Inclusive cost (percentage of total instructions, including callees):

| Function                       | Inclusive | What it does                             |
| ------------------------------ | --------- | ---------------------------------------- |
| `ExecuteTimeSlot`              | 98.5%     | Main simulation loop                     |
| `ExecuteRegion`                | 62.9%     | Process activation dispatch              |
| `FlushAndPropagateConnections` | 34.6%     | Connection/comb fixpoint iteration       |
| `AotProcessDispatch`           | 33.0%     | Process dispatch trampoline + callees    |
| `ReconcilePostActivation`      | 21.9%     | Post-activation subscription handling    |
| `FlushSignalUpdates`           | 12.7%     | Dirty-slot subscription wake-up          |
| `MarkDirtyRange` (UpdateSet)   | 11.5%     | Dirty tracking + RangeSet insert         |
| `InstallWaitSite`              | 9.8%      | Wait-site subscription install           |
| `LyraMarkDirty`                | 7.9%      | Codegen-emitted dirty notification       |
| `ScheduleNba`                  | 6.6%      | NBA queue push                           |
| `RangeSet::Insert`             | 4.1%      | Range merge (binary search + vector ops) |
| `SlotMetaRegistry::Get`        | 4.5%      | Slot metadata lookup                     |

Notable self-cost entries (instructions in the function itself, not callees):

| Entry                          | Self cost | Self % | Notes                                             |
| ------------------------------ | --------- | ------ | ------------------------------------------------- |
| `AotProcessDispatch`           | 81.5M     | 11.2%  | Process dispatch trampoline (array lookup + call) |
| `MarkDirtyRange`               | 54.4M     | 7.5%   | Per-dirty-mark bookkeeping                        |
| `memcpy`                       | 51.6M     | 7.1%   | Connection/NBA/snapshot copies                    |
| `FlushSignalUpdates`           | 51.1M     | 7.0%   | Subscription list traversal + snapshot update     |
| `FlushAndPropagateConnections` | 41.2M     | 5.6%   | Loop control + connection memcmp                  |
| `SlotMetaRegistry::Get`        | 33.1M     | 4.5%   | Slot metadata lookup (per dirty mark)             |
| `Subscribe`                    | 32.2M     | 4.4%   | Subscription install (per wait trigger)           |
| `RangeSet::Insert`             | 29.6M     | 4.1%   | Binary search + vector insert                     |
| `ExecuteRegion`                | 29.4M     | 4.0%   | Region dispatch loop                              |
| `ReconcilePostActivation`      | 25.7M     | 3.5%   | Post-activation subscription handling             |

Key difference from previous `-O0` profiles: STL iterator/container overhead (previously ~200M / 5.3% in `ByteRange` iterator cluster) is fully inlined at `-O2` and no longer appears as a separate cost center.

## Gap Inventory

Each gap has: description, measured hot-path cost, fix direction.

### G4b: Per-activation atomic stores

**Measured cost:** 29.4M self in `ExecuteRegion` (4.0%), which includes atomic stores for progress reporting. Four `memory_order_release` stores per activation: `last_process_id_`, `activation_seq_`, `current_running_process_`, `phase_`. These are for SIGUSR1-based progress reporting.

Fix direction: make conditional (only store when a signal handler is registered) or sample (update every Nth activation).

### G5: Dirty-slot propagation cost

**Measured cost:** `MarkDirtyRange` 11.5% inclusive (54.4M self, 7.5%), `RangeSet::Insert` 4.1% inclusive (29.6M self), `DeltaExternalRangesFor` 0.7% self, `ClearDelta` 1.7% inclusive. These paths overlap (MarkDirtyRange calls RangeSet::Insert, etc.), so the numbers are not additive.

**Sub-items:**

- **`RangeSet::Insert`**: 4.1% inclusive. Uses `std::vector<ByteRange>` with binary search and insertion. At `-c opt`, the STL iterator overhead is inlined but the algorithmic cost (binary search + vector shifting) remains.
- **`DeltaExternalRangesFor`**: 0.7% self. `absl::flat_hash_map<uint32_t, RangeSet>` lookup on every dirty mark for slots with external connections.
- **`ClearDelta`**: 1.7% inclusive. Resets per-slot metadata vectors on every region boundary.
- **`MarkSlotDirty`**: Parent of `MarkDirtyRange`. The two-level dedup (time-slot `seen_` + delta `delta_seen_`) adds overhead on every mark.

Fix direction: the RangeSet representation is the core problem. For the common case (single contiguous write to a slot), a `vector<ByteRange>` with binary search is overkill. Consider: (1) fast path for single-range slots, (2) replace `absl::flat_hash_map` for external ranges with a dense structure, (3) simplify the two-level dirty dedup.

### G7: Connection/comb fixpoint region

**Measured cost:** `FlushAndPropagateConnections` 34.6% inclusive (41.2M self, 5.6%).

The high self cost (5.6%) indicates significant work in the loop itself -- connection memcmp, dirty-slot scanning, iteration control. Likely contributors:

- **Unnecessary repeated iterations**: 2-3 fixpoint iterations per time slot even when the pipeline has no feedback loops.
- **Connection propagation cost per pass**: memcmp + memcpy per connection, `MarkSlotDirty` per changed output. Each iteration re-scans dirty slots.
- **Comb kernel dispatch**: Raw function pointer call per kernel (already uses `CombFunc = void (*)(void*, uint32_t)`, no `std::function` overhead).
- **Dirty-tracking overhead inside each iteration**: overlaps with G5.

Leading architectural direction: compile-time topological ordering for provably acyclic connection/comb subgraphs. A static topological order would reduce the fixpoint to a single ordered pass for acyclic designs. Pipeline has no feedback loops, so this applies directly.

However, further sub-profiling is needed to separate repeated-iteration waste from per-pass connection cost. The current data proves the fixpoint region is expensive, but does not yet distinguish how much is due to iteration count vs per-pass memcmp/dirty-tracking (which overlaps G5).

Other possible improvements within the current fixpoint model: reduce per-connection memcmp cost (e.g., dirty-flag gating instead of full compare).

### G9: Post-activation reconciliation overhead (investigation)

**Measured cost:** `ReconcilePostActivation` 21.9% inclusive (25.7M self, 3.5%). The high inclusive cost is dominated by `InstallWaitSite` (9.8% inclusive) and `ClearInstalledSubscriptions` (3.2% inclusive). The G2 compiled wait-plan refresh path is working correctly, but subscription install/clear per activation is still expensive.

The per-activation refresh path (`RefreshInstalledSnapshots` at 7.1M self, 1.0%) copies current signal values into subscription snapshots for edge detection. The optimization hypothesis still needs semantic validation.

Fix direction: first verify whether `RefreshInstalledSnapshots` is semantically redundant for static waits. `FlushSignalUpdates` already updates snapshots during edge/change detection (lines 1063-1077 of `engine_subscriptions.cpp`). If the flush path always leaves snapshots current for static-wait processes, the explicit refresh after activation may be removable. If redundant, remove or narrow it to only non-static waits.

## Prioritized Working Queue

Re-ranked by measured profile data (pipeline, post-G4, `-c opt`, 730M instructions).

Priority is based on: measured hotspot size, architectural cleanliness, expected overlap with other gaps, and implementation risk.

### Tier 1: Highest measured impact

1. **G5: Dirty-slot propagation** -- `MarkDirtyRange` 7.5% self, `RangeSet::Insert` 4.1% self, `SlotMetaRegistry::Get` 4.5% self. Multiple sub-items, each independently addressable.
2. **G7: Connection/comb fixpoint region** -- 34.6% inclusive (overlaps significantly with G5 dirty tracking). Leading direction: static topo order for acyclic subgraphs. Requires further sub-profiling to isolate iteration waste from per-pass cost.

### Tier 2: Moderate impact, needs investigation

3. **G4b: Atomic stores** -- 4.0% self (in `ExecuteRegion`). Low-risk cleanup.
4. **G9: Post-activation reconciliation** -- 21.9% inclusive (subscription install/clear dominates). Much larger than previously measured at `-c opt`. Needs investigation: why is subscription install/clear running per-activation when compiled wait-plans should make it once-per-process?

### Separate long-term track: clocked-design fast path

Not part of this queue. A separate architectural direction that should be discussed independently once the generic engine overhead is reduced.

The idea: preserve the general event-driven engine for full IEEE 1800 compliance, but add a compiled fast path for a restricted design class (purely clocked, no timing, no inter-delta feedback). This is what Verilator does -- but Verilator gives up generality entirely.

This is a different execution model, not an optimization of the current one. It needs its own design document with explicit eligibility criteria, semantic boundaries, and fallback rules.

## Reprofiling Gates

### After G3+G6 (done)

Reprofiled at `6af69df` with `-c opt`. Pipeline gap is 19x (down from 2000x before G1+G2). The `-c opt` profile reveals process dispatch as the dominant self-cost, not subscription reinstall as the `-O0` profile suggested.

**Important discovery:** All profiles before this point were taken at `-O0` (Bazel fastbuild default). The `-O0` cost distribution is dominated by STL overhead (iterator constructors, `vector::empty`, `begin/end` comparisons) that vanishes at `-O2`. This made G9 (subscription reinstall) appear to be the #1 priority when G4 (`std::function` dispatch) is the actual dominant cost. All gap rankings in this document now reflect `-c opt` measurements. See `docs/profiling.md` for the methodology update.

### After G4 implementation (done)

Reprofiled at post-G4 with `-c opt`. Total instructions: 730M (down from 732M, negligible). `std::function::_M_invoke` is gone from the profile, replaced by `AotProcessDispatch` at 81.5M self (11.2%) -- nearly identical cost. The 81.8M previously attributed to `_M_invoke` was the actual dispatch work (array lookup + indirect call to process function) inlined into the `std::function` wrapper at `-O2`, not `std::function` overhead itself. G4 is a code-quality improvement (cleaner API, better profiler symbols, no unnecessary type erasure), not a performance win.

G7 no longer overlaps with G4 -- comb kernels already used raw function pointers, and process dispatch is now also a raw function pointer. The remaining G7 cost is purely fixpoint iteration + connection propagation + dirty tracking (overlaps G5).

## Startup / Deployment Overhead

Separate from simulation throughput. Tracked here for completeness but not part of the throughput gap.

**Dynamic library startup (AOT)**: AOT binary links to `liblyra_runtime.so` (6MB). Dynamic linker loads and relocates it on every execution. Verilator statically links everything into a ~200KB binary. Adds ~1.7ms startup overhead. Fix direction: static linking option for AOT binaries.

## Completed

| Gap | Description                                             | PR   | Impact                                       |
| --- | ------------------------------------------------------- | ---- | -------------------------------------------- |
| G1  | Dense runtime tables (hash maps to flat vectors)        | #476 | Foundation for G2                            |
| G2  | Compiled wait-site plans                                | #478 | Pipeline 2000x -> 165x, RISC-V CPU 60x -> 5x |
| G3  | Small-buffer NBA storage + explicit full-overwrite mode | #484 | Eliminates NBA heap allocs (common case)     |
| G4  | Explicit process dispatch ABI (remove std::function)    |      | Code quality; no measurable perf impact      |
| G6  | Pointer-out process ABI (remove setjmp/longjmp)         | #481 | Removes nonlocal jump from hot path          |
