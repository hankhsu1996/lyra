# Runtime Performance Gaps

Canonical working queue for closing the simulation performance gap between Lyra and Verilator. Remove items from this document once they land on main.

## North Star

Lyra's runtime engine should achieve simulation throughput within 10x of Verilator for clocked designs. The current gap is 2-139x depending on design complexity.

Strategy: preserve the general event-driven engine (IEEE 1800 semantics), but eliminate accidental overhead first, then introduce specialized fast paths for restricted design classes later.

The ordering principle:

1. Make runtime representation dense and stable (replaces accidental dynamic-structure overhead)
2. Eliminate per-activation overhead: dispatch cost, subscription reinstall, dirty-tracking machinery
3. Only then introduce clocked-design fast paths / static schedules

Do not let persistent subscriptions, topological ordering, or other optimizations become piecemeal special cases sprinkled into the current engine. The clean path: first make runtime representation dense and stable, then add persistent ownership where semantics are unchanged, only then introduce specialized scheduling tiers with explicit eligibility.

## Measurements

Benchmark data from nightly run (2026-03-09, commit `c030550`). Pipeline: 8-stage pipe, 10K cycles. All times are simulation only (compile excluded).

| Design       | AOT (s) | Verilator (s) | Ratio | Notes                                   |
| ------------ | ------- | ------------- | ----- | --------------------------------------- |
| hello        | 0.0027  | 0.0013        | 2x    | Startup overhead only                   |
| stress-array | 0.0041  | 0.0015        | 2.7x  | Memory workload                         |
| riscv-cpu    | 0.0054  | 0.0012        | 4.4x  | Real design, ~1116 processes            |
| pipeline     | 0.39    | 0.0028        | 139x  | Clock-heavy, exposes scheduler overhead |

Pipeline per-cycle cost: 0.020ms/cycle (Lyra) vs 0.3us/cycle (Verilator).

Previous baselines:

- Before G1+G2 (pipeline 8.33s / 2000x, riscv-cpu 0.12s / 60x)
- Before G3+G6 (pipeline 0.46s / 165x, riscv-cpu 0.0059s / 5x)

## Callgrind Profile

Pipeline benchmark profiled at `c030550` (3.75 billion instructions total). See `docs/profiling.md` for methodology.

Inclusive percentages are for hotspot ranking, not additive budgeting. Parent/child paths overlap -- a function's inclusive cost contains its callees' costs. Do not sum inclusive percentages across rows to estimate total overhead.

Inclusive cost (percentage of total instructions, including callees):

| Function                       | Inclusive | What it does                             |
| ------------------------------ | --------- | ---------------------------------------- |
| `ExecuteTimeSlot`              | 98.7%     | Main simulation loop                     |
| `ExecuteRegion`                | 49.8%     | Process activation dispatch              |
| `FlushAndPropagateConnections` | 46.3%     | Connection/comb fixpoint iteration       |
| `ReconcilePostActivation`      | 20.3%     | Post-activation subscription reset       |
| `RunOneActivation`             | 20.1%     | Per-process dispatch                     |
| `FlushSignalUpdates`           | 20.1%     | Dirty-slot subscription wake-up          |
| `MarkDirtyRange` (UpdateSet)   | 17.8%     | Dirty tracking + RangeSet insert         |
| `RangeSet::Insert`             | 12.4%     | Range merge (binary search + vector ops) |
| `LyraMarkDirty`                | 12.3%     | Codegen-emitted dirty notification       |
| `InstallWaitSite`              | 10.9%     | Wait-site reinstall per activation       |
| `DeltaExternalRangesFor`       | 9.5%      | Hash map lookup for external ranges      |
| `ScheduledEvent vector growth` | 5.2%      | Queue push with reallocation             |
| `ClearDelta`                   | 4.2%      | Delta dirty reset per region             |
| `ScheduleNba`                  | 3.5%      | NBA queue push                           |

Notable self-cost entries and clusters (instructions in the function itself, not callees):

| Entry                          | Self cost | Self % | Notes                                                                                                        |
| ------------------------------ | --------- | ------ | ------------------------------------------------------------------------------------------------------------ |
| `SuspendReset`                 | 133M      | 3.5%   | Called on every suspend                                                                                      |
| `MarkDirtyRange`               | 103M      | 2.7%   | Per-dirty-mark bookkeeping                                                                                   |
| `RangeSet::Insert`             | 81M       | 2.2%   | Binary search + vector insert                                                                                |
| `FlushAndPropagateConnections` | 76M       | 2.0%   | Loop control + connection memcmp                                                                             |
| `FlushSignalUpdates`           | 74M       | 2.0%   | Subscription list traversal                                                                                  |
| `memcpy`                       | 58M       | 1.6%   | Connection/NBA/snapshot copies                                                                               |
| `RunOneActivation`             | 56M       | 1.5%   | Atomic stores + dispatch overhead                                                                            |
| `ByteRange` iterator cluster   | ~200M     | 5.3%   | Aggregated: constructor/comparison/dereference across multiple STL iterator instantiations in RangeSet paths |

## Gap Inventory

Each gap has: description, measured hot-path cost, fix direction.

### G4: Activation dispatch overhead

**Measured cost:** `RunOneActivation` 20.1% inclusive, 1.5% self. The self cost is small; the inclusive cost is dominated by callees (process execution, reconciliation). The dispatch overhead specific to G4 is the wrapper and per-activation bookkeeping.

**Sub-items:**

- **`runner_` is `std::function`**: The `std::function::operator()` -> `_M_invoke` -> `__invoke_r` -> `__invoke_impl` -> lambda indirection chain adds measurable overhead on every activation and every comb kernel dispatch. Replace with a direct function pointer.
- **4 atomic stores per activation**: `last_process_id_`, `activation_seq_`, `current_running_process_`, `phase_` -- all Release semantics. Visible in `RunOneActivation` self cost plus `memory_order` operator overhead scattered across the profile.

Fix direction: replace `std::function` with function pointer, make atomic stores conditional or remove.

### G5: Dirty-slot propagation cost

**Measured cost:** `MarkDirtyRange` 17.8% inclusive, `RangeSet::Insert` 12.4% inclusive, `ClearDelta` 4.2% inclusive, `DeltaExternalRangesFor` 9.5% inclusive. These paths overlap (MarkDirtyRange calls RangeSet::Insert, etc.), so the numbers are not additive.

**Sub-items:**

- **`RangeSet::Insert`**: 12.4% inclusive (81M self). Uses `std::vector<ByteRange>` with binary search and insertion. The `ByteRange` iterator cluster (~200M self across multiple instantiations) traces back to this path.
- **`DeltaExternalRangesFor`**: 9.5% inclusive. This is an `absl::flat_hash_map<uint32_t, RangeSet>` lookup. The abseil hash map overhead (capacity check, debug assertions, iterator comparison) is visible in the profile at ~5% combined.
- **`ClearDelta`**: 4.2% inclusive. Resets per-slot metadata vectors on every region boundary.
- **`MarkSlotDirty`**: 17.5% inclusive (parent of `MarkDirtyRange`). The two-level dedup (time-slot `seen_` + delta `delta_seen_`) adds overhead on every mark.

Fix direction: the RangeSet representation is the core problem. For the common case (single contiguous write to a slot), a `vector<ByteRange>` with binary search is overkill. Consider: (1) fast path for single-range slots, (2) replace `absl::flat_hash_map` for external ranges with a dense structure, (3) simplify the two-level dirty dedup.

### G7: Connection/comb fixpoint region

**Measured cost:** `FlushAndPropagateConnections` 46.3% inclusive. This is the single largest inclusive cost center.

The profile shows this region is expensive, but does not by itself prove which sub-cost dominates. Likely contributors:

- **Unnecessary repeated iterations**: 2-3 fixpoint iterations per time slot even when the pipeline has no feedback loops.
- **Connection propagation cost per pass**: memcmp + memcpy per connection, `MarkSlotDirty` per changed output. Each iteration re-scans dirty slots.
- **Comb kernel dispatch**: `std::function` call per kernel (same wrapper overhead as G4).
- **Dirty-tracking overhead inside each iteration**: overlaps with G5.

Leading architectural direction: compile-time topological ordering for provably acyclic connection/comb subgraphs. A static topological order would reduce the fixpoint to a single ordered pass for acyclic designs. Pipeline has no feedback loops, so this applies directly.

Other possible improvements within the current fixpoint model: reduce per-connection memcmp cost (e.g., dirty-flag gating instead of full compare), specialize comb dispatch to direct function pointers (shared with G4).

### G8: SuspendReset overhead

**Measured cost:** `SuspendReset` 133M self cost (3.5%). Called on every process suspend.

This is the only standalone item remaining after overlap cleanup. The `std::function` runner and atomic stores are covered by G4; the reconciliation/reinstall path is covered by G9.

133M instructions for what should be a simple state reset is worth inspecting. Likely causes: clearing trigger arrays, zeroing fields that could be left dirty, or non-inlined helper calls inside the reset path.

Fix direction: audit `SuspendReset` implementation, inline or simplify.

### G9: Per-activation subscription reinstall

**Measured cost:** `ReconcilePostActivation` 20.3% inclusive. Includes `InstallWaitSite` (10.9%), `Subscribe` (8.6%), `ClearInstalledSubscriptions` (0.6%), `HasPostActivationReconciliation` check (8.5% inclusive, called on every activation as the gating check for the full reconciliation path).

**Gap:** Every process activation tears down and reinstalls subscriptions, even when the wait sensitivity hasn't changed. For pipeline's clocked processes, the sensitivity list is identical across activations -- the process always waits on the same clock edge.

Fix direction: skip subscription reinstall when the wait-site is unchanged from the previous activation. The compiled wait-plan infrastructure (G2) already identifies which wait site a process will suspend at. If the wait site matches the previously installed one, skip the teardown/rebuild entirely.

The optimization must key off a compiled wait-plan identity whose semantics include the full installed subscription set (subscribed signals, trigger kinds, late-bound dependency shape), not just a source-level wait-site label. A syntactic match that misses a semantic difference would silently break sensitivity.

## Prioritized Working Queue

Re-ranked by measured profile data (pipeline, `c030550`).

Priority is based on: measured hotspot size, architectural cleanliness, expected overlap with other gaps, and implementation risk.

### Tier 1: Highest measured impact

1. **G9: Skip unchanged subscription reinstall** -- 20.3% inclusive. Largest single non-overlapping optimization opportunity. Requires wait-plan identity comparison with full semantic coverage.
2. **G5: Dirty-slot propagation** -- RangeSet representation, external range hash map, two-level dirty dedup. Multiple sub-items, each independently addressable.
3. **G7: Connection/comb fixpoint region** -- 46.3% inclusive (overlaps significantly with G5 dirty tracking and G4 dispatch). Leading direction: static topo order for acyclic subgraphs. Requires further sub-profiling to isolate iteration waste from per-pass cost.

### Tier 2: Moderate impact, easy wins

4. **G4: Activation dispatch** -- `std::function` replacement and atomic store cleanup. Small self cost but removes constant overhead on every activation and comb dispatch.
5. **G8: SuspendReset** -- 3.5% self. Audit and simplify.

### Separate long-term track: clocked-design fast path

Not part of this queue. A separate architectural direction that should be discussed independently once the generic engine overhead is reduced.

The idea: preserve the general event-driven engine for full IEEE 1800 compliance, but add a compiled fast path for a restricted design class (purely clocked, no timing, no inter-delta feedback). This is what Verilator does -- but Verilator gives up generality entirely.

This is a different execution model, not an optimization of the current one. It needs its own design document with explicit eligibility criteria, semantic boundaries, and fallback rules.

## Reprofiling Gates

### After G3+G6 (done)

Reprofiled at `c030550`. Pipeline improved from 165x to 139x (~15%). NBA heap elimination and setjmp removal each contributed modest gains. The remaining gap is dominated by scheduler hot-loop overhead, not storage or control-flow transport.

Callgrind profile taken. Top cost centers identified: `FlushAndPropagateConnections` (46.3%), `ExecuteRegion` (49.8%), `ReconcilePostActivation` (20.3%), `FlushSignalUpdates` (20.1%), dirty tracking (G5 sub-items).

### After G9/G5 implementation

- Reprofile pipeline under Callgrind
- Compare instruction counts and top-function rankings
- Update measurements table with new wall-clock numbers
- Re-rank remaining gaps

## Startup / Deployment Overhead

Separate from simulation throughput. Tracked here for completeness but not part of the throughput gap.

**Dynamic library startup (AOT)**: AOT binary links to `liblyra_runtime.so` (6MB). Dynamic linker loads and relocates it on every execution. Verilator statically links everything into a ~200KB binary. Adds ~1.7ms startup overhead. Fix direction: static linking option for AOT binaries.

## Completed

| Gap | Description                                             | PR   | Impact                                       |
| --- | ------------------------------------------------------- | ---- | -------------------------------------------- |
| G1  | Dense runtime tables (hash maps to flat vectors)        | #476 | Foundation for G2                            |
| G2  | Compiled wait-site plans                                | #478 | Pipeline 2000x -> 165x, RISC-V CPU 60x -> 5x |
| G3  | Small-buffer NBA storage + explicit full-overwrite mode | #484 | Eliminates NBA heap allocs (common case)     |
| G6  | Pointer-out process ABI (remove setjmp/longjmp)         | #481 | Removes nonlocal jump from hot path          |
