# Runtime Performance Gaps

Canonical working queue for closing the simulation performance gap between Lyra and Verilator. Remove items from this document once they land on main.

## North Star

Lyra's runtime engine should achieve simulation throughput within 10x of Verilator for clocked designs. The current gap is 1-15x depending on design complexity.

Strategy: preserve the general event-driven engine (IEEE 1800 semantics), but eliminate accidental overhead first, then introduce specialized fast paths for restricted design classes later.

The ordering principle:

1. Make runtime representation dense and stable (replaces accidental dynamic-structure overhead)
2. Eliminate per-activation overhead: dispatch cost, subscription reinstall, dirty-tracking machinery
3. Only then introduce clocked-design fast paths / static schedules

Do not let persistent subscriptions, topological ordering, or other optimizations become piecemeal special cases sprinkled into the current engine. The clean path: first make runtime representation dense and stable, then add persistent ownership where semantics are unchanged, only then introduce specialized scheduling tiers with explicit eligibility.

## Measurements

Benchmark data (2026-03-09, post-G9, built with `-c opt`). Pipeline: 8-stage pipe, 10K cycles. All times are simulation only (compile excluded).

| Design       | AOT (s) | Verilator (s) | Ratio | Notes                                   |
| ------------ | ------- | ------------- | ----- | --------------------------------------- |
| hello        | 0.0012  | 0.0012        | 1.1x  | Startup overhead only                   |
| stress-array | 0.0014  | 0.0013        | 1.1x  | Memory workload                         |
| riscv-cpu    | 0.0024  | 0.0014        | 1.7x  | Real design, ~1116 processes            |
| pipeline     | 0.040   | 0.0029        | 15x   | Clock-heavy, exposes scheduler overhead |

Pipeline per-cycle cost: 4us/cycle (Lyra) vs 0.3us/cycle (Verilator).

Previous baselines:

- Before G1+G2, unoptimized (`-O0`): pipeline 8.33s / 2000x, riscv-cpu 0.12s / 60x
- After G3+G6, unoptimized (`-O0`): pipeline 0.39s / 139x, riscv-cpu 0.0054s / 4.4x
- After G3+G6, optimized (`-c opt`): pipeline 0.050s / 19x, riscv-cpu 0.0018s / 1.1x
- After G9, optimized (`-c opt`): pipeline 0.040s / 15x, riscv-cpu 0.0024s / 1.7x (current)

**Note:** All measurements before this table were taken at `-O0` (Bazel fastbuild default), which inflates runtime by ~5-8x due to missing inlining of STL code. The 139x -> 19x drop for pipeline is entirely from measuring with the correct optimization level. See `docs/profiling.md` for why `-c opt` is mandatory for profiling and benchmarking.

## Callgrind Profile

Profiled post-G9 with `-c opt` (578 million instructions total, down from 730M pre-G9). See `docs/profiling.md` for methodology.

Inclusive percentages are for hotspot ranking, not additive budgeting. Parent/child paths overlap -- a function's inclusive cost contains its callees' costs. Do not sum inclusive percentages across rows to estimate total overhead.

Inclusive cost (percentage of total instructions, including callees):

| Function                       | Inclusive | What it does                          |
| ------------------------------ | --------- | ------------------------------------- |
| `ExecuteTimeSlot`              | 98.1%     | Main simulation loop                  |
| `ExecuteRegion`                | 52.3%     | Process activation dispatch           |
| `FlushAndPropagateConnections` | 44.6%     | Connection/comb fixpoint iteration    |
| `AotProcessDispatch`           | 33.1%     | Process dispatch trampoline + callees |
| `FlushSignalUpdates`           | 16.9%     | Dirty-slot subscription wake-up       |
| `MarkDirtyRange` (UpdateSet)   | 14.5%     | Dirty tracking + RangeSet insert      |
| `ReconcilePostActivation`      | 10.4%     | Post-activation subscription handling |
| `LyraMarkDirty`                | 10.0%     | Codegen-emitted dirty notification    |
| `ScheduleNba`                  | 8.4%      | NBA queue push                        |
| `LyraSuspendWait`              | 5.2%      | Wait suspension ABI call              |
| `RangeSet::Insert`             | 5.1%      | Range merge (binary search + vector)  |
| `RefreshInstalledSnapshots`    | 5.0%      | Edge-detection snapshot refresh       |
| `SlotMetaRegistry::Get`        | 4.6%      | Slot metadata lookup                  |

Notable self-cost entries (instructions in the function itself, not callees):

| Entry                          | Self cost | Self % | Notes                                             |
| ------------------------------ | --------- | ------ | ------------------------------------------------- |
| `AotProcessDispatch`           | 55.2M     | 9.5%   | Process dispatch trampoline (array lookup + call) |
| `MarkDirtyRange`               | 54.4M     | 9.4%   | Per-dirty-mark bookkeeping                        |
| `FlushSignalUpdates`           | 52.2M     | 9.0%   | Subscription list traversal + snapshot update     |
| `memcpy`                       | 41.8M     | 7.2%   | Connection/NBA/snapshot copies                    |
| `FlushAndPropagateConnections` | 41.2M     | 7.1%   | Loop control + connection memcmp                  |
| `RangeSet::Insert`             | 29.6M     | 5.1%   | Binary search + vector insert                     |
| `SlotMetaRegistry::Get`        | 26.9M     | 4.6%   | Slot metadata lookup (per dirty mark)             |
| `LyraSuspendWait`              | 25.5M     | 4.4%   | Wait suspension path                              |
| `ExecuteRegion`                | 24.1M     | 4.2%   | Region dispatch loop                              |
| `ScheduleNba`                  | 18.0M     | 3.1%   | NBA queue push                                    |
| `ReconcilePostActivation`      | 18.0M     | 3.1%   | Post-activation subscription handling             |
| `RefreshInstalledSnapshots`    | 15.1M     | 2.6%   | Snapshot refresh for edge detection               |

G9 impact: `InstallWaitSite` dropped from 9.8% inclusive to negligible. `ClearInstalledSubscriptions` dropped from 3.2% to 0.1%. `Subscribe` dropped from 4.4% self to negligible. Total instructions reduced by 21% (730M -> 578M).

## Gap Inventory

Each gap has: description, measured hot-path cost, fix direction.

### G4b: Per-activation atomic stores

**Measured cost:** 24.1M self in `ExecuteRegion` (4.2%), which includes atomic stores for progress reporting. Four `memory_order_release` stores per activation: `last_process_id_`, `activation_seq_`, `current_running_process_`, `phase_`. These are for SIGUSR1-based progress reporting.

Fix direction: make conditional (only store when a signal handler is registered) or sample (update every Nth activation).

### G5: Dirty-slot propagation cost

**Measured cost:** `MarkDirtyRange` 14.5% inclusive (54.4M self, 9.4%), `RangeSet::Insert` 5.1% inclusive (29.6M self), `SlotMetaRegistry::Get` 4.6% self, `DeltaExternalRangesFor` 0.9% self, `ClearDelta` 2.2% inclusive. These paths overlap (MarkDirtyRange calls RangeSet::Insert, etc.), so the numbers are not additive.

**Sub-items:**

- **`RangeSet::Insert`**: 4.1% inclusive. Uses `std::vector<ByteRange>` with binary search and insertion. At `-c opt`, the STL iterator overhead is inlined but the algorithmic cost (binary search + vector shifting) remains.
- **`DeltaExternalRangesFor`**: 0.7% self. `absl::flat_hash_map<uint32_t, RangeSet>` lookup on every dirty mark for slots with external connections.
- **`ClearDelta`**: 1.7% inclusive. Resets per-slot metadata vectors on every region boundary.
- **`MarkSlotDirty`**: Parent of `MarkDirtyRange`. The two-level dedup (time-slot `seen_` + delta `delta_seen_`) adds overhead on every mark.

Fix direction: the RangeSet representation is the core problem. For the common case (single contiguous write to a slot), a `vector<ByteRange>` with binary search is overkill. Consider: (1) fast path for single-range slots, (2) replace `absl::flat_hash_map` for external ranges with a dense structure, (3) simplify the two-level dirty dedup.

### G7: Connection/comb fixpoint region

**Measured cost:** `FlushAndPropagateConnections` 44.6% inclusive (41.2M self, 7.1%).

The high self cost (7.1%) indicates significant work in the loop itself -- connection memcmp, dirty-slot scanning, iteration control.

#### Root cause analysis: trigger granularity

Investigation (2026-03-10) revealed two things:

1. The propagation hot path is **slot-granular** -- connections and comb kernels fire for any dirty mark on a slot regardless of which bytes changed. The dirty-range infrastructure (`UpdateSet`, `RangeSet`) tracks byte-level precision, but propagation ignores it.

2. **Connections structurally cannot benefit from range filtering today.** `TryKernelizeConnection` enforces no projections on source/dest (full-slot copy). The trigger signal is always a root-level design-global with no projections. Sensitivity analysis (`HasStaticObservation`) requires projections to narrow the observation, so all kernelized connections have `trigger_byte_size == 0` (full-slot observation). The `trigger_byte_offset` / `trigger_byte_size` fields in `ConnectionDescriptor` are structurally always `(0, 0)`.

This means the original G7 framing ("add range-overlap check before memcmp") would be dead code for current connection semantics. Adding the guard is deferred until kernelization relaxes to allow sub-slot connections.

#### Where range precision IS consumed

Process subscriptions in `FlushSignalUpdates` already use `dirty_ranges.Overlaps(byte_offset, byte_size)` to skip non-overlapping wakeups. This works because processes have narrowed observations (struct field reads, array element reads, bit selects). The asymmetry: the less-frequent outer operation (process wakeup) has precision, while the hot inner loop (connections + comb kernels) does not and currently cannot.

#### Comb kernels

`CombKernelEntry` stores only whole-slot `trigger_slots`. No read-set extraction exists -- `TryKernelizeComb` extracts trigger slot IDs from the Wait terminator but never analyzes which byte ranges the kernel body reads. The runtime fires all kernels triggered by a slot regardless of which bytes changed.

Blockers for comb kernel range filtering: no general read-set extraction pass exists (sensitivity analysis only covers Wait triggers, not the full body). Inter-procedural analysis needed if kernels call functions.

#### Fix directions

The investigation invalidated the original "consume existing precision" approach for connections. Revised directions:

1. **Instrument the propagation loop** (prerequisite for all others): Add counters for connection evaluations, comb kernel evaluations, memcmp hit rate, fixpoint iteration count. Without instrumentation, the relative waste of connections vs comb kernels vs iteration count is unknown. This is cheap and should be done first. (Done: `PropagationStats` counters, gated behind `-vv`.)

2. **Comb kernel read-set extraction** (medium effort, hypothesis pending instrumentation): Extend `TryKernelizeComb` to extract per-trigger-slot byte ranges from the process body. Encode in `CombKernel` metadata. Filter in the propagation loop using `DeltaRangesFor().Overlaps()`. Unlike connections, comb kernels can read sub-slot ranges, so this has plausible impact -- but the actual waste magnitude is unknown until instrumented.

3. **Static topological ordering** (high effort, architectural): Topo sort for acyclic connection/comb subgraphs, reducing fixpoint to a single ordered pass. Pipeline has no feedback loops. Value depends on how many iterations the current fixpoint takes -- instrumentation (1) will answer this. Requires careful phase-boundary design: the sort must happen at design realization (where the full connection graph is known), not during specialization compilation. Must not contaminate the specialization-scoped compilation model.

4. **Relax connection kernelization** (medium effort, compile-time): Allow sub-slot source projections in `TryKernelizeConnection`. This would enable narrowed trigger observations on connections. Requires changes to connection extraction and metadata lowering.

### G9: Event-controlled process lowering defeats refresh reuse (DONE)

**Measured cost (pre-fix):** ~13% of total instructions (`InstallWaitSite` 9.8% + `ClearInstalledSubscriptions` 3.2%).

**Root cause:** `always_ff`/`always` were lowered to Wait -> body -> Repeat. The Repeat forced `kRepeat` which invalidated `installed_wait`, so the next `kWait` did a full clear + reinstall instead of the cheap refresh path.

**Fix:** Lowering-shape rewrite in `process.cpp`. For `always_ff` and static event-controlled `always` with a pure wait-arm entry block (no statements), the body `Repeat` back-edge is replaced with a cloned `Wait` terminator. The entry block remains as a one-time initial arm; subsequent activations re-arm the same wait directly from the body block. Pipeline improved from 19x to 15x.

## Prioritized Working Queue

Re-ranked by measured profile data (pipeline, post-G9, `-c opt`, 578M instructions).

Priority is based on: measured hotspot size, architectural cleanliness, expected overlap with other gaps, and implementation risk.

### Tier 1: Highest measured impact

1. **G5: Dirty-slot propagation** -- `MarkDirtyRange` 9.4% self, `RangeSet::Insert` 5.1% self, `SlotMetaRegistry::Get` 4.6% self. ~19% combined self cost. The dominant cost center. Multiple sub-items, each independently addressable.
2. **G7: Connection/comb fixpoint region** -- 44.6% inclusive (overlaps significantly with G5 dirty tracking). Investigation confirmed propagation is slot-granular. Connection range filtering is structurally blocked (all connections are full-slot today). Next steps: (a) instrument to measure iteration count vs per-pass waste, (b) comb kernel read-set extraction, (c) evaluate topo ordering based on instrumentation data.
3. **`FlushSignalUpdates`** -- 9.0% self (52.2M). Subscription list traversal + snapshot update on every dirty-slot flush. Newly prominent after G9 removed subscription reinstall overhead.

### Tier 2: Moderate impact

4. **G4b: Atomic stores** -- 4.2% self (in `ExecuteRegion`). Low-risk cleanup.

### Cross-cutting: Execution-discipline observability

Performance bugs where the system produces correct results but does excessive work are invisible to functional tests and only surface via profiling. The G7 investigation exposed this pattern: dirty-range precision exists but propagation ignores it. To catch similar issues earlier, add structured runtime counters and counter-based regression tests.

**Propagation counters** (connection-path, add first since the code is already being touched):

- `connections_considered`, `connections_memcmp_executed`, `connections_memcpy_executed`

**Future counter categories** (add when working on the relevant gap):

- Comb kernel: `comb_considered`, `comb_executed`, `comb_no_visible_write`
- Fixpoint: `propagation_iterations_per_timeslot` (histogram or max)
- Dirty precision: `dirty_marks_total`, `dirty_marks_full_slot`, `dirty_marks_precise`

**Counter-based perf tests**: Tests that assert on internal counter values (not wall clock) to verify execution discipline. Example: a design where only one field of a multi-field slot changes per cycle -- once narrowed trigger metadata exists (currently all connections are full-slot), unrelated connections/kernels should not be evaluated. For now, useful for verifying fixpoint iteration counts and memcmp hit rates. These tests are deterministic, CI-friendly, and catch over-triggering regressions that functional tests miss.

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

### After G9 implementation (done)

Reprofiled post-G9 with `-c opt`. Total instructions: 578M (down from 730M, -21%). G9 eliminated the per-cycle subscription reinstall churn for static event-controlled processes. `InstallWaitSite` dropped from 9.8% inclusive to negligible, `ClearInstalledSubscriptions` from 3.2% to 0.1%, `Subscribe` from 4.4% self to negligible. `AotProcessDispatch` self cost dropped from 81.5M to 55.2M (the reinstall work was attributed to it via the dispatch path).

The cost distribution shifted: G5 dirty-slot propagation is now the clear #1 priority (~19% combined self cost). `FlushSignalUpdates` (9.0% self) emerged as a new prominent cost center -- previously masked by the subscription reinstall overhead.

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
| G9  | Static event-loop back-edge rewrite                     | #493 | Pipeline 19x -> 15x, -21% instructions       |
