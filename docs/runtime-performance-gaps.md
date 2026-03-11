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

Post-G5, built with `-c opt`. Pipeline: 8-stage pipe, 10K cycles. Simulation time only.

| Design       | AOT (s) | Verilator (s) | Ratio |
| ------------ | ------- | ------------- | ----- |
| hello        | 0.0012  | 0.0012        | 1.1x  |
| stress-array | 0.0014  | 0.0013        | 1.1x  |
| riscv-cpu    | 0.0024  | 0.0014        | 1.7x  |
| pipeline     | 0.034   | 0.0029        | 12x   |

Previous baselines (pipeline / riscv-cpu):

- Before G1+G2 (`-O0`): 2000x / 60x
- After G3+G6 (`-c opt`): 19x / 1.1x
- After G9 (`-c opt`): 15x / 1.7x
- After G5 (`-c opt`): 12x / 1.7x (current)

All measurements use `-c opt`. See `docs/profiling.md` for why this is mandatory.

## Callgrind Profile

Post-G5 with `-c opt`, 515M instructions total (down from 583M pre-G5, 730M pre-G9). See `docs/profiling.md` for methodology.

**Profiling target:** Always profile the AOT binary directly (`out/bin/Top`), not `lyra run`. AOT mode forks a child process for the simulation; callgrind only profiles the process it launches. Profiling `lyra run` shows the compiler (~208M instructions) while the simulation (~515M) runs in an untraced child. See `docs/profiling.md` "AOT architecture and profiling implications" for details.

Percentages are inclusive (contain callees) -- do not sum across rows.

| Area                       | Inclusive | Self % | Description                        |
| -------------------------- | --------- | ------ | ---------------------------------- |
| Process dispatch           | 54.6%     | 10.7%  | Region loop + dispatch trampoline  |
| Connection/comb fixpoint   | 41.9%     | 9.2%   | Propagation loop + memcmp          |
| Signal flush               | 16.3%     | 10.8%  | Subscription traversal + snapshots |
| Dirty tracking (TouchSlot) | 6.3%      | 6.3%   | Slot dedup + kind/epoch join       |
| Wait suspension            | 5.9%      | 5.0%   | Process suspend/resume path        |
| NBA queue                  | 8.6%      | 3.8%   | NBA push operations                |
| LyraMarkDirty              | 5.9%      | 2.7%   | Codegen -> TouchSlot trampoline    |
| ClearDelta                 | 2.2%      | 2.2%   | Per-region range/kind reset        |

## Gap Inventory

### G4b: Per-activation atomic stores

**Cost:** 4.2% self. Four `memory_order_release` stores per activation for SIGUSR1 progress reporting.

**Fix:** Make conditional (only store when signal handler is registered) or sample every Nth activation.

### G5: Dirty-slot propagation cost (partially resolved)

**Was:** ~19% combined self (MarkDirtyRange 9.3%, RangeSet::Insert 5.1%, SlotMetaRegistry::Get 4.6%).

**G5 fast path applied:** Full-extent terminal state bypasses RangeSet::Insert entirely; TouchSlot inlined in header; SlotMetaRegistry::Get inlined with cold throw. Result: 583M -> 515M instructions (-12%), three functions collapsed into one (TouchSlot at 6.3% self).

**Remaining sub-items:**

- External range lookup uses `absl::flat_hash_map` on every dirty mark
- `ClearDelta` resets per-slot vectors on every region boundary (2.2% self)
- RangeSet linear scan still runs for precise (sub-slot) dirty marks, though these are rare in current benchmarks

### G7: Connection/comb fixpoint region

**Cost:** 44.6% inclusive (overlaps significantly with G5).

**Key findings:**

- Connections are structurally full-slot today -- range filtering would be dead code
- Comb kernel trigger precision is plumbed end-to-end (PR #497) but `comb_narrow=0` on benchmarks because sensitivity analysis produces full-slot observations for current design patterns

**Remaining directions:**

- Measure fixpoint iteration behavior and propagation fanout on real designs
- Assembly-time topological ordering for acyclic realized propagation subgraphs
- Relax connection kernelization to allow sub-slot connections

## Prioritized Working Queue

Ranked by measured profile data (pipeline, post-G5, `-c opt`, 515M instructions).

### Tier 1: Highest impact

1. **Signal flush** -- 10.8% self. Subscription traversal + snapshot refresh. Now the single largest self-cost function.
2. **G7: Connection/comb fixpoint** -- 41.9% inclusive (9.2% self). Remaining: measure iteration behavior, topo ordering, sub-slot connections.
3. **Process dispatch** -- 10.7% self. Region loop + dispatch trampoline overhead.

### Tier 2: Moderate impact

4. **G5 remaining: ClearDelta + external ranges** -- 2.2% self for ClearDelta; external range hash map overhead.
5. **G4b: Atomic stores** -- not re-measured post-G5; was 4.2% self pre-G5.

### Observability

Propagation counters for connections, comb kernels, and fixpoint iteration counts are in place. Future: dirty-mark precision counters and counter-based regression tests.

### Clocked-design fast path (long-term)

Not part of this queue. A separate execution model for restricted designs (purely clocked, no timing, no inter-delta feedback). Needs its own design document once generic engine overhead is reduced.

## Startup / Deployment Overhead

Separate from simulation throughput. AOT binary links `liblyra_runtime.so` (6MB) dynamically vs Verilator's ~200KB static binary. Adds ~1.7ms startup. Fix: static linking option.

## Completed

| Gap | Description                         | PR   | Impact                                           |
| --- | ----------------------------------- | ---- | ------------------------------------------------ |
| G1  | Dense runtime tables                | #476 | Foundation for G2                                |
| G2  | Compiled wait-site plans            | #478 | Pipeline 2000x -> 165x, RISC-V CPU 60x -> 5x     |
| G3  | Small-buffer NBA storage            | #484 | Eliminates NBA heap allocs (common case)         |
| G4  | Explicit process dispatch ABI       |      | Code quality; no measurable perf impact          |
| G6  | Pointer-out process ABI             | #481 | Removes nonlocal jump from hot path              |
| G9  | Static event-loop back-edge rewrite | #493 | Pipeline 19x -> 15x, -21% instructions           |
| G7b | Comb kernel trigger precision       | #497 | Infrastructure only; comb_narrow=0 on benchmarks |
| G5a | Dirty-slot full-extent fast path    |      | Pipeline 15x -> 12x, 583M -> 515M (-12%)         |
