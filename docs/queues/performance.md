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

### Current baselines (post split-init-codegen PR #565, 2026-03-16)

| Fixture        | Total Ir | Top self-cost functions                                           |
| -------------- | -------- | ----------------------------------------------------------------- |
| clock-pipeline | 16.45B   | fixpoint 14%, dirty-track 13%, flush 12%, dispatch 8%, suspend 7% |
| fanout-comb    | 18.24B   | suspend 11%, RangeSet 10%, fixpoint 9%, dirty-track 9%, comb 5%   |
| nba-heavy      | 20.73B   | NBA 24%, dispatch 17%, dirty-track 13%, suspend 10%, flush 3%     |
| edge-sub-dense | 73.00B   | NBA 20%, dispatch 16%, dirty-track 13%, suspend 11%, enqueue 4%   |

Cost category breakdown (% of total Ir):

| Category             | clock-pipeline | fanout-comb | nba-heavy | edge-sub-dense |
| -------------------- | -------------- | ----------- | --------- | -------------- |
| Per-activation       | 14%            | 13%         | 27%       | 27%            |
| NBA lifecycle        | 7%             | --          | 24%       | 21%            |
| Dirty tracking       | 13%            | 19%         | 13%       | 13%            |
| Fixpoint/propagation | 15%            | 9%          | --        | --             |
| Edge/change flush    | 12%            | 1%          | 3%        | 2%             |
| memcpy/memcmp/memset | 9%             | 5%          | 10%       | 12%            |
| Generated code       | 4%             | 5%          | --        | --             |

Per-activation = AotProcessDispatch + RunOneActivation + ExecuteActiveRegion +
ReconcilePostActivation + LyraSuspendWait.
NBA lifecycle = ScheduleNba + ExecuteNbaRegion + SmallByteBuffer (move/copy/destroy).
Dirty tracking = MarkSlotDirty + TouchSlot + ClearDelta + MarkDirtyRange + RangeSet::Insert.

Runtime stats (1000-cycle samples, `-vvv`):

| Counter (per cycle)    | clock-pipeline | fanout-comb | nba-heavy | edge-sub-dense |
| ---------------------- | -------------- | ----------- | --------- | -------------- |
| total_activations      | 13             | 5           | 36        | 133            |
| nba_entries            | 18             | 2           | 33        | 129            |
| nba_elided             | 10.5 (58%)     | 0           | 0         | 0              |
| nba_changed            | 7.5            | 2           | 33        | 129            |
| edge_sub_wakeups       | 11             | 3           | 34        | 130            |
| propagation_iterations | 3              | 1.5         | 0         | 0              |
| conn_memcmp            | 27             | 0           | 0         | 0              |
| comb_executed          | 12             | 65          | 0         | 0              |

## Gap Inventory

### G13: Per-activation overhead

The largest cost category across all fixtures when summed (14-27%).

Components: `AotProcessDispatch` (3-8%), `RunOneActivation` (2-4%), `ExecuteActiveRegion` (2-4%), `ReconcilePostActivation` (2-5%), `LyraSuspendWait` (4-5%).

Each process activation pays: queue dequeue, dispatch trampoline, coroutine resume, generated code execution, coroutine suspend, wait-site reconciliation. For clocked designs where many processes share the same edge trigger, this per-process overhead dominates over the actual logic.

On clock-pipeline: 11 activations per posedge, 58% of NBA entries are elided (value unchanged). Per-process wake analysis (2026-03-17) confirmed that none of the 11 hot processes are wasted -- all produce meaningful state changes via NBAs. The overhead is per-activation primitive cost, not wasted wakeups.

**G13 Stage 1: process trigger metadata (completed, 2026-03-17).** Canonical process trigger metadata pipeline end-to-end: codegen extracts `ProcessTriggerEntry` from MIR wait shape, lowering canonicalizes to design-global slot IDs, metadata lowering classifies Stage-1 groupability, ABI/runtime parsing produces typed `ProcessTriggerRegistry`, constructor time builds deterministic trigger groups keyed by `(slot_id, edge)`. On clock-pipeline: 27 trigger groups, 28 grouped processes, 9 ungrouped.

**G13 Stage 2: grouped process dispatch (investigated, closed, 2026-03-17).** Built `GroupWakeRegistry` with observation points, watchers, and Stage-2 arming index. Grouped processes bypass subscription installation and are batch-enqueued from trigger groups. Gated by `kGroupedProcessDispatch` feature flag.

Result on clock-pipeline: +13% regression (1,910M vs 1,689M baseline). The subscription bypass savings (~2M Ir from reconcile) were dwarfed by new flush-path overhead: `FlushSlotEdgeAndGroupedDispatch` (+96M), `DispatchGroupedWatchers` (+42M), `FlushSignalUpdates` (+37M). The unified observation path with per-flush matching, dual dispatch loops, and armed-state checks added more overhead than it saved.

Root cause: per-activation subscription install/refresh (ReconcilePostActivation at 5%) is too small a target. The dominant cost is propagation (59%), not subscription management.

Current status: retained behind `kGroupedProcessDispatch` flag (off by default). The metadata pipeline (Stage 1) is independently useful.

Long-term direction: compile-time metadata about process trigger structure plus constructor-time realization of scheduling metadata (clock groups, execution order). Not whole-design static flattening -- that conflicts with separate compilation and specialization-first code generation.

### G14: NBA queue lifecycle cost

8-24% across fixtures (highest on nba-heavy/edge-sub-dense).

Components: `ScheduleNba` (5-10%), `ExecuteNbaRegion` (1-4%), `SmallByteBuffer` move/copy/destroy (4-7%).

Each NBA entry allocates a SmallByteBuffer, copies the value, pushes to queue, then at commit time compares, applies, and destroys the buffer. For designs with many registers this adds up.

The early-out optimization (skip if value unchanged at schedule time) already helps -- 58% elision rate on clock-pipeline. But the process still had to wake, execute, and attempt the schedule before the elision fires.

**Validated fix: bump-allocator arena for NBA value/mask bytes.** Estimated 5-8% wall-clock on nba-heavy/edge-sub-dense, 3-5% on clock-pipeline. Replace per-entry SmallByteBuffer with offset+size into a shared arena. Arena invariants: (1) bytes valid from ScheduleNba through end of ExecuteNbaRegion, never accessed after reset; (2) append-only within a region, no aliasing; (3) clear resets write pointer but preserves capacity -- no reallocation in steady state; (4) arena and queue reset together -- no lifetime mismatch; (5) read-only during commit -- safe for future parallel NBA execution. Clean long-term form, not a hack. Ready to implement when scheduled.

### G7: Connection/comb fixpoint region

14.5% on clock-pipeline (largest single function: `FlushAndPropagateConnections`). Near 0% on nba-heavy/edge-sub-dense (no connections).

**G7a: fixpoint infrastructure.**

- Connections are structurally full-slot today -- range filtering would be dead code
- Comb kernel trigger precision is plumbed end-to-end (PR #497) but `comb_narrow=0` on benchmarks because sensitivity analysis produces full-slot observations
- Clock-pipeline averages 3 fixpoint iterations per cycle, max 5

**Root cause of multiple iterations (investigation, 2026-03-16):** `InitConnectionBatch` sorts connections by `trigger_slot_id` for cache-efficient lookup, but the fixpoint pending list iterates in dirty-registration order -- NOT dependency order. On clock-pipeline's linear chain (s0->s1->...->s7), each stage needs connection (reg->out) + comb kernel (in->comb) + connection (comb->next_in). Because evaluation is unordered, a change at s0 takes multiple passes to ripple through: iteration 1 propagates reg->out for all stages, iteration 2 evaluates comb kernels, iteration 3 propagates comb->next_in -- and the newly dirtied next_in slots trigger further iterations. With topological ordering the acyclic chain converges in 1 pass.

**G7g: constructor-time realized propagation plan (investigated, not landing).**

Implemented and validated: canonical comb kernel write-set metadata through the full pipeline, Tarjan SCC decomposition at constructor time, PropagationPlan artifact with explicit acyclic/cyclic regions, region-driven dirty-seeded executor with cross-region seed delivery, topo-rank monotonicity invariants. All existing tests pass.

Result on clock-pipeline: 17.91B (+8.9% regression from 16.45B baseline). The runtime overhead of seed bucketing (0.40B), region bookkeeping, and downstream enqueue machinery (1.20B) outweighed the reduction in fixpoint iterations (3 -> 1 pass). The old unordered fixpoint at 2.38B (14.5%) was replaced by region-driven propagation at 3.70B (20.7%).

Root cause of regression: the clock-pipeline propagation graph is small (8-stage linear chain, ~16 slots) and the old fixpoint converges in 3 cheap iterations. The plan-based executor adds per-slot dispatch overhead (region lookup, seed dedup, topo-rank checks) that dominates on small graphs. The approach would likely break even or win on designs with larger propagation subgraphs or deeper dependency chains, but does not pay for itself on the target benchmark.

Architectural lessons preserved:

- Canonical comb kernel write-set metadata is valuable regardless (used for dependency analysis)
- Constructor-time SCC decomposition correctly identifies acyclic vs cyclic structure
- Region-driven execution is the right long-term shape if per-slot bookkeeping cost is amortized

Current status: implemented as the planned propagation strategy (`ExecutePlannedPropagation`), retained in the codebase behind `kPlannedPropagation` feature flag (off by default). The default worklist strategy (`ExecuteWorklistPropagation`) remains the active path. Comb kernel write-set metadata (stage 1) is independently useful and may land separately.

**G7h: compiled acyclic propagation executor (investigated, closed, 2026-03-17).** Attempted to bypass the generic worklist entirely for eligible acyclic regions with a constructor-time compiled propagation sequence. Three attempts, all worse:

| Attempt              | Result | Root cause                                           |
| -------------------- | ------ | ---------------------------------------------------- |
| Full unconditional   | +104%  | MarkSlotDirty explosion from marking unchanged slots |
| With memcmp gating   | +42%   | memcmp on all ops exceeds worklist savings           |
| Entry-driven partial | +7.8%  | Still executes too many ops; entry lookup overhead   |

Root cause: the generic worklist's dirty-driven per-slot gating (~24 Ir per slot visit) is already very efficient. Any replacement that adds per-op overhead (memcmp, entry lookup, region dispatch) can't beat it because the per-op cost is comparable to the per-slot worklist cost. The worklist's advantage is that it naturally skips the entire downstream cone of unchanged slots.

Architectural conclusion: the propagation cost (59% of total on clock-pipeline) is not reducible by replacing the worklist loop shape. The loop overhead (39% of propagation), MarkSlotDirty bookkeeping (37%), and actual data movement (19%) are all near-optimal for the current per-slot dirty-driven model. The win must come from reducing work volume, not making each iteration cheaper.

**Propagation cost breakdown (clock-pipeline, 50K cycles, 2026-03-17):**

| Component                 | Self Ir | Per prop call | % of propagation |
| ------------------------- | ------- | ------------- | ---------------- |
| Worklist loop/queue       | 236M    | 787           | 39%              |
| MarkSlotDirty + TouchSlot | 227M    | 757           | 37%              |
| memcmp                    | 56M     | 187           | 9%               |
| memcpy                    | 58M     | 193           | 10%              |
| ClearDelta                | 52M     | 173           | 9%               |

Propagation call-site breakdown (300K calls, 150K with work):

- Post-active: avg 0.67 pending slots/call (mostly just clock toggle)
- Post-NBA: avg 2.51 pending slots/call (pipeline cascade)
- 50% of calls were empty (fixed by early-return, PR pending)

Worklist internals: 74% of trigger-map lookups are misses (slot has no downstream), but the miss check (array load + count==0) is already cheapest possible. Zero re-enqueues within one propagation episode.

**Closed optimization attempts on worklist loop:**

- Fanout flags (+1.2%): extra byte load not cheaper than count==0 check
- Loop fusion (+1.5%): larger function body hurts icache

**Deferred G7 directions:**

- Relax connection kernelization to allow sub-slot connections

### G5: Dirty-slot propagation cost (partially resolved)

12-19% across fixtures. Highest on fanout-comb (19%) due to `RangeSet::Insert` (10%).

Full-extent fast path (G5a), dense subscriber storage (G5b), and compile-time trigger descriptors (G5c) resolved the bulk of this. Remaining:

- `RangeSet::Insert` linear scan -- 0% on clock-pipeline (full-slot marks only) but 10% on fanout-comb (many sub-slot dirty ranges)
- `ClearDelta` resets per-slot vectors on every region boundary (3% on clock-pipeline)
- External range lookup uses `absl::flat_hash_map` on every dirty mark

### G4b: Per-activation atomic stores

Make conditional (only store when signal handler is registered) or sample every Nth activation.

## Prioritized Working Queue

Ranked by cross-fixture impact. Baselines from 2026-03-16 profiles.

### Tier 1: Structural execution shape (constructor-time realization)

These address the core gap by making the clocked execution path more structured. Ordered by dependency, not just profile weight.

1. **G13: Clock-group execution structure** -- process trigger metadata pipeline completed (Stage 1). Grouped dispatch investigated and closed (Stage 2, +13% regression). The metadata is useful for analysis but dispatch-level optimization does not pay for itself because per-activation overhead (14%) is not the dominant cost. The dominant cost is propagation infrastructure (59%).
2. **G7g/G7h: Propagation optimization** -- both planned propagation (+8.9% regression) and compiled propagation executor (+7.8% to +104% regression) are closed. The generic worklist is near-optimal for its algorithmic shape. The next win must come from reducing propagation work volume, not loop optimization or executor replacement.

### Tier 2: Localized overhead reduction

Ready to implement when scheduled. Each is self-contained and does not depend on Tier 1.

3. **G14: NBA arena** -- validated fix, estimated 5-8% on nba-heavy/edge-sub-dense. See G14 entry for arena invariants.
4. **G5 remaining: RangeSet + ClearDelta** -- RangeSet::Insert 10% on fanout-comb. ClearDelta 3% on clock-pipeline.
5. **Signal flush helpers** -- FlushDirtySlot, FlushSlotEdgeGroups, FlushSlotChangeSubs. 2-12% depending on fixture.

### Strategic direction: runtime work elimination

**Updated 2026-03-17.** The previous strategic direction was "constructor-time realization of execution structure" -- better scheduling plans, grouped dispatch, compiled propagation executors. Multiple implementations of that approach were tried (G7g, G13 Stage 2, G7h) and all regressed or broke even. The common failure mode: they rearranged how the same work was executed without eliminating work itself.

**The core lesson.** On clock-pipeline, the runtime executes ~72 connection copies, ~8 comb kernels, and ~2M MarkSlotDirty calls per 50K cycles. Attempts to schedule these more cleverly all failed because the per-node cost (~24 Ir per slot visit) is already near-optimal for a per-slot dirty-driven model. Any replacement adds comparable or higher per-op overhead.

**The real question is not "how to execute this graph faster" but "which nodes and edges in this graph should not exist as runtime objects at all."**

Many slots, connections, and comb kernels in the current runtime representation exist because the lowering materializes every SV semantic object as an independent runtime slot with full signal/subscriber/dirty-tracking identity. For designs like clock-pipeline, this creates:

- Port-forwarding slots that are pure wire copies (no subscriber, no event semantics)
- Module-internal combinational temporaries that could be fused into consuming processes
- Connection chains (reg -> out -> in -> comb -> out -> in) where intermediate slots carry no independent runtime purpose
- Slots with full dirty/range/epoch tracking machinery when they only need value storage

**Runtime work elimination audit (clock-pipeline, 2026-03-17):**

Structural slot classification for the clock-pipeline benchmark (~70 realized slots). This is an observational audit, not a proven transform-safe analysis. Unresolved proof gaps (active trace/display references, process-body reads) remain.

| Category                                                           | Slots | Connections | Structural observation                  |
| ------------------------------------------------------------------ | ----- | ----------- | --------------------------------------- |
| Architectural registers (clk, rst_n, counter, data_reg, valid_reg) | 20    | 0           | Required: NBA targets, real state       |
| Clock port forwarding (clk -> pipe_stage.clk)                      | 8     | 8           | Required: has edge subscribers          |
| Reset port forwarding (rst_n -> pipe_stage.rst_n)                  | 8     | 8           | Candidate: stable after reset           |
| Data/valid port forwarding (out -> parent -> in)                   | 32    | 32          | Candidate: structurally forwarding-only |
| Top-level wire forwarding (counter -> d0, rst_n -> v0)             | 2     | 2           | Candidate: structurally forwarding-only |
| Comb temporaries (data_comb in each pipe_stage)                    | 8     | 0           | Candidate: single-consumer internal     |

"Candidate" means the slot appears structurally forwarding-only based on connection graph shape (single writer, single downstream, no process triggers, no comb triggers). This does NOT prove the slot is safe to eliminate -- active trace references and process-body reads are not yet checked.

Automated analysis (connection origin provenance + forwarding candidate scan) identifies 14 parent-side routing nets as high-confidence candidates. All 14 pass the current provable checks; trace/display reference proof remains unresolved.

The current pipeline chain per stage is: `data_reg -> (comb) data_out -> (connection) d_parent -> (connection) data_in -> (comb) data_comb -> (read by ff)`. Each intermediate hop costs one connection evaluation + one MarkSlotDirty + one trigger-map entry.

**Elimination targets:**

All three are compile-time / lowering-level changes, not runtime executor changes. They reduce the realized design graph before it reaches the runtime engine.

1. **Port-forwarding chain collapse** (32 slots, 32 connections on clock-pipeline). `data_reg_sN -> data_out_sN -> data_in_sN+1` becomes `data_reg_sN -> data_in_sN+1`. Same for valid chain. Eliminates intermediate slots, connections, and MarkSlotDirty calls per propagation episode. The pipeline cascade drives most propagation work.

2. **Single-consumer non-observable dataflow fusion** (8 slots, 8 comb kernels on clock-pipeline). `data_comb` is computed by `always_comb` and read only by the same module's `always_ff`. Fuse the comb computation into the consuming process body. Eliminates the comb kernel, its intermediate slot, trigger entry, and dirty machinery. This is a special case of a more general pattern: any intermediate value with no independent observability, no subscribers, and a single consumer path.

3. **Top-level wire forwarding** (2 slots, 2 connections on clock-pipeline). Trivial assigns like `d0 = counter`. Useful as a test case but too benchmark-specific to be a primary direction.

**Primary strategic direction: non-observable forwarding identity elimination (G15)**

The core observation from the audit is not about specific SV syntax patterns. It is about a general structural problem: the current lowering materializes every port binding, internal wire, and module-boundary forwarding path as a full runtime slot with independent signal identity, dirty tracking, and propagation participation. For any design with module hierarchy, this creates large numbers of runtime objects that exist only because of the lowering's representation choices, not because they carry independent runtime semantics.

The right framing is not "connection chain collapse" or "comb fusion" as separate features. It is a single general analysis:

**Which realized runtime objects lack independent observability and can be eliminated from the propagation graph?**

An object is non-observable if:

- No edge/change subscribers watch it
- No external observer (trace, display, strobe) references it
- It is not a timing/event boundary (NBA target, suspend point, etc.)
- It serves only as a forwarding/aliasing path between producer and consumer

For such objects, the value may still need to exist in design state (for correct read access), but the slot does not need independent runtime identity: no trigger-map entry, no dirty tracking, no propagation-graph participation. The connection that writes it can instead directly target the downstream consumer.

The most impactful sub-problem within this direction is **cross-module port-forwarding collapse**. Input ports that are pure pass-through of upstream values, and output ports that are pure aliases of internal registers, are the dominant source of non-observable forwarding objects in hierarchical RTL designs. This is not benchmark-specific -- it applies to any design with module instantiation.

Concrete analysis required before implementation:

- Which lowering stage creates port-forwarding slots (realization? layout?)
- Whether port slots carry any semantic obligation beyond value storage
- How connection descriptors reference port slots vs internal slots
- Whether eliminating a port slot changes the design state layout
- How trace/display references interact with port slot identity

This direction is compile-time / lowering-level work, not runtime work. The runtime engine stays unchanged; it simply receives a smaller, denser propagation graph.

The key architectural distinction: process semantic boundaries (wait, suspend, resume, edge-triggered sequential logic, NBA ordering) must be preserved. But a large fraction of the runtime's propagation work is not process-semantic -- it is dataflow that happens to be expressed through the same generic dirty/slot/subscriber protocol. Lowering that dataflow into a form LLVM can reason about is more likely to produce multi-x wins than further runtime scheduling optimization.

**Previous direction (retained for reference):**

Constructor-time realization of execution structure -- metadata pipelines, clock groups, propagation plans. The metadata infrastructure (process trigger descriptors, comb write-sets, Tarjan SCC analysis) is independently useful for analysis and future directions, even though the executor-level optimizations built on it have not yet produced wins.

**Metadata pipeline (investigation, 2026-03-16):**

| Tier             | What exists today                                                                                                                                                                 | What's missing                                                                                           |
| ---------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------- |
| Compile time     | Connection descriptors (src, dst, trigger). Comb kernel triggers (slot, byte range, self-edge). Wait-site shape (static/rebindable/dynamic). Process kind (always/always_ff/etc). | Comb kernel write-set (canonical realized outputs). Process trigger metadata (which signal, which edge). |
| Constructor time | Connection trigger map (slot -> connections). Comb trigger map (slot -> kernels). Slot metadata registry. Process trigger registry + trigger groups.                              | Dependency graph over realized work items. DAG/SCC partitioning. Slot elimination analysis.              |
| Runtime          | Per-process subscriptions (installed lazily on first suspend). EdgeWatchGroups (grouped by observation point, polarity-split).                                                    | Subscriber-free slot identification. Connection chain analysis. Comb fusion opportunities.               |

### Observability

Propagation counters for connections, comb kernels, and fixpoint iteration counts are in place.

Runtime stats available via `-vv` (core counters) and `-vvv` (detailed per-element counters). Activation trace via `--trace-activations` (per-process wake/run log to stderr).

**Added 2026-03-17:**

- Per-process wake/run summary (behind `-vvv`): wake attempts by cause, actual runs, direct dirty vs nba-only breakdown, sorted by wake count descending
- MarkSlotDirty fast-path vs first-touch breakdown
- Worklist propagation internals: trigger-map hit/miss, enqueue/dedup, pending-slot counts
- Propagation entry-point breakdown: calls with/without work, total pending slots
- Skip redundant propagation when no dirty slots (halves call count on clock-pipeline)

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
