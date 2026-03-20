# Runtime Performance

> **Queue rules.** Checkboxes at top for progress. Unchecked = gap. Finished items: remove the section, keep one checked line. Every unchecked item must have a short tag/ID for tracking (e.g. G14, CQ1). Each item should be independently addressable -- it can be investigated and landed as a standalone change. No code in items (no function names, variable names, snippets). Items describe what the gap is, where to look, and why it matters -- enough to re-investigate from scratch. Investigation logs and design details belong in conversation history, not here.

Canonical working queue for closing the simulation performance gap between Lyra and Verilator.

## North Star

Achieve simulation throughput within 10x of Verilator for clocked designs. Preserve the general event-driven engine (IEEE 1800 semantics), eliminate accidental overhead first, then introduce specialized fast paths later.

## Progress

- [x] G1: Dense runtime tables (#476)
- [x] G2: Compiled wait-site plans (#478)
- [x] G3: Small-buffer NBA storage (#484)
- [x] G4: Explicit process dispatch ABI
- [x] G5a: Dirty-slot full-extent fast path (#501)
- [x] G5b: Dense typed subscriber storage (#503)
- [x] G5c: Compile-time trigger descriptors (#506)
- [x] G6a: Pointer-out process ABI (#481)
- [x] G6b: Snapshot refresh guard
- [x] G7b: Comb kernel trigger precision (#497)
- [x] G7c: Comb self-trigger bucket fix (#516)
- [x] G7d: Must-def sensitivity Phase 1 (#520)
- [x] G7e: Self-edge gating + scratch hoist (#521)
- [x] G7f: Fixpoint workspace hoisting (#523)
- [x] G9: Static event-loop back-edge (#493)
- [x] G11: Fused flush traversal (#511)
- [x] G12: Polarity-aware edge dispatch
- [x] G13 per-activation Stage 1: process trigger metadata
- [x] Packed storage view Stage 0: module boundary + data model
- [x] Packed storage view Stage 1: byte-addressable localized read
- [x] Packed storage view Stage 2: byte-addressable localized immediate write (benchmark pending)
- [x] Packed storage view Stage 3: localized deferred/NBA write (#585)
- [x] PSV4: Packed storage view whole-value materialization boundary
- [ ] CQ1: Packed storage view bulk init lowering quality
- [ ] CQ2: Packed storage view 2-state unknown-plane elision
- [ ] CQ3: Packed storage view deferred-notification dead code elision
- [x] Commit-boundary model: visibility/commit boundary definition
- [x] Commit-boundary model: multi-segment activation-local support
- [ ] CB1: Facts-to-contract model for activation-local statements
- [ ] CB2: Region-local canonical dependency analysis
- [ ] CB3: Commit-boundary model delayed-commit register promotion
- [ ] G14: NBA arena (bump-allocator for value/mask bytes)
- [ ] G5 remaining: RangeSet linear scan + ClearDelta per-region reset
- [ ] G15: Signal flush helpers
- [ ] G4b: Per-activation atomic stores (conditional on signal handler)
- [ ] G16: Runtime work elimination
- [ ] CQ4: Place-based value materialization overhead

## Active Gaps

### CB1: Facts-to-contract model for activation-local statements

Replace the taxonomy-centered activation-local boundary model (BoundaryKind -> TranslateBoundary -> SyncAction) with a facts-to-contract pipeline. Semantic analysis produces factual descriptions of each statement's canonical-state interaction relevant to managed coherence. Contract planning converts those facts into sync/reload decisions. Executor consumes the decisions mechanically.

The v1 model used a vague kObservation bucket that drifted from its real correctness requirements: display effects, module-slot assigns, and file I/O were all classified as observation boundaries requiring pre-sync, even though they do not interact with canonical storage in a way that affects managed-shadow coherence. The facts model eliminates these unnecessary syncs.

Adjacent fix: the runtime flush architecture had a hidden cross-slot ordering dependency between rebind watchers and edge evaluation. When a rebind dependency slot and its target slot became dirty in the same pass, flush order determined whether the edge trigger evaluated at the stale or updated observation point. Fixed by splitting FlushSignalUpdates into two global phases: rebind first, then edge/change/container. This makes the flush order-independent and removes a correctness constraint that was previously masked by the old per-statement sync.

### CB2: Region-local canonical dependency analysis

Per-region analysis of which managed slots have been modified since the last sync. This is the proof that tells the contract planner whether a sync can be skipped for a specific managed slot at a given contract point. The current model syncs all managed slots at every contract point; CB2 would allow skipping sync for slots whose shadow has not been modified. Must be specialization-local.

### CB3: Commit-boundary model delayed-commit register promotion

Keep eligible slot-backed scalars in registers across a region, commit back to slot storage only at required boundaries. This is the downstream optimization that uses the commit-boundary definition and region-local analysis. It is not the first thing to build.

### CQ1: Packed storage view bulk init lowering quality

Whole-value packed zero-initialization emits a single LLVM integer store (e.g. a 65536-bit integer zero) instead of a memset intrinsic. LLVM's backend unrolls this into hundreds of vector store instructions (~256 vmovups for an 8KB array), consuming instruction cache and producing a large code footprint for a single zero-fill. The issue is that the alloca for the canonical buffer is typed as a large integer rather than a byte array, which prevents LLVM from recognizing it as a bulk memory operation.

The fix is to ensure the canonical buffer alloca is typed as a byte array and the zero-fill is emitted as a memset intrinsic that LLVM can lower to an efficient loop or rep-stos sequence. Look at the packed commit layer where canonical buffers are allocated and zero-initialized for aggregate stores.

### CQ2: Packed storage view 2-state unknown-plane elision

Per-element packed array writes unconditionally store to the unknown (X/Z) plane even when the RHS is provably 2-state (no unknown bits). In the packed-array-write benchmark, every inner-loop iteration writes a constant zero to the unknown plane alongside the value plane write. This doubles memory bandwidth for the hot loop.

The packed storage view byte-addressable store path checks whether the storage is 4-state but does not check whether the specific RHS value has unknown bits. When the RHS unknown component is null (provably 2-state), the unknown-plane load, store, and compare should all be skipped. Look at the byte-addressable store function in the packed storage view module.

### CQ3: Packed storage view deferred-notification dead code elision

Per-element packed array writes compute a change-detection predicate (load old value, compare with new value) even when dirty notification is deferred to a loop-exit edge. The predicate is never used -- the deferred notification path returns early without consuming it. LLVM's dead code elimination removes the compare instructions from native code, so there is no runtime cost, but the unnecessary IR instructions increase IR size and optimization time.

The fix is to skip change-detection computation when the notification policy is deferred. The notification deferral flag is available at the store call site but is not checked before emitting the compare sequence. Look at the byte-addressable store function and the notification dispatch in the packed storage view module.

### G14: NBA arena

Replace per-entry heap allocation for NBA value/mask bytes with a bump-allocator arena. Estimated 5-8% on nba-heavy/edge-sub-dense, 3-5% on clock-pipeline. Arena is append-only within a region, reset at region boundary.

### G5 remaining: RangeSet + ClearDelta

RangeSet linear scan is 10% on fanout-comb (many sub-slot dirty ranges, 0% on clock-pipeline which uses full-slot marks only). ClearDelta resets per-slot vectors on every region boundary (3% on clock-pipeline).

### G15: Signal flush helpers

Per-slot flush functions are 2-12% depending on fixture. Possible inlining or specialization of the hot flush path.

### G4b: Per-activation atomic stores

Every process activation writes several atomic stores (phase, activation sequence, current process ID) for the signal-handler crash dump path. These are memory-order-release stores on every dispatch and return, adding per-activation overhead even when no signal handler is installed. Conditional on whether a signal handler is registered, these could be skipped entirely or downgraded to plain stores.

### G16: Runtime work elimination

Many runtime slots, connections, and comb kernels exist because the lowering materializes every SV semantic object with full signal/subscriber/dirty-tracking identity. A large fraction is pure dataflow that could be fused or eliminated. This is the strategic direction for multi-x wins beyond per-component optimization.

Categories: subscriber-free internal nets, port-forwarding copy chains, trivial comb fusion into consuming processes, lowering-level dataflow compilation.

### CQ4: Place-based value materialization overhead

The MIR-to-LLVM lowering materializes every intermediate value through stack-allocated places (allocas). Simple operations like loop counter increments produce chains of load-store-load-store through multiple intermediate places. Similarly, boolean comparison results are extended to storage width and then re-narrowed to i1 for branch conditions, producing redundant zext/icmp sequences.

LLVM's mem2reg and instcombine passes fully clean up both patterns -- native code shows tight loops with values in registers and direct branches. There is no runtime cost. However, the excessive IR increases LLVM optimization time and makes unoptimized IR dumps harder to read for debugging.

The root cause is that the lowering always routes through place-backed storage rather than keeping pure SSA values in registers when the value has no storage identity (temporaries, comparison results, loop counters). A value-mode lowering path for expressions that do not need place-backed storage would produce cleaner IR.

## Closed Investigations

These were investigated and did not produce wins. Retained for context so they are not re-attempted.

- **G13 per-activation Stage 2: grouped process dispatch** -- +13% regression. Subscription bypass savings dwarfed by new flush-path overhead. Per-activation overhead (14%) is not the dominant cost; propagation infrastructure (59%) is.
- **G7g: constructor-time propagation plan** -- +8.9% regression. Region-driven executor adds per-slot dispatch overhead that dominates on small graphs. Old unordered fixpoint is already cheap.
- **G7h: compiled acyclic propagation executor** -- +7.8% to +104% regression across three attempts. Generic worklist with dirty-driven gating (~24 Ir per slot) is already near-optimal. Any replacement adds comparable per-op overhead.

Architectural conclusion: propagation cost (59% of total) is not reducible by replacing the worklist loop shape. The win must come from reducing work volume (runtime work elimination), not making each iteration cheaper.

## Reference Data

All measurements use `-c opt` callgrind on AOT binary. See `docs/profiling.md` for methodology.

### Fixtures

| Fixture              | Family     | Parameters                       |
| -------------------- | ---------- | -------------------------------- |
| unpacked-array-read  | storage    | 32768 elements, 4096 iters       |
| unpacked-array-write | storage    | 32768 elements, 4096 iters       |
| clock-pipeline       | scheduling | 8-stage pipe, 500K cycles        |
| fanout-comb          | scheduling | 64-way fanout, 500K cycles       |
| nba-heavy            | scheduling | 32 regs, 500K cycles             |
| edge-sub-dense       | scheduling | 128 procs, 500K cycles           |
| delta-converge       | scheduling | 32-stage comb chain, 500K cycles |
| change-sub-dense     | scheduling | 128 observers, 500K cycles       |
| sparse-wakeup        | scheduling | 256 regs, 500K cycles            |

### Baselines (post split-init-codegen PR #565, 2026-03-16)

delta-converge, change-sub-dense, and sparse-wakeup need callgrind profiling to establish baselines.

| Fixture        | Total Ir | Top self-cost categories                                          |
| -------------- | -------- | ----------------------------------------------------------------- |
| clock-pipeline | 16.45B   | fixpoint 14%, dirty-track 13%, flush 12%, dispatch 8%, suspend 7% |
| fanout-comb    | 18.24B   | suspend 11%, RangeSet 10%, fixpoint 9%, dirty-track 9%, comb 5%   |
| nba-heavy      | 20.73B   | NBA 24%, dispatch 17%, dirty-track 13%, suspend 10%, flush 3%     |
| edge-sub-dense | 73.00B   | NBA 20%, dispatch 16%, dirty-track 13%, suspend 11%, enqueue 4%   |

### Cost category breakdown (% of total Ir)

| Category             | clock-pipeline | fanout-comb | nba-heavy | edge-sub-dense |
| -------------------- | -------------- | ----------- | --------- | -------------- |
| Per-activation       | 14%            | 13%         | 27%       | 27%            |
| NBA lifecycle        | 7%             | --          | 24%       | 21%            |
| Dirty tracking       | 13%            | 19%         | 13%       | 13%            |
| Fixpoint/propagation | 15%            | 9%          | --        | --             |
| Edge/change flush    | 12%            | 1%          | 3%        | 2%             |
| memcpy/memcmp/memset | 9%             | 5%          | 10%       | 12%            |
| Generated code       | 4%             | 5%          | --        | --             |
