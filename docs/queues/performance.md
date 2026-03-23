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
- [x] CQ1: Packed storage view bulk init lowering quality
- [x] CQ2: Packed storage view 2-state unknown-plane elision
- [x] CQ3: Packed storage view deferred-notification dead code elision
- [x] Commit-boundary model: visibility/commit boundary definition
- [x] Commit-boundary model: multi-segment activation-local support
- [ ] G16-obs: Relay elimination observation contract
- [ ] G16a: Specialization-local relay elimination
- [ ] G16b: Suppress dirty-mark emission for subscriber-free slots
- [ ] G14: NBA arena (bump-allocator for value/mask bytes)
- [ ] G4b: Per-activation atomic stores (conditional on signal handler)
- [ ] CB1: Facts-to-contract model for activation-local statements
- [ ] CB2: Region-local canonical dependency analysis
- [ ] CB3: Commit-boundary model delayed-commit register promotion
- [ ] G16c: Assign-chain single-consumer comb fusion
- [ ] G5 remaining: RangeSet linear scan + ClearDelta per-region reset
- [ ] G15: Signal flush helpers
- [ ] CQ4: Place-based value materialization overhead
- [ ] CQ5: Loop-level unknown-plane bulk clear for 2-state overwrite loops
- [ ] G16a-cross: Cross-boundary relay collapse (depends on H4-H5)
- [ ] G16d: Realized-graph dead node pruning (depends on H3-H5)

## Active Gaps

### G16: Runtime work elimination (volume reduction)

The dominant runtime cost is propagation infrastructure (59% of total Ir on clock-pipeline). Closed investigations (G7g, G7h, G13 Stage 2) proved this cost is not reducible by changing the propagation loop shape -- the worklist is already near-optimal per slot. The win must come from reducing the number of slots, connections, and comb kernels that exist in the runtime graph.

The root cause: the lowering materializes every SV semantic object (port, net, assign target) with full runtime identity -- slot allocation, dirty-tracking subscription, connection evaluation, propagation worklist entry. A large fraction of these are pure relay / forwarding nodes with no computational or observability purpose. Eliminating them reduces work volume across all cost categories (dirty marks, connection memcmp/memcpy, propagation iterations, flush checks) proportionally.

clock-pipeline evidence (500K cycles, `-vvv --stats`): 14 port-binding relay slots identified as provable pass-throughs by the existing forwarding analysis. These 14 relays produce 13.4M connection evaluations (memcmp + memcpy), inflate propagation from 1-2 iterations to 5 max per call, and generate proportional dirty-mark overhead. 58% of NBA entries are elided (value unchanged), with valid_reg relay chains as the dominant source. The structural cause is the `assign data_out = data_reg` pattern repeated across 8 pipeline stages -- each creates an intermediate slot with full runtime identity that is a pure copy.

A relay node is a slot that exists only to forward a value: single-writer identity copy, no process or comb triggers, all consumers are connections. Such slots can be eliminated by redirecting downstream connections to the source.

Long-term direction: compile pure-dataflow regions into single functions evaluated once per propagation call. This subsumes relay elimination, subscriber-free net suppression, and comb fusion. Full cross-boundary scope requires realized graph metadata (H4-H5). Sequencing: relay elimination first, then subscriber-free suppression and comb fusion, then full dataflow compilation.

### G16-obs: Relay elimination observation contract

The forwarding analysis already proves structural legality for all relay candidates (single writer, identity copy, no process/comb triggers, pure forwarding). The remaining blocker is observability: whether a relay slot may be observed by trace/VCD, `$display`/`$monitor`, or hierarchical reference.

Define a contract: relay slots are elimination-eligible by default unless explicitly observed (via trace selection, display operand, or hierarchical reference). Build the escape analysis that identifies which slots are observed. This is a design decision plus an audit of existing observation paths, not a large implementation.

This gates G16a. Without this contract, the transform cannot proceed.

### G16a: Specialization-local relay elimination

Within a single module body, eliminate relay slots from port-binding assigns. The compiler identifies relay candidates from the body's own connection table and trigger tables. The existing forwarding analysis proves structural conditions. After the observation contract (G16-obs) establishes legality, implement the transform: redirect downstream connections to read from the relay's source slot, suppress the relay slot and its assign process.

Scope is strictly specialization-local. No cross-module knowledge, no realization dependency, no H-series prerequisite.

### G16b: Suppress dirty-mark emission for subscriber-free slots

First cut: at codegen time, identify slots with no process subscribers and no comb triggers. For these slots, suppress the dirty-mark call emitted by blocking-assign and NBA-commit paths. The slot still exists in the design state (connections read from it by raw byte offset), but it does not enter the dirty set, does not appear in the flush scan, and does not generate propagation worklist entries.

This is narrower than full slot elimination (which would remove the slot from the arena). Suppressing dirty marks is a codegen-level change with no layout impact. Full slot elimination is a separate, harder follow-up.

### G14: NBA arena

Replace per-entry heap allocation for NBA value/mask bytes with a bump-allocator arena. Estimated 5-8% on nba-heavy/edge-sub-dense, 3-5% on clock-pipeline. Arena is append-only within a region, reset at region boundary.

### G4b: Per-activation atomic stores

Every process activation writes several atomic stores (phase, activation sequence, current process ID) for the signal-handler crash dump path. These are memory-order-release stores on every dispatch and return, adding per-activation overhead even when no signal handler is installed. Conditional on whether a signal handler is registered, these could be skipped entirely or downgraded to plain stores.

### CB1: Facts-to-contract model for activation-local statements

Replace the taxonomy-centered activation-local boundary model (BoundaryKind -> TranslateBoundary -> SyncAction) with a facts-to-contract pipeline. Semantic analysis produces factual descriptions of each statement's canonical-state interaction relevant to managed coherence. Contract planning converts those facts into sync/reload decisions. Executor consumes the decisions mechanically.

The v1 model used a vague kObservation bucket that drifted from its real correctness requirements: display effects, module-slot assigns, and file I/O were all classified as observation boundaries requiring pre-sync, even though they do not interact with canonical storage in a way that affects managed-shadow coherence. The facts model eliminates these unnecessary syncs.

Adjacent fix: the runtime flush architecture had a hidden cross-slot ordering dependency between rebind watchers and edge evaluation. When a rebind dependency slot and its target slot became dirty in the same pass, flush order determined whether the edge trigger evaluated at the stale or updated observation point. Fixed by splitting FlushSignalUpdates into two global phases: rebind first, then edge/change/container. This makes the flush order-independent and removes a correctness constraint that was previously masked by the old per-statement sync.

### CB2: Region-local canonical dependency analysis

Per-region analysis of which managed slots have been modified since the last sync. This is the proof that tells the contract planner whether a sync can be skipped for a specific managed slot at a given contract point. The current model syncs all managed slots at every contract point; CB2 would allow skipping sync for slots whose shadow has not been modified. Must be specialization-local.

### CB3: Commit-boundary model delayed-commit register promotion

Keep eligible slot-backed scalars in registers across a region, commit back to slot storage only at required boundaries. This is the downstream optimization that uses the commit-boundary definition and region-local analysis. Downstream of CB1 and CB2.

### G16c: Assign-chain single-consumer comb fusion

When a comb kernel writes a slot that is read by exactly one downstream comb kernel (no other consumers, no subscribers, no trace), fuse the two into one kernel with the intermediate slot eliminated. First PR restricted to: both kernels are single-block, single-statement, no control flow, no side effects. This covers the `assign a = f(x); assign b = g(a)` chain pattern common in pipeline combinational logic.

### G5 remaining: RangeSet + ClearDelta

RangeSet linear scan is 10% on fanout-comb (many sub-slot dirty ranges, 0% on clock-pipeline which uses full-slot marks only). ClearDelta resets per-slot vectors on every region boundary (3% on clock-pipeline).

### G15: Signal flush helpers

Per-slot flush functions are 2-12% depending on fixture. Possible inlining or specialization of the hot flush path.

### CQ4: Place-based value materialization overhead

The MIR-to-LLVM lowering materializes every intermediate value through stack-allocated places (allocas). Simple operations like loop counter increments produce chains of load-store-load-store through multiple intermediate places. Similarly, boolean comparison results are extended to storage width and then re-narrowed to i1 for branch conditions, producing redundant zext/icmp sequences.

LLVM's mem2reg and instcombine passes fully clean up both patterns -- native code shows tight loops with values in registers and direct branches. There is no runtime cost. However, the excessive IR increases LLVM optimization time and makes unoptimized IR dumps harder to read for debugging.

The root cause is that the lowering always routes through place-backed storage rather than keeping pure SSA values in registers when the value has no storage identity (temporaries, comparison results, loop counters). A value-mode lowering path for expressions that do not need place-backed storage would produce cleaner IR.

### CQ5: Loop-level unknown-plane bulk clear for 2-state overwrite loops

When a loop performs a full 2-state overwrite into contiguous elements of 4-state packed storage, the current lowering emits a per-element unconditional zero store to the unknown plane inside the hot loop. This is correct but suboptimal: the inner loop touches the unknown-plane cache lines on every iteration even though the entire unknown-plane region could be cleared once before the loop.

The target shape is to hoist unknown-plane clearing out of the per-element loop. Before the loop body, emit a bulk clear (memset or equivalent) of the unknown-plane byte range that the loop will overwrite. Inside the loop, use no per-element unknown-plane instructions at all -- the inner loop becomes pure value-plane work.

This requires reasoning at the loop or region level, not at the per-store level. The lowering must prove: (a) the loop writes a contiguous range of elements, (b) all writes in the loop body are provably 2-state, (c) the unknown-plane byte range for that element range can be computed at loop entry. This is a different kind of analysis than the per-store CQ2 domain propagation or the CQ3 deferred-notification elision.

### G16a-cross: Cross-boundary relay collapse (depends on H4-H5)

After H4-H5 move connection and trigger metadata behind constructor-time expansion, the runtime constructor sees the full realized connection graph. It can then collapse relay chains spanning module boundaries (e.g., Top-level `assign d0 = counter` intermediaries where the Top-level slot is a pure relay between a child output and a sibling input).

Requires constructor-owned connection and trigger metadata. Cannot proceed until H4-H5 are complete.

### G16d: Realized-graph dead node pruning (depends on H3-H5)

After the constructor owns the realized process and connection graph, it can identify and eliminate dead runtime nodes: processes whose outputs feed nothing, slots with no downstream consumers, connection chains to unconnected ports. This is the constructor-time analog of dead code elimination.

Requires constructor-owned process graph with realized connectivity (H3-H5). Cannot proceed until those are complete.

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
