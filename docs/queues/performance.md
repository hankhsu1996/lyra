# Runtime Performance

> **Queue rules.** Checkboxes at top for progress. Unchecked = gap. Finished items: remove the section, keep one checked line. No code in items (no function names, variable names, snippets). Items describe what the gap is, where to look, and why it matters -- enough to re-investigate from scratch. Investigation logs and design details belong in conversation history, not here.

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
- [ ] Packed storage view Stage 4: whole-value materialization boundary
- [x] Commit-boundary model: visibility/commit boundary definition
- [x] Commit-boundary model: multi-segment activation-local support
- [ ] Commit-boundary model: boundary taxonomy refinement
- [ ] Commit-boundary model: region-local read/write/observation analysis
- [ ] Commit-boundary model: delayed-commit register promotion
- [ ] G14: NBA arena (bump-allocator for value/mask bytes)
- [ ] G5 remaining: RangeSet linear scan + ClearDelta per-region reset
- [ ] Signal flush helpers optimization
- [ ] G4b: Per-activation atomic stores (conditional on signal handler)
- [ ] Runtime work elimination (slot/connection/comb fusion)

## Active Gaps

### Commit-boundary model: boundary taxonomy refinement

The v1 boundary model collapses two different concepts into kObservation: read-only external observation (e.g. $display reads format args) and notification-visible publication (module-slot write emits dirty-mark that triggers runtime subscriptions). These are not the same thing. A $display reads but does not trigger subscriptions. A module-slot write does not read managed slots but its dirty-mark can synchronously expose them through subscriber callbacks.

The v1 fix treats all module-slot writes as kObservation boundaries, which is correct but overly conservative. It forces a sync before every non-managed module-slot write, even when no managed slot has been modified since the last sync. The clean model should distinguish notification-publication boundaries from read-only observation boundaries, so the contract can decide whether sync is actually needed based on the managed-slot dirty state.

### Commit-boundary model: region-local read/write/observation analysis

Analysis facts for the region between commit boundaries. Not just which roots become dirty, but which slot-backed values are read, written, or potentially observed inside the region. This is the proof that tells us whether a slot-backed scalar is safe to keep as local temporary state until the next commit boundary. The current deferred-notification analysis tracks notified roots (write set) but not the read set or observation set. Must be specialization-local.

### Commit-boundary model: delayed-commit register promotion

Keep eligible slot-backed scalars in registers across a region, commit back to slot storage only at required boundaries. This is the downstream optimization that uses the commit-boundary definition and region-local analysis. It is not the first thing to build.

### Packed storage view Stage 4: whole-value materialization boundary

Whole-value packed stores (initialization, direct assignment) still go through the old commit path outside the packed storage view module. The architecture goal is to route all packed storage access through the module -- localized subview access for element operations, explicit materialization boundary for whole-value operations.

See the commit layer for packed values and the stubs in the packed storage view module. Callers include init effects and array/struct initialization in the type ops layer.

### G14: NBA arena

Replace per-entry heap allocation for NBA value/mask bytes with a bump-allocator arena. Estimated 5-8% on nba-heavy/edge-sub-dense, 3-5% on clock-pipeline. Arena is append-only within a region, reset at region boundary.

### G5 remaining: RangeSet + ClearDelta

RangeSet linear scan is 10% on fanout-comb (many sub-slot dirty ranges, 0% on clock-pipeline which uses full-slot marks only). ClearDelta resets per-slot vectors on every region boundary (3% on clock-pipeline).

### Signal flush helpers

Per-slot flush functions are 2-12% depending on fixture. Possible inlining or specialization of the hot flush path.

### Runtime work elimination

Many runtime slots, connections, and comb kernels exist because the lowering materializes every SV semantic object with full signal/subscriber/dirty-tracking identity. A large fraction is pure dataflow that could be fused or eliminated. This is the strategic direction for multi-x wins beyond per-component optimization.

Categories: subscriber-free internal nets, port-forwarding copy chains, trivial comb fusion into consuming processes, lowering-level dataflow compilation.

## Closed Investigations

These were investigated and did not produce wins. Retained for context so they are not re-attempted.

- **G13 per-activation Stage 2: grouped process dispatch** -- +13% regression. Subscription bypass savings dwarfed by new flush-path overhead. Per-activation overhead (14%) is not the dominant cost; propagation infrastructure (59%) is.
- **G7g: constructor-time propagation plan** -- +8.9% regression. Region-driven executor adds per-slot dispatch overhead that dominates on small graphs. Old unordered fixpoint is already cheap.
- **G7h: compiled acyclic propagation executor** -- +7.8% to +104% regression across three attempts. Generic worklist with dirty-driven gating (~24 Ir per slot) is already near-optimal. Any replacement adds comparable per-op overhead.

Architectural conclusion: propagation cost (59% of total) is not reducible by replacing the worklist loop shape. The win must come from reducing work volume (runtime work elimination), not making each iteration cheaper.

## Reference Data

All measurements use `-c opt` callgrind on AOT binary. See `docs/profiling.md` for methodology.

### Fixtures

| Fixture              | Family     | Parameters                 |
| -------------------- | ---------- | -------------------------- |
| unpacked-array-read  | storage    | 32768 elements, 4096 iters |
| unpacked-array-write | storage    | 32768 elements, 4096 iters |
| clock-pipeline       | scheduling | 8-stage pipe, 500K cycles  |
| fanout-comb          | scheduling | 64-way fanout, 500K cycles |
| nba-heavy            | scheduling | 32 regs, 500K cycles       |
| edge-sub-dense       | scheduling | 128 procs, 500K cycles     |

### Baselines (post split-init-codegen PR #565, 2026-03-16)

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
