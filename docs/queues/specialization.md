# Specialization

> **Queue rules.** Checkboxes at top for progress. Unchecked = gap. Finished items: remove the section, keep one checked line. Every unchecked item must have a short tag/ID for tracking (e.g. G3, F1). Each item should be independently addressable -- it can be investigated and landed as a standalone change. No code in items (no function names, variable names, snippets). Items describe what the gap is, where to look, and why it matters -- enough to re-investigate from scratch. Investigation logs and design details belong in conversation history, not here.

Working queue for migrating Lyra toward the natural model: specialization-shaped compile artifacts, constructor-time instance realization, object-local runtime representation.

For the stable architecture: see [compilation-model.md](../compilation-model.md). For the natural model: see [natural-model.md](../natural-model.md).

## Progress

- [x] A-E -- Shared-body migration (MIR dedup, specialization grouping, per-body codegen, backend API narrowing)
- [x] M2 -- Storage assignment and compile-owned discriminator
- [x] B6 -- HIR ownership split
- [x] m2 -- Instance paths deferred to runtime
- [x] G -- Instance-independent LLVM codegen (per-instance code eliminated, shared-body code paths in place)
- [x] H1-H6 -- Constructor-time realization migration (process/trigger/comb/slot/trace/path/init realization moved behind constructor)
- [x] C1 -- Remove per-instance emitted constructor IR/globals
- [x] R1 -- Runtime instance/object model: instances as objects that own state
- [ ] R2 -- Forwarding as connectivity, not storage redefinition
- [ ] R3 -- Observability and relocation on object-local coordinates
- [ ] R4 -- Process execution context: object-local signal identity
- [ ] R5 -- Design-global coordination API: dirty/subscription on object-scoped identity
- [ ] R6 -- Constructor-to-runtime handoff preserves instance structure
- [ ] T1 -- Topology-independence validation (scaling gates)
- [ ] F1 -- Parallel specialization compilation
  - [x] F1-design -- Parallel ownership model
  - [x] F1-prep Cuts 1-4 -- Per-body HIR/MIR ownership, type/constant arena freeze
  - [ ] m3 -- Param transmission table: replace raw symbol pointers with group-scoped key
  - [ ] F1-impl -- Per-group isolated compilation with deterministic merge
- [ ] F2 -- Specialization caching
- [ ] Documentation: natural model, ownership boundaries, and runtime model docs need alignment
- [ ] CI policy gates: specialization invariants and natural model regression checks

## Dependency Shape

C1 is prerequisite-free and can land independently. It is a constructor-time artifact cleanup within the current design-global storage model; it does not require the R-series.

F1 (parallel compilation) is independent of the R-series and can proceed in parallel.

The R-series items have a partial order:

- **R1 (instance object model)** is the architectural anchor. It defines the runtime object/state ownership contract that all other R-series items must conform to.
- **R2 (forwarding)** re-expresses forwarding under the contract established by R1. R1 and R2 are closely coupled and likely need joint design, but R1 is the authority -- forwarding serves the object model, not the other way around.
- **R4 (signal identity)** depends on R1. Object-local signal identity requires instances to exist as addressable objects.
- **R3 (observability)** depends on R1, R2, and R4. Observable descriptor relocation changes when instance objects, forwarding representation, and signal identity change. Forwarded aliases are currently the dirtiest part of the observability pipeline, so R2's resolution directly shapes R3.
- **R5 (coordination API)** depends on R4. The engine's dirty/subscription API contract follows from how signal identity works.
- **R6 (construction handoff)** depends on R1. The constructor produces whatever runtime objects R1 defines.
- **T1 (validation)** runs after C1 and whichever R-series items have landed.

## C1: Remove per-instance emitted constructor IR/globals

Body-local state sizing metadata is now carried on BodyRealizationDesc (inline/appendix/total bytes). The emitted constructor still drives instance materialization through per-instance IR: one LyraConstructorAddInstance call per instance, one path string global per instance, one param payload global per instance, and topology-sized string cleanup IR in emitted main.

This item replaces those per-instance emitted artifacts with compact construction data (pooled paths, pooled payloads, structural construction program) while preserving the current forwarding-aware realized-placement contract. The slot-byte-offset oracle remains as the realized placement source of truth for this scope.

Separate migration boundary: constructor-time artifact cleanup in the current model, not runtime object-model redesign.

## R1: Runtime instance/object model (completed)

RuntimeInstance is the authoritative owner of module-local storage. Per-instance storage is heap-allocated from per-instance realized sizes. Slot meta uses domain-aware addressing (kDesignGlobal vs kInstanceOwned). All coordination surfaces (trace, subscriptions, connections) resolve slot storage through the instance object model. The design-global arena remains only for package/global state and forwarding compatibility dead space (physical arena shrink deferred to after R2 resolves forwarding off compile-time arena offsets).

## R2: Forwarding as connectivity, not storage redefinition

**Current implementation:** when the forwarding analysis determines a slot is a "forwardable connection relay," the layout pass assigns no storage for that slot in the owning instance (inline_offsets is nullopt in SpecSlotInfo). The codegen classifies it as kForwardedInline or kForwardedContainer in SpecSlotAccessKind and routes access through design_ptr + arena-absolute offset of the canonical owner instead of this_ptr + instance-relative offset. Observable descriptors for these slots carry kObservableFlagStorageAbsolute to signal cross-instance arithmetic at constructor time. The slot_storage_bindings entry is ForwardedStorageAlias rather than OwnedLocalStorage.

**Target:** connectivity is linkage/routing/reference. Every body-local slot owns storage in its instance. When one instance's port is connected to another instance's signal, that relationship is modeled as a reference or routing descriptor between instances, not as the absence of local storage in one instance.

**Why separate:** forwarding is the primary place where connectivity currently redefines object shape. R2 re-expresses forwarding under the object model established by R1. R1 and R2 are closely coupled and likely need joint design, but the architectural authority is R1 -- the object model defines what forwarding must conform to, not the reverse.

Where to look: forwarding_analysis, forwarding_map, SpecSlotAccessKind in codegen_session, GetModuleSlotPointer in context_place, observable_storage_ref, slot_storage_bindings in layout.

## R3: Observability and relocation on object-local coordinates

**Current implementation:** observable descriptors are built per-body with body-relative offsets (this is good). The constructor relocates them to arena-absolute SlotMeta base_off. Forwarded aliases use kObservableFlagStorageAbsolute for cross-instance arithmetic. Trace and snapshot systems index by design-global slot_id with arena-absolute byte offsets.

**Target:** observability works in object-local coordinates. SlotMeta describes a member within an instance, not a byte offset into a design-global arena. Trace coordinates are (instance, local_signal), not flat design-global slot_id.

**Why separate:** observability is already partially body-shaped (the descriptor templates). The remaining work is changing the realized output coordinates and the trace/snapshot consumers. Depends on R1 (instance objects define what coordinates exist), R2 (forwarded aliases are currently the dirtiest part of the observability pipeline -- the cross-instance absolute-offset flags exist specifically because of forwarding), and R4 (signal identity defines what trace coordinates look like).

Where to look: observable_storage_ref, observable_descriptor_utils, constructor observable realization, slot_meta, trace_manager, trace_flush.

## R4: Process execution context: object-local signal identity

**Current implementation:** signal identity requires design-global renumbering: signal_id_offset + local_id produces a design-global slot_id at runtime. Every dirty mark, subscription, and trace event uses this design-global coordinate. design_ptr is available in every process frame header, giving every process access to the entire design's state.

**Target:** signal identity is object-local. A signal is member N of this instance, not design-global slot M. Processes should not need (or have) access to the full design's storage.

**Why separate:** signal identity is the coordinate system that all runtime consumers (dirty tracking, subscriptions, tracing) depend on. Changing it is a distinct contract change from changing instance storage (R1) or observability output (R3).

Where to look: ProcessFrameHeader (signal_id_offset, design_ptr), EmitSignalId, EmitMutationTargetSignalId, LyraMarkDirty.

## R5: Design-global coordination API

**Current implementation:** dirty tracking, subscriptions, and comb fixpoint all use design-global slot_ids as coordinates. UpdateSet vectors are sized to design-global slot count. SlotSubscriptions are indexed by design-global slot_id. This is the outer orchestration layer, but its current public API contract requires every caller to produce design-global slot_ids, leaking design-global identity into every subsystem it touches.

**Target:** the public runtime contract -- the API boundary between codegen/constructor and engine -- accepts object-scoped coordinates (instance + local signal) rather than requiring design-global slot_ids. This is a contract change, not just an internal refactor. The engine may keep flat arrays internally for performance, but the public interface must not force callers to reason in design-global coordinates.

**Why separate:** this is the public runtime contract change, distinct from the instance model (R1) and signal identity (R4). Depends on R4 because the API shape follows from how signal identity works.

Where to look: engine (MarkSlotDirty, MarkDirtyRange), update_set, engine_subscriptions, LyraMarkDirty in simulation.

## R6: Constructor-to-runtime handoff preserves instance structure

**Current implementation:** ConstructionResult contains per-process frames and design-global metadata (realized slot_meta, trigger_meta, comb_meta, trace_signal_meta). Instance identity is dissolved -- no per-instance object or metadata bundle exists in the output. The runtime receives this flat data and indexes it by design-global coordinates.

**Target:** the constructor-to-runtime handoff preserves instance structure. Construction produces instance objects (or per-instance metadata bundles) that the runtime can address per-instance. The runtime should no longer receive only flattened per-design metadata as the primary representation.

**Why separate:** this is the constructor-to-runtime boundary contract. It depends on R1 (what runtime instance objects look like) but is a distinct implementation boundary -- the handoff shape is what makes or breaks instance identity preservation at runtime.

Where to look: ConstructionResult in constructor, SetupAndRunSimulation in simulation, Engine initialization.

## T1: Topology-independence validation

After C1 and the R-series migrations, compile time, optimize time, and object emission time should no longer materially scale with instance count. Emitted artifacts should remain body-/schema-shaped; instance realization should scale only in constructor/runtime work, not in compile-time work.

Validation: re-run the generate-expand compile benchmark at 128 / 1024 / 4096 instances and verify that all three compile phases (lower_llvm, optimize_ir, emit_obj) are flat or near-flat across instance count changes.

## F1: Parallel specialization compilation

See [parallel-compilation.md](../parallel-compilation.md) for the full design. All prep cuts are complete. Next: m3 (param transmission table) then F1-impl (per-group isolated compilation with deterministic merge).

Independent of the R-series. Can proceed in parallel.

## F2: Specialization caching

Not yet designed. Depends on F1 completing the parallel compilation model.

## Documentation: natural model and runtime model alignment

Several docs need alignment with the natural model and R-series runtime model migration:

- pipeline-contract.md still describes types as "language-level" without compile-owned vs constructor-owned ownership distinction
- state-layout.md describes allocation lifecycle without framing as ownership boundaries
- runtime.md, change-propagation.md, and module-hierarchy.md describe mechanisms without referencing the natural model or noting current-state mismatches

The code already implements the correct projection for types. The docs need to catch up with both the type ownership model and the broader natural model / runtime model direction.

## CI policy gates

Specialization invariants lacking CI enforcement:

- Within-group param variance is transmitted per-instance (regression test)
- Compile-owned differences produce distinct specializations (regression test)
- Specialization IR is topology-independent (regression test)
- No instance paths in specialization artifacts (policy check)
- Specialization grouping is deterministic (regression test)
- LLVM artifact count is instance-independent (scaling regression test)

Natural model regression checks (to be enforced as R-series items land):

- No new design-global coordinate as primary identity for instance-local state
- No topology-driven redefinition of instance storage shape
- Constructor-to-runtime handoff preserves instance structure
