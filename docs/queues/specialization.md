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
- [x] R1 -- Runtime instance/object model: two-domain storage, domain-aware slot resolution, process-instance binding
- [x] R2 -- Forwarding as connectivity, not storage redefinition
- [ ] R3 -- Object-local signal identity and coordination API
- [ ] R4 -- Constructor-to-runtime handoff preserves per-instance structure
- [ ] R5 -- Observability/trace/snapshot on object-local coordinates
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

The R-series is a strict sequence: R2 -> R3 -> R4 -> R5.

- **R1 (instance object model, completed)** is the architectural anchor. It established the two-domain storage model, domain-aware slot resolution, and process-instance binding. All subsequent R-series items operate within this contract.
- **R2 (forwarding)** eliminates the last place where connectivity redefines object storage shape. Self-contained: changes layout, codegen, and constructor forwarding paths without touching signal identity or coordination APIs.
- **R3 (signal identity + coordination API)** removes the design-global signal namespace. signal_id_offset is the sole bridge between body-local and design-global coordinates; removing it immediately breaks every dirty/subscription caller, so signal identity and coordination API are one seam, not two.
- **R4 (constructor handoff)** restructures the constructor-to-runtime boundary to preserve per-instance metadata. Depends on R3 because the flat arrays (RealizedSlotMeta, RealizedTriggerMeta, RealizedCombMeta) are indexed by the design-global coordinates that R3 eliminates.
- **R5 (observability)** moves trace/snapshot to object-local coordinates. Last because its target coordinate system -- (instance, local_signal) -- does not exist until R3 defines object-local signal identity, and the trace alias fanout for forwarded signals is cleanest after R2 resolves forwarding.
- **T1 (validation)** runs after C1 and whichever R-series items have landed.

## C1: Remove per-instance emitted constructor IR/globals

Body-local state sizing metadata is now carried on BodyRealizationDesc (inline/appendix/total bytes). The emitted constructor still drives instance materialization through per-instance IR: one LyraConstructorAddInstance call per instance, one path string global per instance, one param payload global per instance, and topology-sized string cleanup IR in emitted main.

This item replaces those per-instance emitted artifacts with compact construction data (pooled paths, pooled payloads, structural construction program) while preserving the current forwarding-aware realized-placement contract. The slot-byte-offset oracle remains as the realized placement source of truth for this scope.

Separate migration boundary: constructor-time artifact cleanup in the current model, not runtime object-model redesign.

## R1: Runtime instance/object model (completed)

Scope was larger than the original name suggested. R1 established:

- **Two-domain storage model.** SlotMeta carries a domain enum (kDesignGlobal vs kInstanceOwned) with domain-specific addressing. Slot meta ABI bumped to v4.
- **Per-instance heap-allocated storage.** Each RuntimeInstance owns inline + appendix regions, sized per-instance from realized sizes (supports parameterized variation).
- **Domain-aware slot resolution.** All coordination surfaces (dirty tracking, subscriptions, trace, connections, comb fixpoint) resolve slot storage through domain-routing in ResolveSlotBytes, dispatching to either the design arena or the owning instance's heap storage.
- **Process-instance binding.** ProcessFrameHeader holds a RuntimeInstance pointer. RuntimeInstance carries module_proc_base and num_module_processes.
- **Observable descriptor domain classification.** Codegen emits body-relative vs owner-absolute storage refs per slot; constructor consumes these to produce domain-tagged SlotMeta.
- **RuntimeInstance as LLVM struct type.** Codegen emits RuntimeInstance and RuntimeInstanceStorage with hard static_assert binary-contract enforcement.
- **Explicit transitional seam.** signal_id_offset is documented as not part of the object's semantic identity, targeted for removal in R3.

The design-global arena remains only for package/global state.

## R2: Forwarding as connectivity, not storage redefinition (completed)

Forwarding no longer redefines object storage shape. Every body-local slot owns local storage in its instance. Forwarding survives only as connection-analysis routing metadata (relay-candidate classification) for future direct-routing optimization.

- **Storage plane.** ForwardedStorageAlias, StorageOwnerSlotId, and the SlotStorageBinding variant deleted. Layout allocates every body-local slot unconditionally. storage_owner_slot_id removed from DesignLayout. Offset queries (GetBodyOffset, GetInstanceOffset) are total for all body-local slots. Design arena contains only package/global state (forwarding dead space removed).
- **Access plane.** kForwardedInline and kForwardedContainer deleted from SpecSlotAccessKind. All module-local pointer paths go through this_ptr + local offset. No module-local access routes through design_ptr. Observable descriptors for body slots are always body-relative (ObservableOwnerAbsoluteStorageRef deleted).
- **Runtime slot resolution.** ResolveSlotBytes uses direct domain switch only. No owner-chase for instance-owned slots. R2 self-ownership invariant enforced (storage_owner_slot_id == slot_id for all instance-owned slots).
- **Connection analysis.** ForwardingMap deleted. Replaced by AnalyzeConnections producing ConnectionAnalysisResult with original-slot-ID connection edges, per-slot usage summaries, and relay-candidate classification. Connection entries use original slot IDs; no canonical-owner aliasing or identity-edge elimination.
- **Trace.** Forwarded-alias dirty-set validation and alias fanout loop removed from trace flush. Direct per-slot trace path only.

## R3: Object-local signal identity and coordination API

Merges the former R4 (signal identity) and R5 (coordination API) because they share a single seam: signal_id_offset is the sole bridge between body-local and design-global coordinates, and removing it immediately breaks every dirty/subscription/trace caller. No stable intermediate state exists where signal identity is object-local but the coordination API still requires design-global slot_ids.

**Current state:** signal identity requires design-global renumbering. signal_id_offset + local_id produces a design-global slot_id at runtime. Every dirty mark, subscription install, and trace event uses this design-global coordinate. The public engine API (MarkSlotDirty, MarkDirtyRange) requires design-global slot_ids. UpdateSet vectors, SlotSubscriptions, conn_trigger_map\_, comb_trigger_map\_ are all sized and indexed by design-global slot count. design_ptr in every process frame header gives every process access to the entire design's state.

**Target:** signal identity is object-local -- a signal is member N of this instance, not design-global slot M. The public runtime contract (the API boundary between codegen/constructor and engine) accepts object-scoped coordinates (instance + local signal). The engine may keep flat arrays internally for performance, but the public interface must not force callers to reason in design-global coordinates. signal_id_offset is removed from RuntimeInstance.

Where to look: ProcessFrameHeader (signal_id_offset, design_ptr), EmitSignalId, EmitMutationTargetSignalId, LyraMarkDirty, engine (MarkSlotDirty, MarkDirtyRange), update_set, engine_subscriptions.

## R4: Constructor-to-runtime handoff preserves per-instance structure

**Current state:** the constructor produces flat per-design arrays for all metadata categories: RealizedSlotMeta (indexed by global slot_id), RealizedTriggerMeta (global process_id + global slot_id), RealizedCombMeta (global process_id), RealizedTraceSignalMeta (global trace signal index), process metadata (global process_id). Instance identity is dissolved in the handoff -- ConstructionResult contains RuntimeInstance objects but all metadata is flattened.

**Target:** the constructor-to-runtime handoff preserves per-instance metadata bundles. The runtime receives instance-structured data, not only flattened per-design metadata. Per-instance process lists, trigger metadata, and slot metadata are grouped by instance.

**Depends on R3:** the flat arrays are indexed by design-global coordinates that R3 eliminates. Until the new object-scoped coordinate system exists, per-instance metadata bundles cannot be cleanly defined.

Where to look: ConstructionResult in constructor, RealizedSlotMeta/TriggerMeta/CombMeta/TraceSignalMeta, SetupAndRunSimulation in simulation, Engine initialization.

## R5: Observability/trace/snapshot on object-local coordinates

**Current state:** observable descriptors are built per-body with body-relative offsets (this is correct). The constructor relocates them to design-global SlotMeta entries. Trace and snapshot systems index by design-global slot_id. Forwarded alias trace fanout uses storage_owner_slot_id alias groups keyed by global slot_id. TraceSignalMetaRegistry is a flat dense vector.

**Target:** observability works in object-local coordinates. SlotMeta describes a member within an instance, not a byte offset into a design-global arena. Trace coordinates are (instance, local_signal), not flat design-global slot_id.

**Why last:** the target coordinate system -- (instance, local_signal) -- does not exist until R3 defines object-local signal identity. Forwarded alias trace fanout is cleanest after R2 resolves forwarding. The observable descriptor templates are already body-shaped; the remaining work is the realized output coordinates and trace/snapshot consumers.

Where to look: observable_storage_ref, observable_descriptor_utils, constructor observable realization, slot_meta, trace_manager, trace_flush.

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
