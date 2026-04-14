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
- [x] R3 -- Object-local signal identity and coordination API
- [x] R4 -- Constructor-to-runtime handoff preserves per-instance structure
- [x] R5 -- Observability/trace/snapshot on object-local coordinates
- [ ] B -- Recipe model: non-local access, connections, construction (see [compilation-model.md](../compilation-model.md))
  - [x] B1 -- Freeze compile-time contract boundary: CompiledModuleHeader/Body/Specialization types, core recipe types (ExternalRefId, ConnectionRecipe, ChildBindingSiteId), header-only dependency rule, documented descendant-path direction (NonLocalTargetRecipe deferred to B2)
  - [x] B2 -- Compile-time recipe lowering: hierarchical refs to external ref handles, connections to body-local recipes (simple and expression), parent-child port via CompiledModuleHeader; delete ResolvedBindingPlan, ResolvedKernelBinding, backend kernel adapter
  - [x] V3a -- Remove backend non-local access bridge: stop rebuilding non-local reads/writes as synthetic global-place accesses; make backend non-local access consume resolved target object + target local slot directly
  - [x] V3b+V3c -- Unify hierarchical read/write, sensitivity, trigger, and index-plan transport on topology-resolved non-local identity: delete ResolveHierarchicalRef and cross-instance-place-based B2 gate; delete prebinding sensitivity resolution; carry unresolved external refs through MIR and normalize at backend using ResolvedExternalRefBinding; extend behavioral dirty-propagation bitmap to cover cross-instance subscribers
  - [ ] V3d -- Remove body-lowering cross-instance global-place lookup: delete body-lowering dependence on cross-instance symbol-to-global lookup once all remaining hierarchical lowering paths no longer require early global-slot resolution
  - [ ] V4 -- Remove flat/global runtime transport for instance-owned state: replace the remaining mixed local/global metadata and scheduling encodings with object-local runtime forms, remove design-level connection/init transport, and complete object-local connection execution
    - [x] V4a -- Split mixed-domain slot resolver: rename to design-wide, delete from connection path, make FFI global-only
    - [x] V4b -- Ext-ref storage byte offset: replace flat storage_slot with body-relative target_byte_offset in ext-ref binding
    - [x] V4c -- Connection object/member shape: replace flat src/dst/trigger_slot_id with object_index + byte_offset in connection descriptors
    - [x] V4d -- Delete mixed-domain resolvers and forwarding analysis
    - [ ] V4e -- Connection body-local recipes: replace design-global connection descriptor table with body-local owner-relative recipes (Self / ChildOrdinal(K)) attached to body descriptor package; constructor materializes runtime connection descriptors from live parent/child pointers during assembly
    - [ ] V4f -- Ext-ref direct-pointer binding: replace serialized target_instance_id with RuntimeExtRefBinding carrying direct RuntimeInstance\*; atomic across runtime field type, LLVM binding type, constructor materialization, and all codegen/FFI consumers
    - [ ] V4g -- Constructor object model: current constructor is a flat-loop replay machine with no parent-child assembly context; connections bypass the constructor entirely; target model is body-local recipes consumed by constructor with parent-local child pointer view
    - [ ] V4h -- SlotMeta owner_instance_id split: separate truly-global slot metadata from per-instance local metadata; eliminate owner_instance_id as a routing key
    - [ ] V4i -- Connection analysis graph refactor: replace flat SlotId indexing in relay elimination with object/member endpoint keys
- [ ] T1 -- Topology-independence validation (scaling gates)
- [ ] F1 -- Parallel specialization compilation
  - [x] F1-design -- Parallel ownership model
  - [x] F1-prep Cuts 1-4 -- Per-body HIR/MIR ownership, type/constant arena freeze
  - [ ] m3 -- Param transmission table: replace raw symbol pointers with group-scoped key
  - [ ] F1-impl -- Per-group isolated compilation with deterministic merge
- [ ] F2 -- Specialization caching
- [x] I1 -- Constructor-time container construction recipes
- [ ] Compilation unit isolation enforcement (I-1 through I-5)
- [ ] X -- XIR layer introduction (execution-model IR between HIR and MIR)
  - [ ] X1 -- XIR design: define core types, compilation-unit structure, C++ projection contract
  - [ ] X2 -- XIR vertical slice: one representative feature (e.g., deferred assertions) through HIR -> XIR -> MIR
  - [ ] X3 -- XIR validation: C++ projection proves execution-model shape for the slice
  - [ ] X4 -- Layer rename: XIR -> MIR, current MIR -> LIR (after shape is proven)
- [x] Documentation: XIR introduction, per-compilation-unit model, four-layer pipeline across architecture docs
- [ ] CI policy gates: specialization invariants and natural model regression checks

## Dependency Shape

C1 is prerequisite-free and can land independently. It is a constructor-time artifact cleanup within the current design-global storage model; it does not require the R-series.

F1 (parallel compilation) is independent of the R-series and B-series and can proceed in parallel.

The R-series (R1-R5) is complete. All instance-owned state uses object-local coordinates end-to-end.

B1 is prerequisite-free (investigation + type boundary). B2 depends on B1 (compile-time migration). V3a depends on B2 (backend bridge). V3b+V3c depends on V3a (body-lowering read/write + sensitivity/trigger/index-plan transport, merged because write-path and sensitivity-path identity must move to the same slot-ID space atomically). V3d depends on V3b+V3c (cross-instance lookup deletion). V4 depends on V3d (runtime transport).

### Current migration boundary

- Backend non-local read/write bridge is already separable from the remaining lowering/runtime work.
- Body-lowering hierarchical read/write fallback is separable from sensitivity/index-plan transport migration.
- Flat/global runtime transport for instance-owned state remains a later migration surface.

- **T1 (validation)** runs after C1, the R-series, V3a-V3d, and V4.

## X: XIR layer introduction

XIR is a new execution-model IR between HIR and the current MIR. It exists because the current pipeline has no formal home for execution-model concepts (closures, observers, wait models, object composition), so lowering code invents them ad-hoc and the results drift toward design-global tables and ID-based orchestration.

**Temporary naming:** XIR is a working name. The long-term plan is to rename XIR -> MIR and current MIR -> LIR, establishing the four-layer pipeline (HIR / MIR / LIR / LLVM IR). The rename happens after the XIR shape is proven on at least one vertical slice (X2/X3). Architecture docs use whichever name is current at each rename.

**Dependency:** X1 is prerequisite-free (design work). X2 depends on X1. X3 depends on X2. X4 depends on X3 proving the shape. X is independent of the V-series and F-series.

**Methodology anchor:** XIR must be projectable into a C++ object model (module ~ class, process ~ bound behavior, signal ~ member, hierarchy ~ subobjects). The C++ projection is a methodology tool and design oracle, not a product backend. If an execution concept cannot project cleanly, the XIR shape is wrong.

## C1: Remove per-instance emitted constructor IR/globals

Body-local state sizing metadata is now carried on BodyRealizationDesc (inline/appendix/total bytes). The emitted constructor still drives instance materialization through per-instance IR: one LyraConstructorAddInstance call per instance, one path string global per instance, one param payload global per instance, and topology-sized string cleanup IR in emitted main.

This item replaces those per-instance emitted artifacts with compact construction data (pooled paths, pooled payloads, structural construction program) while preserving the current forwarding-aware realized-placement contract. The slot-byte-offset oracle remains as the realized placement source of truth for this scope.

Separate migration boundary: constructor-time artifact cleanup in the current model, not runtime object-model redesign.

## R1: Runtime instance/object model (completed)

Scope was larger than the original name suggested. R1 established:

- **Two-domain storage model.** SlotMeta carries a domain enum (kDesignGlobal vs kInstanceOwned) with domain-specific addressing. Slot meta ABI bumped to v4.
- **Per-instance heap-allocated storage.** Each RuntimeInstance owns inline + appendix regions, sized per-instance from realized sizes (supports parameterized variation).
- **Domain-aware slot resolution.** All coordination surfaces (dirty tracking, subscriptions, trace, connections, comb fixpoint) resolve slot storage through domain-routing in ResolveGlobalSlotBase, dispatching to either the design arena or the owning instance's heap storage.
- **Process-instance binding.** ProcessFrameHeader holds a RuntimeInstance pointer. RuntimeInstance carries module_proc_base and num_module_processes.
- **Observable descriptor domain classification.** Codegen emits body-relative vs owner-absolute storage refs per slot; constructor consumes these to produce domain-tagged SlotMeta.
- **RuntimeInstance as LLVM struct type.** Codegen emits RuntimeInstance and RuntimeInstanceStorage with hard static_assert binary-contract enforcement.
- **Explicit transitional seam.** signal_id_offset is documented as not part of the object's semantic identity, targeted for removal in R3.

The design-global arena remains only for package/global state.

## R2: Forwarding as connectivity, not storage redefinition (completed)

Forwarding no longer redefines object storage shape. Every body-local slot owns local storage in its instance. Forwarding survives only as connection-analysis routing metadata (relay-candidate classification) for future direct-routing optimization.

- **Storage plane.** ForwardedStorageAlias, StorageOwnerSlotId, and the SlotStorageBinding variant deleted. Layout allocates every body-local slot unconditionally. storage_owner_slot_id removed from DesignLayout. Offset queries (GetBodyOffset, GetInstanceOffset) are total for all body-local slots. Design arena contains only package/global state (forwarding dead space removed).
- **Access plane.** kForwardedInline and kForwardedContainer deleted from SpecSlotAccessKind. All module-local pointer paths go through this_ptr + local offset. No module-local access routes through design_ptr. Observable descriptors for body slots are always body-relative (ObservableOwnerAbsoluteStorageRef deleted).
- **Runtime slot resolution.** ResolveGlobalSlotBase uses direct domain switch only. No owner-chase for instance-owned slots. R2 self-ownership invariant enforced (storage_owner_slot_id == slot_id for all instance-owned slots).
- **Connection analysis.** ForwardingMap deleted. Replaced by AnalyzeConnections producing ConnectionAnalysisResult with original-slot-ID connection edges, per-slot usage summaries, and relay-candidate classification. Connection entries use original slot IDs; no canonical-owner aliasing or identity-edge elimination.
- **Trace.** Forwarded-alias dirty-set validation and alias fanout loop removed from trace flush. Direct per-slot trace path only.

## R3: Object-local signal identity and coordination API (completed)

Signal identity for instance-owned signals is now object-local. The design-global signal namespace is removed as a semantic concept for instance-owned coordination.

- **Identity layer.** LocalSignalId, GlobalSignalId, ObjectSignalRef, DenseSignalCoord defined in signal_coord.hpp. signal_id_offset removed from RuntimeInstance, ObserverContext, ProcessDescriptorEntry. Replaced by local_signal_coord_base (engine-private dense coordination base).
- **Codegen.** SignalCoordExpr carries semantic domain (kLocal/kGlobal). EmitSignalCoord returns typed coordinates. Generated code passes (engine_ptr, instance_ptr, local_id) for local signals, (engine_ptr, global_id) for global signals. No codegen path forms signal_id_offset + local_id. Inline first-dirty-seen bitmap path removed.
- **Runtime helpers.** Typed local/global helper pairs for dirty marking, packed/string store, NBA scheduling, trace observation, container/aggregate notification. All local helpers take (engine_ptr, instance_ptr, local_id) -- valid in both process bodies and module-scoped user functions.
- **Engine API.** Typed overloads: MarkDirty(ObjectSignalRef), MarkDirty(GlobalSignalId), ScheduleNba, IsTraceObserved. Flat-slot conversion is engine-internal via ToFlatSlotId.
- **Dense coordination.** Engine owns base assignment via AssignDenseCoordinationBases (scans slot meta, validates contiguous local-id ordering). Module-scoped user function ABI extended to carry instance_ptr.
- **Constructor.** Trigger and comb realization pass body-local slot ids with kFlagBodyLocal / kCombTriggerFlagBodyLocal. Engine init does the relocation. Observable/slot-meta relocation remains (R5 scope).
- **Remaining follow-up.** V3b+V3c resolved the trigger/plan transport identity mismatch by normalizing unresolved external refs at backend emission time using ResolvedExternalRefBinding. The remaining flat trigger installation path (WaitTriggerRecord/IndexPlanOp formats) is consumed by the runtime and tracked by V4 (runtime transport).

## R4: Constructor-to-runtime handoff preserves per-instance structure (completed)

The constructor-to-runtime boundary now preserves per-instance structure for all metadata categories. The constructor builds only design-global/package observable metadata; all instance-owned metadata is engine-derived from bundles.

- **Slot meta.** Constructor narrowed to design-global-only slot entries. Engine builds instance-owned SlotMeta from bundles via shared observable walk (ForEachBundleObservable) in InitModuleInstancesFromBundles. Dense coordination base assignment derived from bundle observable descriptors, not from a pre-built flat slot registry.
- **Trace signal meta.** Constructor builds design-global/package trace entries only. Instance trace signals are built from the same shared observable walk and returned from InitModuleInstancesFromBundles. Final merged TraceSignalMetaRegistry is assembled by BuildMergedTraceSignalMeta in simulation.cpp before InitTraceSignalMeta.
- **Variable inspection.** LyraResolveSlotAddress deleted. Test framework variable inspection restructured: codegen-time InspectionPlan with typed placements (DesignGlobalPlacement, InstanceOwnedPlacement) resolved at compile time. EmitVariableInspection emits per-domain IR with runtime abi_ptr null guard for instance-owned variables. No post-engine-destruction slot resolution needed.
- **ABI codegen.** Canonical GetRuntimeAbiStructType in runtime_abi_codegen.cpp (single source of truth). EmitLoadAbiInstancePtr and EmitInstanceOwnedByteAddress as canonical instance storage resolution helpers.
- **Type unification.** SlotId moved to common::SlotId (common/slot_id.hpp), InstanceByteOffset moved to common::InstanceByteOffset (common/byte_offset.hpp). Both used across MIR, codegen, and runtime layers without aliases or compatibility shims.

## R5: Observability/trace/snapshot on object-local coordinates (completed)

Instance-owned observability uses (RuntimeInstance\*, LocalSignalId) identity end-to-end. ToFlatSlotId and local_signal_coord_base deleted.

- **Container split.** Per-instance RuntimeInstanceObservability with BodyObservableLayout, LocalUpdateSet, and instance-local subscriptions for instance-owned signals. Design-global UpdateSet and signal_subs narrowed to package/global only.
- **Slot metadata split.** InstanceSlotMeta and BodyTraceMeta indexed by LocalSignalId for instance-owned signals. SlotMetaRegistry narrowed to design-global entries only.
- **Domain-typed trace.** GlobalValueChange / LocalValueChange events in trace_event.hpp. Separate flush paths: FlushGlobalDirtySlotsToTrace / FlushLocalDirtySlotsToTrace. Hierarchical names composed at sink boundary via ComposeHierarchicalTraceName, not pre-flattened.
- **Observer ABI.** ObserverContext carries this_ptr and InstanceId for direct object-local addressing. No flat-base ABI argument.

## T1: Topology-independence validation

After C1 and the R-series migrations, compile time, optimize time, and object emission time should no longer materially scale with instance count. Emitted artifacts should remain body-/schema-shaped; instance realization should scale only in constructor/runtime work, not in compile-time work.

Validation: re-run the generate-expand compile benchmark at 128 / 1024 / 4096 instances and verify that all three compile phases (lower_llvm, optimize_ir, emit_obj) are flat or near-flat across instance count changes.

**Current status (post-I1):** LLVM IR line count is constant (1051 lines at all N). But optimize_ir and emit_obj still scale linearly because four emitted LLVM constant globals grow with instance count:

- `__lyra_slot_byte_offsets` -- `[N*slots_per_instance x i64]`, per-slot per-instance byte offsets
- `__lyra_path_pool` -- `[~18*N x i8]`, per-instance hierarchical path strings
- `__lyra_param_pool` -- `[4*N x i8]`, per-instance parameter value payloads
- `__lyra_construction_program` -- `[N x ConstructionProgramEntry]`, one entry per instance

Measured scaling (generate-expand, -c opt):

| N    | lower_llvm | optimize_ir | emit_obj |
| ---- | ---------- | ----------- | -------- |
| 128  | 0.00s      | 0.09s       | 0.14s    |
| 1024 | 0.02s      | 0.42s       | 0.91s    |
| 4096 | 0.08s      | 1.82s       | 4.45s    |

Body descriptors and recipes are O(1). The remaining O(N) data is constructor-time topology data (paths, params, slot offsets, program entries) that is currently baked into LLVM constants at compile time. These should be moved out of the LLVM compilation path entirely.

## F1: Parallel specialization compilation

See [parallel-compilation.md](../parallel-compilation.md) for the full design. All prep cuts are complete. Next: m3 (param transmission table) then F1-impl (per-group isolated compilation with deterministic merge).

Independent of the R-series. Can proceed in parallel.

## F2: Specialization caching

Not yet designed. Depends on F1 completing the parallel compilation model.

## I1: Constructor-time container construction recipes (completed)

Replaced the flat InitPatchEntry/InitHandleEntry model with a typed storage construction recipe IR. Body and package descriptors now carry `StorageConstructionOp` arrays with explicit op kinds (kScalarFourStateInit, kStructInit, kInlineArrayInit, kOwnedContainerConstruct) that preserve container structure. The compile-time recipe builder produces O(1) recipes from the StorageSpecArena tree. The runtime interpreter applies recipes recursively at constructor time, performing container handle realization and element initialization as first-class operations.

Key files: `storage_construction_recipe.hpp` (recipe IR), `storage_construction_recipe_builder.cpp` (compile-time builder), `storage_construction.cpp` (runtime interpreter).

## Documentation: natural model and runtime model alignment

Several docs need alignment with the natural model and R-series runtime model migration:

- pipeline-contract.md still describes types as "language-level" without compile-owned vs constructor-owned ownership distinction
- state-layout.md describes allocation lifecycle without framing as ownership boundaries
- runtime.md and change-propagation.md describe mechanisms without referencing the natural model or noting current-state mismatches

The code already implements the correct projection for types. The docs need to catch up with both the type ownership model and the broader natural model / runtime model direction.

## Compilation unit isolation enforcement

The specialization-scoped IR principle is documented in architecture-principles.md, but enforcement today relies on convention and code review. Violations recur because the wrong thing is merely discouraged, not structurally impossible. The goal is to make boundary violations unrepresentable or prohibitively expensive.

### I-1: Backend must not link frontend/slang

The LLVM backend library currently transitively includes slang headers. This allows backend code to reach frontend AST objects, instance symbols, and scope trees. The backend build target should not depend on the slang library. Frontend types must not appear in any backend header or source file.

Target: remove slang from the backend's dependency closure. Any data the backend needs from the frontend must cross the HIR/MIR serialization boundary as owned, frontend-free types.

### I-2: HIR as owned quarantine boundary

HIR currently carries slang pointers (SymbolId referencing slang symbols, source locations pointing into slang source buffers). After AST-to-HIR lowering, subsequent phases should not require the slang compilation to remain alive. HIR should be fully owned and reconstructable without frontend memory.

Target: HIR types carry no pointers into the slang AST. After HIR is built, the frontend world can be dropped. This makes "sneak a frontend pointer into MIR/backend" structurally impossible.

### I-3: No design-wide query API in backend codegen

Backend codegen (Context, SpecLocalScope, spec_session) currently receives ConstructionInput, which provides a design-wide object table queryable by body_group. This enables body-group-indexed scanning for "first instance" or "representative" lookups that violate compilation unit isolation.

Target: codegen receives only specialization-scoped data (recipes, layout, slot info) and runtime-instance-reachable handles. No ConstructionInput, no Design, no body-group-to-objects query. Per-instance data is loaded at runtime via instance_ptr, not looked up at codegen time via design-wide tables.

### I-4: Opaque typed IDs across layers

A single body_group uint32_t can be used to re-open the whole design-level world from any layer. Different layers should use opaque typed IDs that cannot be directly cross-referenced without explicit conversion at an owning boundary.

Target: separate opaque ID types per layer (HIR, MIR, codegen, runtime). No raw uint32_t body/instance indices cross layer boundaries without typed wrappers that restrict what queries are available.

### I-5: CI forbidden-dependency checks

Static enforcement of include/dependency rules:

- `llvm_backend/` must not include `slang/` headers
- `llvm_backend/context*.cpp` must not include `construction_input.hpp`
- Codegen-facing structs (CompiledModuleSpecInput, SpecLocalScope, ExternalRefResolutionEnv) must not carry body_group, object_index, or construction-level grouping keys
- New fields on codegen-facing structs that carry design-topology identity must fail CI

Target: policy check script (like check_exceptions.py) that enforces these rules on every PR.

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
