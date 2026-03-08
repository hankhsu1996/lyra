# Specialization Migration Gaps

Canonical working queue for migrating Lyra toward specialization-based compilation.

## North Star

Compile unit is `ModuleSpecId = (ModuleDefId, BehaviorFingerprint)`. Four distinct phases:

1. **Elaboration Discovery** - discover module definitions and instances from slang
2. **Specialization Compilation** (parallel per spec) - AST -> HIR -> MIR -> LLVM IR, specialization-scoped
3. **Assembly/Link** - bind instances to compiled specializations, produce connectivity tables, emit metadata
4. **Runtime Execution**

Key architectural rule: the `this_base + local_offset` addressing pattern (currently in `ModuleVariant::rel_byte_offsets`) is the right model. The migration preserves specialization-local relative layout while deleting design-wide compilation coupling.

## Assembly Artifacts

Concrete types that replace the current fused pipeline:

| Artifact              | Scope          | Purpose                                                        |
| --------------------- | -------------- | -------------------------------------------------------------- |
| `ModuleDefId`         | Elaboration    | Stable definition identity (replaces `DefinitionSymbol*`)      |
| `BehaviorFingerprint` | Elaboration    | Hash of structural parameter values                            |
| `ModuleSpecId`        | Elaboration    | `(ModuleDefId, BehaviorFingerprint)` - specialization identity |
| `CompiledModuleSpec`  | Specialization | LLVM IR + specialization-local layout for one spec             |
| `SpecLayout`          | Specialization | Slot count, local offsets, type info for one spec              |
| `InstancePlacement`   | Assembly       | Maps instance -> `(ModuleSpecId, base_offset_in_DesignState)`  |
| `InstanceConstBlock`  | Assembly       | Per-instance value-only parameter values                       |
| `ConnectivityTable`   | Assembly       | Port connection descriptors across instances                   |
| `RuntimeMetadataBlob` | Assembly       | Slot meta, process meta, instance paths, loop sites            |

## Required Invariants

These must hold after migration. Each gets a CI/policy enforcement task.

1. **No design-global IDs in specialization artifacts** - HIR, MIR, and per-specialization LLVM IR must not contain SlotIds, instance IDs, or instance paths that depend on the elaborated design.

2. **No instance-path dependence in compiled code** - Process functions must not embed instance-specific information. Instance paths are assembly/runtime metadata only.

3. **No codegen requiring whole-design layout** - Specialization codegen must not access other modules' layouts, global connectivity, or design-wide slot tables.

4. **Specialization artifacts are cacheable** - Given the same `ModuleSpecId`, the compiled artifact must be identical regardless of which design uses it or how many instances exist.

5. **Value-only params do not create distinct specializations** - Two instances with identical structural params but different value-only params share compiled code. Value-only params initialized at runtime via `InstanceConstBlock`.

6. **Assembly is the only phase with design-global knowledge** - Only assembly sees full design topology. It maps specialization-local slots to design-global offsets and constructs connectivity tables.

## Progress Already Achieved

### Parameter classification exists

`ParamRole` enum (`kShape` / `kValueOnly`) in `include/lyra/lowering/ast_to_hir/param_role.hpp`. `ClassifyParamRoles()` in `src/lyra/lowering/ast_to_hir/param_role.cpp` separates structural from value-only. Value-only params get `StorageClass::kDesignStorage` (runtime slots) instead of `kConstOnly` (compile-time folding).

### Process fingerprinting and relative addressing exist

`ComputeProcessFingerprint()` in `include/lyra/llvm_backend/process_fingerprint.hpp` hashes `kModuleSlot` roots as body-local IDs and `kDesignGlobal` roots as global IDs directly -- no `base_slot_id` normalization needed since MIR storage roots carry explicit scope. `ProcessTemplate` groups processes by fingerprint. Shared functions use `this_ptr + rel_byte_offsets[slot_id - base_slot_id]`. This is the seed of specialization-scoped codegen -- the addressing model is correct, but discovered through design-wide dedup instead of being the native specialization model.

### DesignBindingPlan partially separates port connections

`DesignBindingPlan` (from AST->HIR) captures port connections as data. `ApplyBindings()` converts them to MIR processes. Conceptually an assembly concern, already partially separated from module compilation.

## Migration Strategy

Staged transition. Each phase establishes new invariants while old paths coexist temporarily.

**Stage 1: Identity** (Phase A) - Complete. `ModuleSpecId` and `SpecializationMap` introduced. Grouping is observation-only. Old per-instance pipeline unchanged. Known gap: M2 (param classification misses type-level structural refs -- temporarily mitigated by `type.toString()` hashing in structural fingerprint, not a clean solution).

**Stage 2: Specialization-ready ownership** (Phase B) - B1-B3 done. Ownership boundary established: `ModuleBody` owns behavioral IR, `Module` is instance-side record with `body_id`. Shared bodies active (one body per specialization group). Layout/codegen use `ScheduledProcess` records (no parallel arrays). Interpreter/test framework use typed `ProcessHandle` keys. `SpecializationMap` is non-nullable in MIR-lowering input (enforced at type level). `BuildMirSpecGroups` validates specialization invariants. `InstanceTable::GetPathBySymbol` centralizes instance path lookup. MIR dump uses explicit `ModuleBodies` / `Modules` sections. Remaining: B4 (remove `owner_instance_id`), B5 (codegen compatibility), B6 (HIR ownership split).

**Stage 3: Storage model** (Phase C) - SlotId becomes specialization-local. Assembly-time placement mapping introduced. Old design-global slot allocation removed.

**Stage 4: Assembly extraction** (Phase D) - Assembly becomes a separate phase. Binding, metadata, and connectivity moved out of MIR lowering and codegen. Temporary: assembly still produces the same monolithic LLVM IR.

**Stage 5: Per-specialization codegen** (Phase E) - Codegen API takes one specialization. Design-wide main/metadata generation moves to assembly. Template dedup path removed (dedup is automatic). Old design-wide codegen path deleted.

**Stage 6: Acceleration** (Phase F) - Parallel compilation, caching, incremental. Pure wins from clean architecture.

**Allowed during transition**: Compatibility adapters that reconstruct per-instance views from specialization artifacts for downstream consumers. Design-global slot IDs in specialization process bodies (normalization is a later concern).

**Forbidden immediately**: New code that adds design-global dependencies to specialization artifacts. New code that passes `mir::Design` to specialization-local functions. New code that makes instances own behavioral IR.

## Gap Inventory

### Blockers

**B1: MIR behavioral IR ownership is per-instance instead of per-specialization** (partially addressed)

MIR ownership split done: `mir::ModuleBody` owns processes/functions, `mir::Module` is an instance-side record with `body_id`. Bodies are still 1:1 with instances (no sharing yet). HIR still per-instance.

Remaining:

- `src/lyra/lowering/ast_to_hir/design.cpp:502-506` - `LowerDesign()` creates one `hir::Module` per instance
- `include/lyra/hir/design.hpp` - `Design::elements` parallel to elaboration order, each element owns behavioral content
- `include/lyra/mir/routine.hpp:28` - `mir::Process::owner_instance_id` still embeds instance identity in behavioral IR

**B2: SlotId allocation is design-global** (partially addressed)

Single monotonic counter across entire design. All downstream tables use design-global indices. Specialization artifacts cannot be compiled independently.

MIR place roots now distinguish module-local vs design-global storage: `PlaceRoot::kModuleSlot` (body-local, 0-based) and `PlaceRoot::kDesignGlobal` (package/global). Module body processes use `kModuleSlot` for module-owned state. `SignalRef` carries explicit scope (`kModuleLocal` / `kDesignGlobal`) for wait triggers. `ScopedSlotRef` and `ScopedPlanOp` preserve scope for late-bound index dependencies. `ModuleBody` owns body-local slot descriptors. Each subsystem has one canonical scope-resolution helper: `Context::ResolveDesignGlobalSlotId()` (LLVM backend, with overloads for `PlaceRoot`, `SignalRef`, `ScopedSlotRef`), `ResolveDesignGlobalSlot()` (interpreter), `ResolveSignalToGlobalSlot()` (layout). Process fingerprinting and sensitivity analysis operate on scoped identity directly without collapsing to global. Bodies are storage-local by construction but still 1:1 with instances.

Remaining:

- `src/lyra/lowering/hir_to_mir/design_decls.cpp:27` - `int next_slot = 0;` global counter still drives allocation
- `include/lyra/mir/handle.hpp:79` - `SlotId` is bare `uint32_t`, no scope
- `include/lyra/mir/design.hpp:63` - `Design::slots` indexed by global SlotId
- Design-global slot tables and `instance_slot_ranges` still provide the runtime placement model

**B3: Codegen operates on entire design**

`LowerMirToLlvm()` receives `mir::Design`. `BuildLayout()` processes all instances, processes, connections. Cannot parallelize or cache.

- `include/lyra/llvm_backend/lower.hpp:83-95` - `LoweringInput` holds `const mir::Design*`
- `src/lyra/llvm_backend/lower.cpp:666` - `LowerMirToLlvm()` entry point
- `src/lyra/llvm_backend/layout/layout.cpp:1620` - `BuildLayout()` iterates all instances
- `include/lyra/llvm_backend/context.hpp:471` - `Context` holds `const mir::Design&`

### Major

**M1: Assembly fused into MIR lowering**

`ApplyBindings()` called during `LowerDesign()`. Connection processes mixed with module processes.

- `src/lyra/lowering/hir_to_mir/design_lower.cpp:99` - `ApplyBindings()` mid-lowering
- `src/lyra/lowering/hir_to_mir/design_connections.cpp` - synthetic connection processes
- `include/lyra/mir/design.hpp:83-85` - `alias_map` and `connection_processes` in Design

**M2: ParamRole classification needs refinement for construction vs behavior**

`ShapeParamCollector` walks the elaborated AST for `NamedValueExpression` references outside procedural contexts. Two issues:

1. slang resolves parameterized types during elaboration, so a parameter used in a type declaration (e.g., `bit [WIDTH-1:0] data`) may be baked into the resolved type and not visible as a `NamedValueExpression`. This can cause params that affect packed widths to be misclassified as `kValueOnly`.

2. The classifier does not distinguish between parameters that affect packed widths (execution-time, must specialize) and parameters that affect only unpacked container sizes (elaboration-time, should not specialize).

- `src/lyra/lowering/ast_to_hir/param_role.cpp:53-59` - visitor only sees `NamedValueExpression`, misses resolved types
- Impact: Packed-width params may be misclassified; unpacked-size params may over-specialize
- Fix: Walk type expressions to detect packed-width variation. Separately classify unpacked-size params as elaboration-time (not structural). See architectural direction in this document.

**M3: Runtime metadata built during codegen**

Slot meta, process meta, connection descriptors, comb kernels, instance paths all emitted as LLVM globals during `LowerMirToLlvm()`. These are design-wide.

- `src/lyra/llvm_backend/lower.cpp:154-250` - `EmitSlotMetaTable()`
- `src/lyra/llvm_backend/lower.cpp:297-399` - `EmitProcessMetaTable()`
- `src/lyra/llvm_backend/lower.cpp:1102-1205` - connection descriptors
- `src/lyra/llvm_backend/lower.cpp:1265-1292` - `__lyra_instance_paths`
- `src/lyra/llvm_backend/lower.cpp:635-660` - `EmitParamInitStores()`

**M4: Template dedup is cross-instance optimization in codegen**

`BuildModuleVariants()` groups processes across all instances by fingerprint during layout. Requires design-wide knowledge. In north star model, dedup is automatic.

- `src/lyra/llvm_backend/layout/layout.cpp:1266-1500` - `BuildModuleVariants()`
- `include/lyra/llvm_backend/layout/layout.hpp:183-192` - `ProcessTemplate`
- `src/lyra/llvm_backend/lower.cpp:790-839` - template emission + wrapper routing

### Medium

**m1: DesignState struct is monolithic** - Absolute byte offsets depend on elaboration order. `include/lyra/llvm_backend/layout/layout.hpp:72-83`, `src/lyra/llvm_backend/layout/layout.cpp:154-250`.

**m2: Instance paths baked into codegen** - `include/lyra/mir/instance.hpp:10-32`, `src/lyra/llvm_backend/lower.cpp:1265-1292`.

**m3: Process owner_instance_id in MIR** - `include/lyra/mir/routine.hpp:28`. Instance identity in behavioral IR prevents specialization ownership. Addressed by B4.

**m4: ParamRole uses slang pointer as grouping key** - `src/lyra/lowering/ast_to_hir/param_role.cpp:72-77`.

**m5: main() generation is monolithic** - `src/lyra/llvm_backend/lower.cpp:841-1200+`. Design-wide assembly logic in codegen.

### Minor

**n1: No caching infrastructure** - Future work after specialization compilation separated.

**n2: MIR interpreter uses design-global state** - Debug-only. Align when main path migrated.

**n3: No parallel compilation** - Future work after specializations are independent.

## Prioritized Working Queue

Each task has: goal, areas, acceptance criteria, dependencies. Tasks are independently mergeable.

### Phase A: Identity and grouping

**A1: Define ModuleDefId**

- Goal: Stable definition identity replacing `DefinitionSymbol*`
- Areas: New `include/lyra/common/module_identity.hpp`, `src/lyra/lowering/ast_to_hir/design.cpp`
- Acceptance: `ModuleDefId` type exists. `module_def_key` uses it instead of pointer cast. Test: same definition produces same `ModuleDefId` within a compilation session.
- Dependencies: None
- Invariant: Definition identity is stable within a compilation. Cross-build cache identity comes from a content-based layer above `ModuleDefId` (future work).

**A2: Define BehaviorFingerprint**

- Goal: Hash all elaboration inputs that affect structure/code shape
- Areas: `include/lyra/common/module_identity.hpp`, `src/lyra/lowering/ast_to_hir/param_role.cpp`
- Acceptance: `BehaviorFingerprint` type exists. First implementation hashes kShape parameter values after `ClassifyParamRoles()`. Test: two instances with same structural params produce same fingerprint; different structural params produce different fingerprint.
- Dependencies: A1
- Invariant: Fingerprint captures all elaboration inputs that affect structure/code shape (structural params initially; future: defines, interface bindings, anything that changes elaborated structure).

**A3: Define ModuleSpecId and grouping pass**

- Goal: Group instances by `(ModuleDefId, BehaviorFingerprint)` after elaboration
- Areas: `include/lyra/common/module_identity.hpp`, `src/lyra/lowering/ast_to_hir/design.cpp`
- Acceptance: `ModuleSpecId` type exists. After elaboration, each instance is assigned a `ModuleSpecId`. Test: design with 4 instances of same module (2 param configs) produces exactly 2 specialization groups.
- Dependencies: A2
- Invariant: Instances with identical structural params share a `ModuleSpecId`.

**A4: Test value-only param grouping**

- Goal: Prove value-only params don't split specializations
- Areas: Tests only
- Acceptance: Test: two instances with same structural params but different value-only params share the same `ModuleSpecId`.
- Dependencies: A3
- Invariant: Value-only params do not affect specialization identity.

### Phase B: Specialization-owned behavioral IR

The goal of Phase B is to establish the ownership boundary required for specialization-scoped IR. Module behavioral IR (processes, functions) moves to a dedicated body artifact; instances become lightweight records that reference bodies. Early steps establish the shape with 1:1 mapping; later steps introduce actual sharing by `ModuleSpecId` and remove instance identity from behavioral IR.

**B1: Introduce specialization-ready MIR body artifact** (done)

- Goal: First-class `mir::ModuleBody` type that owns behavioral IR (processes, functions). Purely behavioral --no instance identity, no storage shape, no placement metadata. This is the seed of specialization-owned MIR; actual sharing comes in B3.
- Areas: New `include/lyra/mir/module_body.hpp`, `include/lyra/mir/module.hpp`, `include/lyra/mir/design.hpp`, all MIR consumers
- Acceptance: `mir::ModuleBody` type exists with `processes` and `functions` only. `mir::Module` becomes instance-side record with `instance_sym` and `body_id`. `mir::Design` gains `module_bodies` vector. `LowerModule` returns `ModuleBody`. All consumers access behavioral IR through `GetModuleBody()`. Bodies are 1:1 with instances (no sharing yet).
- Dependencies: None (ownership-only change, no grouping logic needed)
- Invariant: `ModuleBody` contains no instance-specific fields, no storage descriptors, no placement data. `mir::Module` does not own behavioral IR.

**B2: (Absorbed into B1)**

B1 now covers both the body artifact and the instance record conversion as a single atomic change. `mir::Module` is `{instance_sym, body_id}` --no additional instance-side fields until truly needed by later phases.

**B3: Lower MIR body once per specialization group** (done)

- Goal: HIR->MIR module lowering produces one `ModuleBody` per `ModuleSpecId`. Multiple instance records reference the same body. `SpecializationMap` drives the lowering loop.
- Areas: `src/lyra/lowering/hir_to_mir/design_lower.cpp`, `src/lyra/lowering/hir_to_mir/module.cpp`, `include/lyra/lowering/hir_to_mir/lower.hpp`
- Acceptance: `LoweringInput` gains `const SpecializationMap*`. `LowerDesign` iterates specialization groups, lowering module body once per group. Instance records created for each instance referencing the shared body. Test: design with N instances and K specs produces exactly K `LowerModule` calls and K `ModuleBody` entries.
- Dependencies: B1, A3
- Invariant: Module behavioral lowering runs once per specialization, not per instance.
- Done: Shared bodies active. `SpecializationMap` is non-nullable in `LoweringInput` (no default, enforced at type level). `BuildMirSpecGroups` validates group invariants (range, uniqueness, cross-reference with `spec_id_by_instance`). Layout uses `ScheduledProcess` records (single vector, no parallel arrays). Interpreter/test framework use typed `ProcessHandle` keys. MIR verification uses `InstanceTable::GetPathBySymbol` (centralized lookup, no ad-hoc maps). MIR dump has explicit `ModuleBodies` / `Modules` sections.
- Temporary stopgap: Structural fingerprint includes `type.toString()` hashing for variable types to catch param-dependent types resolved at elaboration. This is a presentation-format mitigation, not a clean semantic fingerprint. Proper fix requires explicit structural type fingerprinting over resolved type structure (see M2 gap).
- Remaining debt: `owner_instance_id` remains on `mir::Process` (B4).

**B4: Remove owner_instance_id from MIR Process**

- Goal: `owner_instance_id` on `mir::Process` is instance metadata embedded in behavioral IR. Remove it so processes in a shared `ModuleBody` carry no instance identity. Instance-to-process binding is expressed through the instance record's `body_id` + design walk context.
- Areas: `include/lyra/mir/routine.hpp`, `src/lyra/lowering/hir_to_mir/module.cpp`, codegen sites that read `owner_instance_id`
- Acceptance: `mir::Process` has no `owner_instance_id`. Codegen retrieves instance identity from the design walk context (instance record), not from the process. Test: `mir::Process` struct has no instance-specific fields.
- Dependencies: B3 (sharing must exist before instance identity can be removed from processes)
- Invariant: No instance identity in specialization-owned MIR.
- Note: `VerifyLoweredMir` currently derives module labeling from `process.owner_instance_id` --this must be refactored to use instance record context.

**B5: Codegen compatibility adapter**

- Goal: Existing design-wide codegen continues to work by reconstructing per-instance process views from specialization bodies + instance records. Temporary bridge until Phase E.
- Areas: `src/lyra/llvm_backend/lower.cpp`, `src/lyra/llvm_backend/layout/layout.cpp`
- Acceptance: All existing tests pass. Codegen iterates instance records, looks up shared `ModuleBody`, and provides instance_id from the record. `BuildModuleVariants` continues to work on the reconstructed view. Test: no behavioral change in output.
- Dependencies: B3, B4
- Invariant: No new design-global dependencies added. Adapter is clearly marked as transitional.

**B6: Propagate ownership to HIR**

- Goal: Same ownership split at HIR level. `hir::ModuleBody` owned by specialization, `hir::Module` becomes instance record.
- Areas: `include/lyra/hir/module.hpp`, `include/lyra/hir/design.hpp`, `src/lyra/lowering/ast_to_hir/design.cpp`
- Acceptance: AST->HIR lowering produces one `hir::ModuleBody` per specialization group. Instance records reference shared bodies. Test: K specs produce K HIR body lowering calls.
- Dependencies: B1
- Invariant: HIR behavioral ownership follows the same body/instance split as MIR.

### Phase C: Specialization-local storage model

**C1: Make SlotId specialization-local**

- Goal: SlotIds start at 0 per specialization
- Areas: `src/lyra/lowering/hir_to_mir/design_decls.cpp`, `include/lyra/mir/handle.hpp`
- Acceptance: Each specialization allocates slots starting from 0. Test: two specializations both have slot 0; design-global mapping exists in assembly data.
- Dependencies: B4
- Invariant: Specialization slot numbering is independent of other specializations.

**C2: Introduce assembly-time placement mapping**

- Goal: Assembly maps `(ModuleSpecId, instance) -> base_offset` in DesignState. Assembly owns final DesignState layout; specialization compilation owns only `SpecLayout`.
- Areas: New `include/lyra/assembly/placement.hpp`, `src/lyra/assembly/placement.cpp`, modify `src/lyra/llvm_backend/layout/layout.cpp`
- Acceptance: `InstancePlacement` type exists. Assembly computes base offsets. Test: specialization code uses `this_base + local_offset`, not absolute offsets.
- Dependencies: C1
- Invariant: Specialization code does not know absolute DesignState offsets. Assembly owns DesignState placement.

**C3: Convert codegen to specialization-local slot access**

- Goal: All slot access in codegen uses local offsets, not design-global
- Areas: `src/lyra/llvm_backend/context_place.cpp`, `include/lyra/llvm_backend/context.hpp`
- Acceptance: `GetDesignSlotPointer()` uses local offset + base. No global slot lookup in process codegen. Test: changing instance count does not change specialization LLVM IR.
- Dependencies: C2
- Invariant: Specialization LLVM IR is independent of design topology.

### Phase D: Assembly extraction

**D1: Extract ApplyBindings from MIR lowering**

- Goal: Port connection binding is a separate assembly step
- Areas: `src/lyra/lowering/hir_to_mir/design_lower.cpp`, new `include/lyra/assembly/binding.hpp`, `src/lyra/assembly/binding.cpp`
- Acceptance: `LowerDesign()` no longer calls `ApplyBindings()`. Assembly step consumes `DesignBindingPlan` + compiled specializations. Test: MIR lowering produces no connection processes.
- Dependencies: B4
- Invariant: MIR lowering is purely specialization-scoped.

**D2: Define InstanceConstBlock**

- Goal: Per-instance value-only parameter values as assembly artifact
- Areas: New `include/lyra/assembly/instance_const.hpp`, `src/lyra/assembly/instance_const.cpp`
- Acceptance: `InstanceConstBlock` type exists. Assembly creates one per instance from value-only param values. Replaces `instance_param_inits` in `mir::Design`. Test: two instances with different value-only params produce different `InstanceConstBlock` but share specialization.
- Dependencies: A4, C2
- Invariant: Value-only params are assembly data, not compilation data.

**D3: Move metadata table construction to assembly**

- Goal: Slot meta, process meta, instance paths, connection descriptors produced by assembly
- Areas: Extract from `src/lyra/llvm_backend/lower.cpp` into `src/lyra/assembly/metadata.cpp`
- Acceptance: `EmitSlotMetaTable`, `EmitProcessMetaTable`, `EmitLoopSiteMetaTable`, connection descriptor emission, `__lyra_instance_paths` moved to assembly. Test: per-specialization codegen does not emit design-wide globals.
- Dependencies: D1, C3
- Invariant: Specialization codegen produces no design-wide metadata.

**D4: Move main() generation to assembly**

- Goal: Assembly generates the main function
- Areas: Extract from `src/lyra/llvm_backend/lower.cpp:841-1200+`
- Acceptance: `LowerMirToLlvm()` no longer generates main(). Assembly phase constructs main from `InstancePlacement` + compiled specialization functions. Test: main() exists only in assembly output.
- Dependencies: D3
- Invariant: Assembly owns design-wide code generation.

### Phase E: Per-specialization codegen

**E1: New codegen API takes one specialization**

- Goal: `LowerMirToLlvm()` operates on a single `CompiledModuleSpec`
- Areas: `include/lyra/llvm_backend/lower.hpp`, `src/lyra/llvm_backend/lower.cpp`
- Acceptance: `LoweringInput` no longer holds `const mir::Design*`. Takes specialization MIR + `SpecLayout`. Test: codegen call has no design-global parameters.
- Dependencies: D3, D4
- Invariant: Specialization codegen is self-contained.

**E2: Remove template dedup path**

- Goal: Delete `BuildModuleVariants`, `ProcessTemplate`, fingerprint-based dedup
- Areas: `src/lyra/llvm_backend/layout/layout.cpp:1266-1500`, `include/lyra/llvm_backend/layout/layout.hpp:183-202`
- Acceptance: No cross-instance fingerprinting. Dedup is automatic from specialization model. Test: all tests still pass without template dedup code.
- Dependencies: E1
- Invariant: No cross-instance optimization in codegen.

**E3: Delete legacy design-wide codegen path**

- Goal: Remove all code paths that pass full `mir::Design` to codegen
- Areas: `include/lyra/llvm_backend/lower.hpp`, `src/lyra/llvm_backend/lower.cpp`, `include/lyra/llvm_backend/context.hpp`
- Acceptance: `Context` no longer holds `const mir::Design&`. Test: compilation succeeds with only per-specialization codegen path.
- Dependencies: E2
- Invariant: Design-wide compilation coupling deleted.

**E4: Delete compatibility adapters**

- Goal: Remove transitional codegen adapters from Phase B5
- Areas: `include/lyra/mir/design.hpp`, `src/lyra/llvm_backend/lower.cpp`, any compatibility shims added during Phase B
- Acceptance: No per-instance reconstruction. No compatibility adapters remain. Codegen operates natively on `ModuleBody` + instance records. Test: all tests pass with only specialization-owned path.
- Dependencies: E3
- Invariant: Single clean path. No legacy per-instance artifacts.

### Phase F: Acceleration

**F1: Parallel specialization compilation**

- Goal: Thread pool compiles specializations concurrently
- Areas: `src/lyra/driver/pipeline.cpp`
- Acceptance: Compilation scales with core count. Test: N specializations compile in parallel on N cores.
- Dependencies: E1
- Invariant: Specializations are independently compilable.

**F2: Specialization caching**

- Goal: Content-addressed cache keyed by `ModuleSpecId`
- Areas: New caching infrastructure
- Acceptance: Unchanged specializations skip recompilation. Test: second build of same design hits cache for all specs.
- Dependencies: E1
- Invariant: Cache produces identical artifacts for identical `ModuleSpecId`.

## CI / Policy Gates

Enforcement tasks to add during migration. Each prevents regression.

**G1: Policy check - no instance identity in specialization-owned MIR**

- After Phase B: Assert `mir::Process` has no `owner_instance_id`. Assert `mir::ModuleBody` has no instance-specific fields. Assert instance records do not own processes or functions.
- Tool: `tools/policy/check_specialization_scope.py` or compile-time static assertion.

**G2: Policy check - specialization codegen API has no design input**

- After Phase E: Assert `LoweringInput` for specialization codegen does not accept `mir::Design*`.
- Tool: API signature check in CI.

**G3: Regression test - value-only params share specialization**

- After Phase A: Test that two instances with same structural params but different value-only params produce exactly 1 specialization.

**G4: Regression test - structural params produce distinct specializations**

- After Phase A: Test that two instances with different structural params produce 2 specializations.

**G5: Regression test - specialization IR is topology-independent**

- After Phase C: Test that adding/removing instances of the same spec does not change per-specialization LLVM IR.

**G6: Policy check - no instance paths in specialization artifacts**

- After Phase D: Assert compiled specialization contains no instance path strings.

**G7: Regression test - specialization grouping is deterministic**

- After Phase A: Same source with same elaboration inputs produces identical specialization grouping and ordering. Matters for caching and reproducibility.

## Architectural Direction

**Future direction, not yet implemented.**

The long-term state layout moves from a compile-time LLVM struct to an arena-based layout with elaboration-resolved container offsets. See [state-layout.md](state-layout.md).

Key consequences: unpacked container sizes are no longer structural specialization inputs. `SpecLayout` distinguishes static offsets (packed fields) from container descriptors (elaboration-resolved). Assembly resolves container sizes and allocates the arena. Generate blocks that only construct the design graph (including process instantiation) are elaboration-time, not specialization boundaries.

## Open Questions

1. **BehaviorFingerprint granularity**: Hash structural parameter values, or hash generated HIR? Hashing params is simpler but might miss cases where different param values produce identical IR.

2. **Connection process ownership**: Connection processes span two specializations (parent port + child port). Assembly must create them using both specializations' layouts.

3. **Package compilation**: Packages have no instances. Should they be their own specialization unit or a separate concept?

4. **Container descriptor format**: How should SpecLayout represent container regions? Options include inline descriptor structs, a separate container metadata table, or a hybrid.

5. **MIR interpreter alignment**: Debug-only. Migrate in lockstep or defer?

6. **Specialization body slot numbering**: Addressed. `ModuleBody` processes now use `kModuleSlot` (0-based, body-local) for module-owned storage, with `SignalRef::kModuleLocal` for wait triggers. Design-global references use `kDesignGlobal`. `ModuleBody` owns body-local slot descriptors. Rebasing from body-local to design-global happens through one canonical resolver per subsystem. Remaining transitional items: `body_local_decls` indexed per-instance (should collapse to per-specialization); `SignalRef`/`ScopedSlotRef`/`PlaceRoot` express the same scope concept as separate types (could unify scope enum). Full spec-local slot allocation (removing design-global counter) remains Phase C.
