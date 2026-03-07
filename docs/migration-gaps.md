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

`ComputeProcessFingerprint()` in `include/lyra/llvm_backend/process_fingerprint.hpp` normalizes slot references by subtracting `base_slot_id`. `ProcessTemplate` groups processes by fingerprint. Shared functions use `this_ptr + rel_byte_offsets[slot_id - base_slot_id]`. This is the seed of specialization-scoped codegen -- the addressing model is correct, but discovered through design-wide dedup instead of being the native specialization model.

### DesignBindingPlan partially separates port connections

`DesignBindingPlan` (from AST->HIR) captures port connections as data. `ApplyBindings()` converts them to MIR processes. Conceptually an assembly concern, already partially separated from module compilation.

## Migration Strategy

Staged transition. Each phase establishes new invariants while old paths coexist temporarily.

**Stage 1: Identity** (Phase A) - Complete. `ModuleSpecId` and `SpecializationMap` introduced. Grouping is observation-only. Old per-instance pipeline unchanged. Known gap: M2 (param classification misses type-level structural refs).

**Stage 2: Specialization-scoped IR** (Phase B) - HIR/MIR lowered once per specialization. Old per-instance containers replaced. Assembly still fused. Codegen still design-wide. Temporary: codegen duplicates specialization MIR back into per-instance form for compatibility.

**Stage 3: Storage model** (Phase C) - SlotId becomes specialization-local. Assembly-time placement mapping introduced. Old design-global slot allocation removed.

**Stage 4: Assembly extraction** (Phase D) - Assembly becomes a separate phase. Binding, metadata, and connectivity moved out of MIR lowering and codegen. Temporary: assembly still produces the same monolithic LLVM IR.

**Stage 5: Per-specialization codegen** (Phase E) - Codegen API takes one specialization. Design-wide main/metadata generation moves to assembly. Template dedup path removed (dedup is automatic). Old design-wide codegen path deleted.

**Stage 6: Acceleration** (Phase F) - Parallel compilation, caching, incremental. Pure wins from clean architecture.

**Allowed during transition**: Dual containers (old per-instance + new per-spec) in intermediate stages. Temporary compatibility adapters that expand specialization artifacts back to per-instance form.

**Forbidden immediately**: New code that adds design-global dependencies to specialization artifacts. New code that passes `mir::Design` to specialization-local functions.

## Gap Inventory

### Blockers

**B1: HIR/MIR are per-instance, not per-specialization**

Each elaborated instance gets its own `hir::Module` and `mir::Module`. Two instances of the same module with identical structural params produce duplicate IR.

- `src/lyra/lowering/ast_to_hir/design.cpp` - `LowerDesign()` creates one `hir::Module` per instance
- `src/lyra/lowering/hir_to_mir/design_lower.cpp:33` - `LowerDesign()` creates one `mir::Module` per instance
- `include/lyra/hir/design.hpp` - `Design::elements` parallel to elaboration order
- `include/lyra/mir/design.hpp:56` - `Design::elements` parallel to elaboration order

**B2: SlotId allocation is design-global**

Single monotonic counter across entire design. All downstream tables use design-global indices. Specialization artifacts cannot be compiled independently.

- `src/lyra/lowering/hir_to_mir/design_decls.cpp:27` - `int next_slot = 0;` global counter
- `include/lyra/mir/handle.hpp:79` - `SlotId` is bare `uint32_t`, no scope
- `include/lyra/mir/design.hpp:63` - `Design::slots` indexed by global SlotId
- `include/lyra/mir/place.hpp:13` - `PlaceRoot::kDesign` uses `id` as global SlotId

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

**m3: Process owner_instance_id in MIR** - `include/lyra/mir/routine.hpp:28`. MIR should not know about instances.

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

### Phase B: Specialization-scoped IR containers

**B1: Introduce specialization-indexed HIR container**

- Goal: New container that holds one `hir::Module` per `ModuleSpecId`, alongside existing per-instance container
- Areas: `include/lyra/hir/design.hpp`, `src/lyra/lowering/ast_to_hir/design.cpp`
- Acceptance: `hir::Design` has a spec-indexed container. Old per-instance container still exists (dual-path). Test: design with N instances and K specs has exactly K entries in spec container.
- Dependencies: A3
- Invariant: Spec container has no duplicate modules for same `ModuleSpecId`.

**B2: Lower HIR once per specialization**

- Goal: AST->HIR lowering runs once per `ModuleSpecId`, not per instance
- Areas: `src/lyra/lowering/ast_to_hir/design.cpp`, `src/lyra/lowering/ast_to_hir/module.cpp`
- Acceptance: Lowering consumes specialization input (not "pick an arbitrary instance"). Representative instance selection must be deterministic. Other instances reference the same HIR module. Old per-instance path removed. Test: N identical instances produce 1 HIR lowering call.
- Dependencies: B1
- Invariant: HIR is specialization-scoped. No instance-coupled assumptions in the lowering path.

**B3: Introduce specialization-indexed MIR container**

- Goal: MIR container holds one `mir::Module` per `ModuleSpecId`
- Areas: `include/lyra/mir/design.hpp`, `src/lyra/lowering/hir_to_mir/design_lower.cpp`
- Acceptance: `mir::Design` has spec-indexed modules. Old per-instance MIR still exists as compatibility adapter. Test: K specializations produce K MIR modules.
- Dependencies: B2
- Invariant: MIR modules are specialization-scoped.

**B4: Lower MIR once per specialization**

- Goal: HIR->MIR lowering runs once per `ModuleSpecId`
- Areas: `src/lyra/lowering/hir_to_mir/design_lower.cpp`, `src/lyra/lowering/hir_to_mir/module.cpp`
- Acceptance: Lowering consumes specialization HIR, not arbitrary instance. Representative selection deterministic. Old per-instance MIR path removed. Test: identical instances produce 1 MIR lowering call.
- Dependencies: B3
- Invariant: MIR lowering is specialization-scoped. No instance-coupled assumptions.

**B5: Remove owner_instance_id from MIR Process**

- Goal: MIR processes don't know which instance they belong to
- Areas: `include/lyra/mir/routine.hpp`, codegen compatibility adapter
- Acceptance: `owner_instance_id` removed from `mir::Process`. Instance binding deferred to assembly. Test: MIR Process struct has no instance-specific fields.
- Dependencies: B4
- Invariant: No instance identity in specialization MIR.

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

- Goal: Remove temporary dual-container support and per-instance expansion layers
- Areas: `include/lyra/hir/design.hpp`, `include/lyra/mir/design.hpp`, any compatibility shims added during Phase B
- Acceptance: No dual-path containers remain. No compatibility lowering path remains. No temporary expansion from specialization to per-instance form. Test: all tests pass with only specialization-scoped containers.
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

**G1: Policy check - no design-global IDs in specialization MIR**

- After Phase B: Assert `mir::Process` has no `owner_instance_id`. Assert specialization MIR `SlotId` values are 0-based.
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
