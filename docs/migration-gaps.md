# Specialization Migration Gaps

Working queue for migrating Lyra toward specialization-based compilation.

For the stable architecture (phases, specialization boundary rule, parameter classification, realization model): see [compilation-model.md](compilation-model.md).

## Current Status

**E3 partial: `CodegenSession` no longer carries `const mir::Design*`.**

**Current phase**: E (per-specialization codegen). Variant/template-dedup backend ownership deleted. Specialization compiles directly from body-owned process order and body identity. Realization / assembly code no longer reads design MIR directly -- realization input data is extracted explicitly during `CompileDesignProcesses` into `RealizationData`.

**Transitional state**: design-wide compatibility adapters (B5) remain until Phase E3/E4 removes them.

**Specialization compilation**: `CompileModuleSpecSession` is the native per-specialization backend entrypoint. It takes an owned `CompiledModuleSpecInput` (body MIR membership, `SpecLayout`, `SpecCodegenView`) and returns a `CompiledModuleSpec` product (process functions parallel to view.processes, wait sites). Body-function declare/define is specialization-local. Module-scoped function lowering metadata is registered once per specialization. `CompileDesignProcesses` is orchestration: setup, design-global functions (packages + generated), `CompileModuleSpecSession` x N, per-instance wrappers. Wrapper generation routes explicitly by `(body_id, local_nonfinal_process_index)` via an explicit routing table built from Phase 4 products.

**What was deleted in E2**:

- `BuildModuleVariants`, `ProcessTemplate`, `ModuleVariant`, `ProcessFingerprint`, `ProcessMembership`
- `process_fingerprint.hpp` / `process_fingerprint.cpp`
- `ModuleVariantId`, `TemplateId`, `LocalProcIndex`, `LayoutProcessIndex`, `StandaloneRoute`, `TemplatedRoute`, `ProcessRoute`
- `Layout::RouteProcess`, `Layout::GetInstanceVariant`, `Layout::GetInstanceVariantId`
- `Layout::process_templates`, `Layout::variants`, `Layout::process_membership`, `Layout::instance_variant_ids`

**What was added in E2**:

- `Layout::GetBodyRelByteOffsets(body_id)` -- body-owned relative byte offsets computed directly in `BuildLayout`
- `SpecProcessView` (replaces `SpecTemplateView`) -- body-owned process routing with `local_nonfinal_proc_index`
- `SpecCodegenView::processes` (replaces `templates`)
- `CompiledModuleSpec::process_functions` (replaces `template_functions`)
- Explicit `body_to_compiled_funcs` routing table in Phase 5

**What was done in E3 (partial)**:

- `CodegenSession` no longer holds `const mir::Design*` -- replaced with `RealizationData` (param-init entries, slot types, instance paths)
- Realization/assembly code (`emit_design_main.cpp`, `design_metadata_lowering.hpp/.cpp`) has zero `mir::Design` references
- Design-derived realization inputs are extracted explicitly during `CompileDesignProcesses` at the codegen/realization seam

**Remaining gaps**:

- `LoweringInput` still holds `const mir::Design*`
- `Context` still holds broad design/layout state (`const Layout&`, design types)
- `BuildLayout()` still operates design-wide
- Wrapper generation remains design-wide (correct: wrappers are per-instance, not per-specialization)
- Body-function artifacts are Context side effects, not explicit products
- Process lowering uses representative instance ID for %m path support (compatibility-only, not ownership)

**Architectural uncertainties**:

- M2 -- param classification needs to distinguish packed-width params from unpacked-size params, aligned with the specialization boundary rule
- E1-E4 ordering -- may simplify once codegen API is narrowed

## Completed Milestones

- **Phase A complete** -- `ModuleDefId`, `BehaviorFingerprint`, `ModuleSpecId`, `SpecializationMap` introduced. Known gap: M2 (type-level structural refs mitigated by `type.toString()` hashing, not clean).
- **Phase B mostly complete** (B1-B4 done) -- `mir::ModuleBody` owns behavioral IR, `mir::Module` is instance record with `body_id`. Shared bodies active. `mir::Process` carries no instance identity. `SpecializationMap` required (no fallback). Remaining: B6 (HIR split).
- **Phase C complete** (C1-C3) -- specialization-local slot identity via `CollectBodyLocalDecls`. `mir::InstancePlacement` is source of truth for design-state placement. Alias resolution eliminated from behavioral codegen.
- **Phase D complete** (D1-D4 done) -- bindings separated (`CompileBindings` + `realization::AssembleBindings`). Metadata serialization in `realization::BuildDesignMetadata`. `main()` in `realization::EmitDesignMain()`. `LowerMirToLlvm()` is thin wrapper: `CompileDesignProcesses` -> `EmitDesignMain` -> `FinalizeModule`. D2: `InstanceConstBlock` is a first-class realization artifact in `PlacementMap`; per-instance value-only parameter values are owned by `PlacementMap::const_blocks` (body-local slot indices), not by `mir::Design`. `mir::Design::instance_param_inits` deleted.

## Active Gaps

### Now: E3 narrowing (continued)

`CodegenSession` no longer carries `const mir::Design*` (E3 partial done). Remaining E3 scope:

**B3: Design-wide state still flows through top-level APIs**

`LowerMirToLlvm()` still receives `mir::Design`. `BuildLayout()` still processes all instances. `Context` still holds `const Layout&`. These are incremental narrowing tasks, not architectural blockers.

- `include/lyra/llvm_backend/lower.hpp` -- `LoweringInput` holds `const mir::Design*`
- `src/lyra/llvm_backend/layout/layout.cpp` -- `BuildLayout()` iterates all instances
- `include/lyra/llvm_backend/context.hpp` -- `Context` holds `const Layout&`

### Next: M2

**M2: ParamRole classification refinement**

`ShapeParamCollector` has two issues:

1. slang resolves parameterized types during elaboration, so packed-width params may be invisible as `NamedValueExpression` and misclassified as `kValueOnly`.
2. The classifier does not distinguish packed-width params (require specialization) from unpacked-size params (should not specialize).

Fix: walk type expressions to detect packed-width variation. Separately classify unpacked-size params as elaboration-time. See specialization boundary rule in [compilation-model.md](compilation-model.md).

- `src/lyra/lowering/ast_to_hir/param_role.cpp:53-59` -- visitor misses resolved types

### Later: B6 / M4 / E2-E4 / medium gaps

**B6: HIR ownership split**

Same body/instance split at HIR level. `hir::ModuleBody` owned by specialization, `hir::Module` becomes instance record. AST->HIR lowering produces one body per specialization group.

- `src/lyra/lowering/ast_to_hir/design.cpp:502-506` -- `LowerDesign()` creates one `hir::Module` per instance
- `include/lyra/hir/design.hpp` -- `Design::elements` parallel to elaboration order

**E3: Delete remaining broad design/layout coupling from top-level backend APIs and `Context`**

- Remove all code paths that pass full `mir::Design` to codegen
- `Context` no longer holds `const Layout&` (or narrowed to only what it needs)
- `LoweringInput` no longer exposes `mir::Design`
- Depends on E2 (done)

**E4: Delete compatibility adapters / representative-instance compatibility scaffolding**

- Remove transitional codegen adapters from Phase B5
- Remove representative-instance ID usage for %m path support
- Codegen operates natively on `ModuleBody` + instance records
- Depends on E3

**Medium gaps**

- **m1**: DesignState struct is monolithic -- absolute byte offsets depend on elaboration order
- **m2**: Instance paths baked into codegen
- **m4**: ParamRole uses slang pointer as grouping key

## Phase F: Acceleration

Future work after Phase E.

- **F1**: Parallel specialization compilation -- thread pool, scales with core count
- **F2**: Specialization caching -- content-addressed cache keyed by `ModuleSpecId`

## CI / Policy Gates

| Gate                                              | Enforcement             | Status        |
| ------------------------------------------------- | ----------------------- | ------------- |
| G1: No instance identity in specialization MIR    | Structural (type shape) | Enforced      |
| G2: Codegen API has no design input               | API signature check     | Not added yet |
| G3: Value-only params share specialization        | Regression test         | Not added yet |
| G4: Structural params produce distinct specs      | Regression test         | Not added yet |
| G5: Specialization IR is topology-independent     | Regression test         | Not added yet |
| G6: No instance paths in specialization artifacts | Policy check            | Not added yet |
| G7: Specialization grouping is deterministic      | Regression test         | Not added yet |

## Open Questions

1. **BehaviorFingerprint granularity**: Hash structural parameter values, or hash generated HIR?
2. **Package compilation**: Packages have no instances. Separate specialization unit or separate concept?
3. **Container descriptor format**: How should SpecLayout represent container regions?
4. **MIR interpreter alignment**: Debug-only. Migrate in lockstep or defer?
