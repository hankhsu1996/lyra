# Specialization Migration Gaps

Working queue for migrating Lyra toward specialization-based compilation.

For the stable architecture (phases, specialization boundary rule, parameter classification, realization model): see [compilation-model.md](compilation-model.md).

## Current Status

**Next PR: E1 -- narrow codegen API to one specialization.**

**Current phase**: transitioning from D (realization extraction) to E (per-specialization codegen).

**Immediate blocker**: B3 -- codegen operates on entire design. E1 is the fix path.

**Transitional state**: design-wide compatibility adapters (B5) remain until Phase E removes them.

**Architectural uncertainties**:

- M2 -- param classification needs to distinguish packed-width params from unpacked-size params, aligned with the specialization boundary rule
- E1-E4 ordering -- may simplify once codegen API is narrowed

## Completed Milestones

- **Phase A complete** -- `ModuleDefId`, `BehaviorFingerprint`, `ModuleSpecId`, `SpecializationMap` introduced. Known gap: M2 (type-level structural refs mitigated by `type.toString()` hashing, not clean).
- **Phase B mostly complete** (B1-B4 done) -- `mir::ModuleBody` owns behavioral IR, `mir::Module` is instance record with `body_id`. Shared bodies active. `mir::Process` carries no instance identity. `SpecializationMap` required (no fallback). Remaining: B6 (HIR split).
- **Phase C complete** (C1-C3) -- specialization-local slot identity via `CollectBodyLocalDecls`. `mir::InstancePlacement` is source of truth for design-state placement. Alias resolution eliminated from behavioral codegen.
- **Phase D mostly complete** (D1, D3, D4 done) -- bindings separated (`CompileBindings` + `realization::AssembleBindings`). Metadata serialization in `realization::BuildDesignMetadata`. `main()` in `realization::EmitDesignMain()`. `LowerMirToLlvm()` is thin wrapper: `CompileDesignProcesses` -> `EmitDesignMain` -> `FinalizeModule`. Remaining: D2 (`InstanceConstBlock`).

## Active Gaps

### Now: B3 / E1

B3 and E1 are the same immediate story. B3 is the problem; E1 is the fix.

**B3: Codegen operates on entire design**

`LowerMirToLlvm()` receives `mir::Design`. `BuildLayout()` processes all instances. Cannot parallelize or cache. After D4, `LowerMirToLlvm()` remains a thin backend wrapper that orchestrates specialization codegen + realization, but `CompileDesignProcesses` still processes the full design.

- `include/lyra/llvm_backend/lower.hpp:83-95` -- `LoweringInput` holds `const mir::Design*`
- `src/lyra/llvm_backend/lower.cpp` -- `CompileDesignProcesses()` processes full design
- `src/lyra/llvm_backend/layout/layout.cpp` -- `BuildLayout()` iterates all instances
- `include/lyra/llvm_backend/context.hpp` -- `Context` holds `const mir::Design&`

**E1: New codegen API takes one specialization**

- `LowerMirToLlvm()` operates on a single `CompiledModuleSpec`
- `LoweringInput` no longer holds `const mir::Design*`; takes specialization MIR + `SpecLayout`
- Codegen call has no design-global parameters

### Next: D2 / M2

**D2: InstanceConstBlock**

Per-instance value-only parameter values as realization artifact. Replaces `instance_param_inits` in `mir::Design`. Two instances with different value-only params produce different `InstanceConstBlock` but share specialization. Depends on A4, C2.

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

**M4: Template dedup is cross-instance optimization**

`BuildModuleVariants()` groups processes across all instances by fingerprint. Requires design-wide knowledge. In north star model, dedup is automatic from specialization. Removed by E2.

**E2: Remove template dedup path**

- Delete `BuildModuleVariants`, `ProcessTemplate`, fingerprint-based dedup
- Depends on E1

**E3: Delete legacy design-wide codegen path**

- Remove all code paths that pass full `mir::Design` to codegen
- `Context` no longer holds `const mir::Design&`
- Depends on E2

**E4: Delete compatibility adapters**

- Remove transitional codegen adapters from Phase B5
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
