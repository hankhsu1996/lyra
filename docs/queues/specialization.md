# Specialization Migration Gaps

Working queue for migrating Lyra toward specialization-based compilation.

For the stable architecture: see [compilation-model.md](../compilation-model.md).

## Progress

- [x] A -- Identity (`ModuleDefId`, `BehaviorFingerprint`, `ModuleSpecId`, `SpecializationMap`)
- [x] B1-B4 -- Body ownership (`mir::ModuleBody`, shared bodies, no instance identity in `mir::Process`)
- [x] C1-C3 -- Local storage (`CollectBodyLocalDecls`, `mir::InstancePlacement`, alias resolution eliminated)
- [x] D1-D4 -- Realization extraction (bindings, metadata, `EmitDesignMain`, `InstanceConstBlock`)
- [x] E1-E2 -- Per-spec codegen (`CompileModuleSpecSession`, variant/template-dedup deleted)
- [x] E3 partial -- `CodegenSession` has no `const mir::Design*`
- [ ] **M2 -- Invert specialization/param-role dependency** (next)
- [ ] E3 remaining -- Remove `mir::Design` from `LoweringInput`, narrow `Context` and `BuildLayout`
- [ ] E4 -- Delete B5 compatibility adapters, representative-instance scaffolding
- [ ] B6 -- HIR ownership split (depends on post-M2 grouping contract)
- [ ] m1 -- DesignState struct is monolithic
- [ ] m2 -- Instance paths baked into codegen
- [ ] m4 -- ParamRole uses slang pointer as grouping key
- [ ] F1 -- Parallel specialization compilation
- [ ] F2 -- Specialization caching

## M2: Invert specialization/param-role dependency

Cross-cutting architectural correction. Fixes wrong causality in the pipeline contract between lowering, specialization, and realization. Must land before E3/E4 narrowing -- continuing to build on the wrong conceptual seam makes future work harder.

**Core conceptual correction**: Specialization identity is not a parameter tuple. Parameters are inputs to a module; specialization is the compile-owned equivalence class of the body. Two instances belong to the same specialization when their bodies produce identical compiled code -- regardless of how their parameter values differ, as long as those differences are not compile-owned.

**Problem**: The current pipeline computes param roles first, then derives specialization grouping from those roles. This is backwards. Specialization identity and parameter transmission are conflated -- the classifier tries to answer both "which instances can share code?" and "which params need per-instance storage?" in one step.

### Grouping contract

The grouping rule is **compile-owned body equivalence**: two instances belong to the same specialization group when every fact that the compiler bakes into the compiled body is identical between them. This includes resolved types, packed widths, generate selections, and any other elaboration-time decision that affects emitted code shape.

Differences that are runtime-owned (assembly/constructor-time values, realization metadata) must not split specialization. The grouping ignores those.

**Invariant**: If a parameter difference affects any compile-owned body fact, those instances must already be in different groups. Only within-group parameter variance becomes per-instance transmission.

Group-local variance alone is not sufficient unless the grouping has already absorbed every compile-owned difference. The grouping contract and the transmission derivation are coupled through this invariant -- getting one right requires the other.

### Current state (transitional)

The current `ClassifyParamRoles` is transitional compatibility logic with three passes:

1. Syntax-visible shape collection (AST walk for non-procedural param refs)
2. Cross-instance value comparison to find kValueOnly candidates
3. Structural shape promotion (syntax-walk recovery for packed-width params)

**Transitional grouping rule**: During migration, specialization grouping remains conservatively over-discriminating if needed. The current `ComputeStructuralFingerprint` hashes resolved type strings and parameter values -- this is more conservative than necessary but correct. Remove param-role-driven grouping only after the fingerprint independently captures all compile-owned differences currently relied on.

### Target pipeline

**Current pipeline** (wrong causality):

```
ClassifyParamRoles(all_instances)        -- param roles first
  -> RegisterModuleDeclarations(roles)   -- storage class from roles
  -> BuildSpecializationMap(roles)       -- grouping from roles
```

**Target pipeline** (correct causality):

```
ComputeSpecGroups(all_instances)         -- grouping by compile-owned body equivalence
  -> DeriveParamTransmission(groups)     -- observe within-group variance
  -> RegisterModuleDeclarations(transmission)
```

After M2, `ClassifyParamRoles` simplifies to a group-local variance check. But that check is only correct because the grouping has already separated instances with different compile-owned body facts.

### Storage class consequence

Storage / const-block assignment is derived from within-group parameter variance, not from an upfront global role classifier. A parameter that is constant across all instances in a group was absorbed by the specialization choice -- it has no runtime storage. A parameter that varies within a group is a per-instance transmission argument and gets a const-block slot.

### Why this is better

- Specialization identity is not a parameter tuple; it is the compile-owned equivalence class of the body
- No correctness pressure on an upfront classifier
- No need to recover parameter provenance from resolved types
- Grouping rule can evolve independently of parameter transmission
- Matches ownership model: specialization-owned differences are absorbed into the compiled body, per-instance differences stay outside

### Files involved

- `src/lyra/lowering/ast_to_hir/param_role.cpp` -- simplifies to group-local variance check (correct only because grouping absorbed compile-owned differences)
- `src/lyra/lowering/ast_to_hir/specialization.cpp` -- fingerprint becomes self-contained (no ParamRoleTable input), must capture all compile-owned body facts
- `src/lyra/lowering/ast_to_hir/design.cpp` -- pipeline reordering
- `src/lyra/lowering/ast_to_hir/module.cpp` -- storage class derived from within-group variance, not from upfront role classifier

## E3 remaining: Backend API narrowing

`CodegenSession` no longer carries `const mir::Design*` (done). Remaining scope:

- `include/lyra/llvm_backend/lower.hpp` -- `LoweringInput` holds `const mir::Design*`
- `src/lyra/llvm_backend/layout/layout.cpp` -- `BuildLayout()` iterates all instances
- `include/lyra/llvm_backend/context.hpp` -- `Context` holds `const Layout&`

These are incremental narrowing tasks, not architectural blockers.

## E4: Delete compatibility adapters

- Remove transitional codegen adapters from Phase B5
- Remove representative-instance ID usage for %m path support
- Codegen operates natively on `ModuleBody` + instance records

## B6: HIR ownership split

Same body/instance split at HIR level. `hir::ModuleBody` owned by specialization, `hir::Module` becomes instance record. AST->HIR lowering produces one body per specialization group.

Depends on the post-M2 grouping contract: the specialization groups that define which instances share a body are no longer param-role-defined but compile-owned-equivalence-defined.

## CI / Policy Gates

| Gate                                                           | Enforcement             | Status        |
| -------------------------------------------------------------- | ----------------------- | ------------- |
| G1: No instance identity in specialization MIR                 | Structural (type shape) | Enforced      |
| G2: Codegen API has no design input                            | API signature check     | Not added yet |
| G3: Within-group param variance is transmitted per-instance    | Regression test         | Not added yet |
| G4: Compile-owned differences produce distinct specializations | Regression test         | Not added yet |
| G5: Specialization IR is topology-independent                  | Regression test         | Not added yet |
| G6: No instance paths in specialization artifacts              | Policy check            | Not added yet |
| G7: Specialization grouping is deterministic                   | Regression test         | Not added yet |

## Open Questions

1. **BehaviorFingerprint granularity**: What is the minimal compile-owned representation that should define specialization identity? Resolved compile-owned body facts, lowered HIR/MIR shape, or a hybrid?
2. **Package compilation**: Packages have no instances. Separate specialization unit or separate concept?
3. **Container descriptor format**: How should SpecLayout represent container regions?
4. **MIR interpreter alignment**: Debug-only. Migrate in lockstep or defer?
