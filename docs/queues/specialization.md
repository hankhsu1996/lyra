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
- [x] M2a -- Storage assignment derived from within-group variance (ParamTransmissionTable)
- [x] M2b partial -- Declaration-based grouping, param-role deleted. v1 discriminator captures declaration-side shape only.
- [ ] M2c -- Complete compile-owned discriminator (procedural/behavioral shape, runtime-owned type filtering)
  - [x] M2c-2a -- Artifact inventory with generate availability paths
- [ ] E3 remaining -- Remove `mir::Design` from `LoweringInput`, narrow `Context` and `BuildLayout`
- [ ] E4 -- Delete B5 compatibility adapters, representative-instance scaffolding
- [ ] B6 -- HIR ownership split (depends on post-M2 grouping contract)
- [ ] m1 -- DesignState struct is monolithic
- [ ] m2 -- Instance paths baked into codegen
- [ ] m3 -- ParamTransmissionTable uses raw ParameterSymbol\* as key (should be group-scoped)
- [ ] F1 -- Parallel specialization compilation
- [ ] F2 -- Specialization caching

## M2: Invert specialization/param-role dependency

Cross-cutting architectural correction. Fixes wrong causality in the pipeline contract between lowering, specialization, and realization.

**Core conceptual correction**: Specialization identity is not a parameter tuple. Parameters are inputs to a module; specialization is the compile-owned equivalence class of the body.

### Grouping contract

The grouping rule is **compile-owned body equivalence**: two instances belong to the same specialization group when every compile-owned fact is identical between them.

The v1 discriminator captures **declaration-side compile-owned shape**: module-local variable and net declarations (names and resolved types). This captures differences where parameters affect packed widths, signedness, or 2-state vs 4-state, because those decisions are reflected in the resolved types of these declarations. Parameters and localparams are excluded -- their effects are observed indirectly through the resolved types they influence.

**v1 does not capture all compile-owned facts.** Two known gaps:

- **Procedural/behavioral shape**: generate branch selections that affect code shape without introducing new declarations are not captured. Instances with different procedural code shape but identical declaration sets will incorrectly share a specialization.
- **Runtime-owned type leakage**: `type.toString()` may encode runtime-owned differences (e.g., unpacked container sizing) that should not split specialization, causing oversplitting.

These are tracked as M2c.

Differences that are runtime-owned (assembly/constructor-time values, realization metadata) must not split specialization. The grouping ignores those.

**Invariant**: Specialization identity is defined directly from compile-owned body facts. Parameters are neither classified nor hashed as a primary mechanism. Per-instance parameter transmission is a separate derived step based on within-group variance.

### Pipeline (after M2a + M2b)

```
BuildSpecializationMap(all_instances)            -- v1 declaration-shape grouping
  -> DeriveParamTransmission(groups)             -- observe within-group variance
  -> RegisterModuleDeclarations(transmission)    -- storage from transmission
```

Grouping implementation is internal to `specialization.cpp`: `BuildCompileOwnedBodyDescriptor` collects declaration facts, `HashCompileOwnedBodyDescriptor` produces the fingerprint. No parameter classification, no param-role table.

### Storage class consequence

Storage / const-block assignment is derived from within-group parameter variance, not from an upfront global role classifier. A parameter that is constant across all instances in a group was absorbed by the specialization choice -- it has no runtime storage. A parameter that varies within a group is a per-instance transmission argument and gets a const-block slot.

### Why this is better

- Specialization identity is not a parameter tuple; it is the compile-owned equivalence class of the body
- No correctness pressure on an upfront classifier
- No need to recover parameter provenance from resolved types
- Grouping rule can evolve independently of parameter transmission
- Matches ownership model: specialization-owned differences are absorbed into the compiled body, per-instance differences stay outside

### Files involved

- `include/lyra/lowering/ast_to_hir/specialization.hpp` -- public grouping API
- `src/lyra/lowering/ast_to_hir/specialization.cpp` -- descriptor types, fingerprint, grouping (all internal)
- `include/lyra/lowering/ast_to_hir/param_transmission.hpp` -- M2a: ParamTransmissionTable, DeriveParamTransmission
- `src/lyra/lowering/ast_to_hir/param_transmission.cpp` -- M2a: within-group variance derivation
- `src/lyra/lowering/ast_to_hir/design.cpp` -- pipeline: grouping -> transmission -> registration

## M2c: Complete compile-owned discriminator

The v1 discriminator (M2b) captures declaration-side shape only. M2c expands it to cover all compile-owned facts that affect compiled code.

Known gaps:

1. **Procedural shape without declaration change**: A generate branch that selects different procedural code (e.g., different `always` body) without introducing different variable/net declarations. The v1 discriminator sees identical declaration sets and merges them into one specialization.
2. **Runtime-owned type filtering**: `type.toString()` may include runtime-owned type properties (unpacked container sizes, dynamic array bounds) that should not split specialization. The canonicalizer needs to strip these.

Scope: define what "complete compile-owned representation" means, expand the descriptor, narrow the type canonicalizer.

### M2c-2a: Artifact inventory with generate availability (done)

Extraction layer that walks the full generate structure (including uninstantiated branches) and produces a flat artifact inventory. Each artifact is a slang symbol pointer annotated with its generate availability path -- the sequence of constructor-time selections required for the artifact to exist.

Data model:

- `GenerateSelection` -- one step in an availability path. A selection point is identified by `(parent path position, constructIndex)`. Two kinds: `kBranch` (generate-if/case alternative, identified by block pointer) and `kArrayEntry` (generate-for entry, identified by entry ordinal).
- `ArtifactHandle` -- a slang symbol + its availability path + artifact kind.
- `ArtifactInventory` -- flat vectors of handles (decls, processes, instances, continuous assigns).

This is observation/extraction infrastructure, not semantic identity. The block pointers are valid within the borrowed slang compilation lifetime. Later repertoire descriptor design (M2c-2b) will define what survives beyond this extraction boundary.

Files: `generate_repertoire.hpp`, `generate_repertoire.cpp`. Tests: `generate_repertoire/default.yaml`.

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

Depends on the post-M2 grouping contract: the specialization groups that define which instances share a body are now defined by compile-owned body equivalence.

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

1. **v1 discriminator completeness**: The v1 discriminator captures variable/net names and resolved types. It does not capture procedural/behavioral compile-owned shape. What is the minimal complete compile-owned representation? (Tracked as M2c.)
2. **Package compilation**: Packages have no instances. Separate specialization unit or separate concept?
3. **Container descriptor format**: How should SpecLayout represent container regions?
