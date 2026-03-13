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
  - [ ] M2c-2b -- Definition-owned repertoire descriptor
    - [x] Inspection scaffold landed
    - [x] M2c-2c partial -- Declaration payload strengthening (ordinals + interned type IDs)
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

### M2c-2b: Definition-owned repertoire descriptor (inspection scaffold landed)

Pointer-free, definition-owned, flat repertoire descriptor built from the M2c-2a artifact inventory. Converts extraction-layer slang pointers into deterministic coordinate ordinals. The descriptor captures all compile-relevant artifacts and the constructor-selection coordinates under which each exists.

This is inspection/modeling infrastructure only. It does not replace the v1 specialization hashing or wire into the discriminator. Hashing integration is deferred to M2c-3. Payload identities (decl names, instance names, process ordinals) are provisional inspection labels, not semantic identity. M2c-2c must strengthen payloads before M2c-3 can build on this.

Data model:

- `SelectionStepDesc` -- one pointer-free coordinate step. `kBranch` alternatives get source-order `alt_index` ordinals; `kArrayEntry` entries keep their iteration index.
- `RepertoireArtifactDesc` -- artifact kind + coordinate + provisional payload. Payload identity is name-based (decl, child instance) or encounter-order-based (process, continuous assign). These are NOT semantic identity.
- `DefinitionRepertoireDesc` -- flat sorted vector of artifact descriptors.

Builder phases: (1) build artifact inventory via M2c-2a, (2) traverse definition body to assign branch ordinals by source order, (3) lower each artifact to pointer-free descriptor with bucket-local ordinals, (4) sort deterministically.

Files: `repertoire_descriptor.hpp`, `repertoire_descriptor.cpp`. Tests: `repertoire_descriptor/default.yaml`.

### M2c-2c partial: Declaration payload strengthening

Declaration payloads now use coordinate-local ordinals and structured compile-owned type IDs instead of raw name strings:

- `DeclArtifactDesc` contains `local_ordinal` (encounter order within coordinate bucket) and `TypeDescId` (reference into the definition's `CompileOwnedTypeStore`).
- `CompileOwnedTypeStore` is a per-definition interning store of compile-owned type facts. Each entry has a `TypeDescKind` and a typed payload variant. Structurally identical types within the same definition reuse the same `TypeDescId` via linear deduplication in `InternCompileOwnedType`.
- Type lowering captures compile-owned representation shape only: integral width/signedness/four-state, packed array element + range, packed struct/union field layout, enum base + member values, unpacked container element types. Names and runtime-owned properties (unpacked dimensions, queue bounds) are stripped.
- Debug labels (declaration names) are not part of semantic identity. The dump function recovers names from the body at dump time; no debug labels are stored in the semantic descriptor.
- `DefinitionRepertoireDesc` now owns a `CompileOwnedTypeStore` shared by all declaration artifacts.

Process/child-instance/continuous-assign payloads remain provisional. Process semantic identity is the main open blocker for M2c completion. M2c-3 (discriminator integration) remains blocked until process identity is resolved.

Files: `compile_owned_type_desc.hpp`, `compile_owned_type_desc.cpp`, `repertoire_descriptor.hpp`, `repertoire_descriptor.cpp`. Tests: `repertoire_descriptor/default.yaml`.

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

## Architecture Gap: Type Ownership Model

The codebase has two type worlds that serve different owners:

- `TypeArena` (`common/type.hpp`): Unified type representation for HIR, MIR, codegen, layout, runtime. Captures both compile-owned and constructor-owned properties in one interned type graph.
- `CompileOwnedTypeStore` (`lowering/ast_to_hir/compile_owned_type_desc.hpp`): Compile-owned type facts only, for specialization identity. Strips constructor-owned properties (dimensions, bounds) and names.

This is not accidental duplication. It comes from a real architectural split between three ownership worlds:

- **Compile-owned facts** determine specialization identity and compiled artifact shape. Two instances with identical compile-owned facts share a compiled body. Examples: packed width, signedness, 2-state vs 4-state, struct/union layout, enum values.
- **Constructor-owned facts** determine realization/layout without recompilation. Same compiled body, different constructor-time metadata. Examples: unpacked array dimensions, queue bounds, parameter transmission values.
- **Runtime-owned facts** determine mutable execution state. Examples: dynamic array contents, queue elements, variable values.

The long-term clean shape is not "one type arena for everything." It is three owner-specific projections:

1. Frontend semantic type world (slang AST types -- borrowed, not owned)
2. Compile-owned type facts (`CompileOwnedTypeStore` -- specialization identity)
3. Runtime/layout value-type facts (`TypeArena` -- codegen, layout, runtime)

`CompileOwnedTypeStore` is the right direction for compile-owned projection. This ownership split should be treated as a first-class architectural principle, not left as an M2c implementation detail.

### Audit: docs that still blur ownership boundaries

Three audit questions for each doc:

- What is the compile unit? (specialization = compile-owned equivalence class)
- What does constructor-time own/do? (realization metadata, layout, per-instance values)
- What does runtime own/do? (mutable state, scheduling, execution)

Docs that still blur compile-owned vs constructor-owned:

- `pipeline-contract.md`: Says "types are language-level, owned by HIR" without distinguishing compile-owned from constructor-owned properties. Does not frame what crosses the compilation/realization boundary.
- `state-layout.md`: Correctly describes three phases (Compilation, Realization, Simulation) but does not frame them as three type ownership boundaries or explain what each phase owns.
- `TypeArena` interning in `type-system.md` captures both compile-owned and constructor-owned properties in one graph. The doc now has a "Type Ownership Boundaries" section but does not yet explain when the unified arena is the right shape vs when owner-specific projections are needed.

These are documentation/terminology gaps, not code correctness issues. The code already implements the right compile-owned projection (M2c-2c). The docs need to catch up.

### Tracked updates

- [x] `compilation-model.md`: Added "Type Ownership" section with compile-owned / constructor-owned / runtime-owned
- [x] `architecture-principles.md`: Fixed "execution-time" -> "compile-owned" terminology
- [x] `type-system.md`: Added "Type Ownership Boundaries" section
- [ ] `pipeline-contract.md`: Clarify "types are language-level" to distinguish ownership worlds
- [ ] `state-layout.md`: Frame three phases as three ownership boundaries

## Open Questions

1. **v1 discriminator completeness**: The v1 discriminator captures variable/net names and resolved types. It does not capture procedural/behavioral compile-owned shape. What is the minimal complete compile-owned representation? (Tracked as M2c.)
2. **Package compilation**: Packages have no instances. Separate specialization unit or separate concept?
3. **Container descriptor format**: How should SpecLayout represent container regions?
