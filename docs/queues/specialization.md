# Specialization Migration Gaps

Working queue for migrating Lyra toward specialization-based compilation.

For the stable architecture: see [compilation-model.md](../compilation-model.md).

## Progress

- [x] A -- Identity (`ModuleDefId`, `BehaviorFingerprint`, `ModuleSpecId`, `SpecializationMap`)
- [x] B1-B4 -- Body ownership (`mir::ModuleBody`, shared bodies, no instance identity in `mir::Process`)
- [x] C1-C3 -- Local storage (`CollectBodyLocalDecls`, `mir::InstancePlacement`, alias resolution eliminated)
- [x] D1-D4 -- Realization extraction (bindings, metadata, `EmitDesignMain`, `InstanceConstBlock`)
- [x] E1-E2 -- Per-spec codegen (`CompileModuleSpecSession`, variant/template-dedup deleted)
- [x] E3 -- Backend API narrowing (no `mir::Design` in codegen, layout, realization paths)
- [x] E4 -- Delete B5 compatibility adapters, representative-instance scaffolding, observer program model
- [x] M2a -- Storage assignment derived from within-group variance (ParamTransmissionTable)
- [x] M2b partial -- Declaration-based grouping, param-role deleted.
- [x] M2c partial -- Compile-owned discriminator (current pipeline complete; process body identity deferred to constructor-repertoire model)
  - [x] M2c-2a -- Artifact inventory with generate availability paths
  - [x] M2c-2b -- Definition-owned repertoire descriptor (inspection scaffold + declaration payload)
  - [x] M2c-3 -- Specialization fingerprint from definition-scoped type store (v1 removed)
- [x] B6 -- HIR ownership split (`hir::ModuleBody` per spec group, `hir::Module` instance record, ownership-shaped lowering inputs)
- [x] m2 -- Instance paths deferred to runtime (LyraPrintModulePath + instance_id)
- [ ] F1 -- Parallel specialization compilation (next milestone, see [parallel-compilation.md](../parallel-compilation.md))
  - [x] F1-design -- Parallel ownership model: per-body owned unit, identity contract, phase split
  - [x] F1-prep Cut 1 -- Per-body HIR ownership (`ModuleBody` owns arena, body-aware OriginMap, `ForkForBodyLowering`)
  - [ ] F1-prep Cut 3 -- Per-body diagnostics
  - [ ] F1-prep Cut 2 -- Per-body MIR ownership
  - [ ] F1-prep Cut 4 -- TypeArena/ConstantArena investigation
  - [ ] m3 -- ParamTransmissionTable: replace raw ParameterSymbol\* with group-scoped key (Phase 0 only, not F1-blocking)
  - [ ] F1-impl -- Per-group isolated compilation with deterministic merge
- [ ] F2 -- Specialization caching

## M2: Invert specialization/param-role dependency

Cross-cutting architectural correction. Fixes wrong causality in the pipeline contract between lowering, specialization, and realization.

**Core conceptual correction**: Specialization identity is not a parameter tuple. Parameters are inputs to a module; specialization is the compile-owned equivalence class of the body.

### Grouping contract

The grouping rule is **compiled-representation equivalence**: two instances belong to the same specialization group when every compiled-representation fact is identical between them.

Specialization identity comes from the load-bearing subset of the definition-scoped `CompileOwnedTypeStore`: integral types (packed width, signedness, 2-state vs 4-state), packed arrays, packed structs, packed unions, and enums. These are the entries that independently change compiled representation (LLVM IR shape). Unpacked/container entries (unpacked array, dynamic array, queue, associative array, unpacked struct, unpacked union) and kind-only entries (void, real, short_real, string) are not hashed -- they only differ when their element's packed type differs, which is already captured. The type store is definition-scoped because the extraction layer walks all generate branches (slang's `body.members()` includes uninstantiated `GenerateBlockSymbol`s). Types from all constructor alternatives contribute to one type store.

Generate constructs (generate-if, generate-for, generate-case) are constructor-time logic. They select which pre-compiled artifacts to install at construction time. Branch structure, artifact coordinates, artifact existence/multiplicity do not affect specialization identity. Only compiled-representation facts (type store entries) determine whether instances share a specialization.

Constructor-owned facts (unpacked dimensions, queue bounds, parameter transmission values) are stripped by `InternCompileOwnedType` and do not split specialization.

**Invariant**: Specialization identity is defined by the definition-scoped compile-owned type store. Parameters are neither classified nor hashed as a primary mechanism. Per-instance parameter transmission is a separate derived step based on within-group variance.

### Pipeline (after M2a + M2c-3)

```
BuildSpecializationMap(all_instances)            -- type-store fingerprint grouping
  -> DeriveParamTransmission(groups)             -- observe within-group variance
  -> RegisterModuleDeclarations(transmission)    -- storage from transmission
```

Grouping implementation is internal to `specialization.cpp`: `BuildDefinitionRepertoireDesc` produces the definition-scoped descriptor with compile-owned type store, `HashCompileOwnedTypeStore` hashes only the load-bearing subset (integral, packed array, packed struct, packed union, enum). Unpacked/container entries are redundant and excluded. No parameter classification, no param-role table, no `toString()`, no repertoire structure in fingerprint.

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

## M2c: Compile-owned discriminator

Specialization fingerprint based on definition-scoped compiled-representation facts.

### Definition-scoped type store contract

The specialization fingerprint depends on the type store being **definition-scoped** -- it must contain types from all generate branches (active and inactive), not just the instantiated path. This is not a traversal quirk; it is a load-bearing correctness requirement.

**Why it matters**: Two instances of the same definition on different active branches (e.g., MODE=0 vs MODE=1) must produce the same fingerprint. If the type store only captured types from the active branch, each instance would see a different set of types and incorrectly split into separate specializations.

**How it works**: slang elaborates both sides of every generate-if/case and creates `GenerateBlockSymbol`s for each. The active branch has `isUninstantiated=false`, the inactive has `isUninstantiated=true`. Both appear in `body.members()`. `BuildArtifactInventory` intentionally does NOT check `isUninstantiated` -- it walks every `GenerateBlockSymbol` unconditionally. This makes the resulting `CompileOwnedTypeStore` definition-scoped.

**Dependencies**:

- slang must include uninstantiated `GenerateBlockSymbol`s in `body.members()` (verified in slang `Scope.cpp:984-989` and `BlockSymbols.cpp:502-518`)
- `VisitScope` in `generate_repertoire.cpp` must not filter by `isUninstantiated`
- `InternCompileOwnedType` must intern types from both active and inactive branches

**Test**: `definition_scoped_type_store_includes_inactive_generate_branches` in `specialization_grouping/default.yaml`. This test puts two instances on different active branches (one sees `bit[7:0]`, the other `bit[15:0]`) and asserts they group together. If the store were instance-scoped, they would split.

### What changed (M2c-3)

- v1 discriminator removed: `CompileOwnedDeclDescriptor`, `CompileOwnedBodyDescriptor`, `IsCompileOwnedDeclaration`, `CanonicalCompileOwnedTypeRepr`, `BuildCompileOwnedBodyDescriptor`, `HashCompileOwnedBodyDescriptor` -- all deleted.
- Fingerprint now comes from `HashCompileOwnedTypeStore(desc.type_store)` where `desc` is built by `BuildDefinitionRepertoireDesc`. Only the load-bearing subset is hashed: integral, packed array, packed struct, packed union, enum. Unpacked/container and kind-only entries are excluded (redundant -- they only differ when element packed types differ).
- `type.toString()` eliminated. Type identity uses structured `CompileOwnedTypeStore` entries.
- Constructor-owned facts stripped: unpacked dimensions, queue bounds, names.
- Repertoire structure (artifact coordinates, kinds, process existence) excluded from fingerprint -- those are constructor-assembly data.
- The type store is definition-scoped because `BuildArtifactInventory` walks all generate branches (slang includes uninstantiated `GenerateBlockSymbol`s in `body.members()`).

### Remaining M2c scope

The type store covers declaration-side compiled representation. Process body identity (what code a process executes) is not yet part of the fingerprint. This is acceptable because processes with different code but the same type universe share a specialization under the constructor model -- the constructor selects which process artifacts to install.

### M2c-2a: Artifact inventory with generate availability (done)

Extraction layer that walks the full generate structure (including uninstantiated branches) and produces a flat artifact inventory. Each artifact is a slang symbol pointer annotated with its generate availability path -- the sequence of constructor-time selections required for the artifact to exist.

Data model:

- `GenerateSelection` -- one step in an availability path. A selection point is identified by `(parent path position, constructIndex)`. Two kinds: `kBranch` (generate-if/case alternative, identified by block pointer) and `kArrayEntry` (generate-for entry, identified by entry ordinal).
- `ArtifactHandle` -- a slang symbol + its availability path + artifact kind.
- `ArtifactInventory` -- flat vectors of handles (decls, processes, instances, continuous assigns).

This is observation/extraction infrastructure, not semantic identity. The block pointers are valid within the borrowed slang compilation lifetime. Later repertoire descriptor design (M2c-2b) will define what survives beyond this extraction boundary.

Files: `generate_repertoire.hpp`, `generate_repertoire.cpp`. Tests: `generate_repertoire/default.yaml`.

### M2c-2b: Definition-owned repertoire descriptor

Pointer-free, definition-owned, flat repertoire descriptor built from the M2c-2a artifact inventory. Converts extraction-layer slang pointers into deterministic coordinate ordinals. The descriptor captures all compile-relevant artifacts and the constructor-selection coordinates under which each exists.

The descriptor serves two purposes: (1) it owns the `CompileOwnedTypeStore` used by the specialization fingerprint, and (2) it provides constructor-assembly data (artifact coordinates, kinds, payloads) for future realization phases.

The type store is definition-scoped because `BuildArtifactInventory` walks all generate branches -- slang's `body.members()` includes uninstantiated `GenerateBlockSymbol`s with `isUninstantiated=true`.

Files: `repertoire_descriptor.hpp`, `repertoire_descriptor.cpp`, `compile_owned_type_desc.hpp`, `compile_owned_type_desc.cpp`. Tests: `repertoire_descriptor/default.yaml`.

### M2c-3: Specialization fingerprint from type store (done)

Replaced v1 discriminator with `HashCompileOwnedTypeStore`. The fingerprint hashes only the load-bearing subset of `CompileOwnedTypeStore` entries from the repertoire descriptor: integral types, packed arrays, packed structs, packed unions, and enums. These are the entries that independently change compiled representation (LLVM IR shape).

Unpacked/container entries (unpacked array, dynamic array, queue, associative array, unpacked struct, unpacked union) and kind-only entries (void, real, string) are excluded from the fingerprint. They only differ between instances when their element's packed type differs, which is already captured by the packed entries. Hashing them would be redundant and would falsely suggest all type-store entries define specialization identity.

- v1 internals deleted: `CompileOwnedDeclDescriptor`, `CompileOwnedBodyDescriptor`, `IsCompileOwnedDeclaration`, `CanonicalCompileOwnedTypeRepr`, `BuildCompileOwnedBodyDescriptor`, `HashCompileOwnedBodyDescriptor`.
- `type.toString()` eliminated. Structured type entries replace string-based identity.
- Repertoire structure (coordinates, artifact kinds, process existence) excluded from fingerprint.
- Constructor-owned facts (unpacked dimensions, queue bounds) stripped by `InternCompileOwnedType`.

Files: `specialization.cpp`, `specialization.hpp`. Tests: `specialization_grouping/default.yaml`.

## E3 remaining: Backend API narrowing

- `CodegenSession` has no `const mir::Design*`
- `BuildLayout()` takes narrow `LayoutModulePlan` spans, not `mir::Design`
- `BuildSlotInfo()` takes `span<SlotDesc>`, not `mir::Design`
- `ExtractRealizationData()` takes narrow parameters, not `mir::Design`
- Wrapper generation uses pre-extracted `module_base_slots`, not `design.placement`
- `Context` holds `const Layout&` (pure LLVM artifact, no `mir::Design` reference)

`LoweringInput` holds `const mir::Design*` by design -- it is the orchestration entry point. `CompileDesignProcesses` extracts narrow inputs from design during setup; no per-spec compilation, codegen, or realization path reads `mir::Design` directly.

## E4: Delete compatibility adapters

Deleted (core contract tightening):

- `representative_module_index` from `SpecProcessView` (backend-only compatibility field)
- `current_instance_id_` / `SetCurrentInstanceId` / `GetCurrentInstanceId` from `Context` (static instance identity for shared process lowering)
- `module_base_slot_id_` from `Context` (fake design-global rebasing)
- `%m` lowering now requires `kSpecializationLocal` with dynamic instance identity
- kModuleSlot + kDesignGlobal identity mapping removed from `ResolveDesignGlobalSlotId`, `EmitSignalId`, `GetSlotRootPointer`, `GetSignalSlotPointer` -- replaced with `InternalError` (architecture violation)

Deleted (observer program model, PR #548):

- Observer programs (strobe, monitor-setup, monitor-check) use uniform ABI with `ObserverContext*`
- `EnterObserverSpecializationLocalContext()` installs `kSpecializationLocal` addressing for module-scoped observers
- Design-global rebasing paths (`kModuleSlot + kDesignGlobal`) replaced with `InternalError`
- Thunk terminology replaced: `ThunkKind` -> `RuntimeProgramKind`, thunk fields -> program fields

## B6: HIR ownership split

`hir::ModuleBody` is specialization-owned shared behavioral HIR (processes, functions, tasks). `hir::Module` is an instance/realization record (symbol, per-instance SymbolIds, param values, body_id reference). `hir::Design.module_bodies` owns all shared bodies.

AST->HIR lowering uses ownership-shaped prepared inputs. `PrepareModuleLoweringInputs` does one structural walk per instance, then immediately partitions into `BodyLoweringInput` (definition-scoped behavioral content + prepared var_init tuples) and `InstanceRegistrationInput` (per-instance registration pointers). `CollectedMembers` is internal to preparation only. `LowerModuleBody` consumes `BodyLoweringInput` once per specialization group. `CollectModuleInstance` consumes `InstanceRegistrationInput` per instance (lookup-only, born-complete records). HIR->MIR: `LowerModule` takes `const hir::ModuleBody&` for behavioral content, `CollectBodyLocalDecls` takes `hir::Module&` for per-instance registration SymbolIds.

Variables/nets/param_slots remain on `hir::Module` as per-instance registration artifacts (not body-owned declarations). This is the current justified ownership boundary: these SymbolIds are consumed by design-global place allocation (`CollectDesignDeclarations`) and port binding compilation, both of which are instance-scoped consumers. If any subset is later identified as specialization-shared semantic ownership, it should move into `hir::ModuleBody`.

## F1: Parallel specialization compilation

See [parallel-compilation.md](../parallel-compilation.md) for the full design.

The core model: Phase 0 (sequential global setup) produces immutable shared reference data. Phase 1 (per-group isolated compilation) produces per-body owned units (`hir::ModuleBody` with embedded arena, body-local IDs). Phase 2 (deterministic assembly) collects bodies and builds design-wide artifacts. Body-local IDs stay body-local permanently -- no rebasing.

The old "m1 -- DesignState struct is monolithic" label is retired. There is no `DesignState` struct. The real concern (monolithic boundary bags) is addressed as part of F1-prep, not as standalone cleanup.

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
