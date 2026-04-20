#Parallel Specialization Compilation(F1)

Target model for parallelizing specialization group compilation.

Prerequisite work: B1-B6 (body ownership), E1-E4 (backend narrowing), M2 (grouping contract). See [compilation-model.md](compilation-model.md) for the specialization data model.

## 1. Phase Split

Three phases, applied within each lowering layer (AST->HIR, HIR->MIR):

**Phase 0: Sequential global setup.** Produces immutable shared reference data that all groups read. No group-local work.

**Phase 1: Per-group isolated compilation.** Each specialization group compiles in complete isolation. Reads only immutable shared reference data from Phase 0. Writes only to a group-owned body unit. No group may observe another group's state during this phase.

**Phase 2: Deterministic assembly.** Body units are collected in deterministic order (SpecializationMap group order). Per-instance records are assembled. Design-wide artifacts are produced.

### AST->HIR phases

| Phase | Work                                                                                                                                       | Shared state access                                                                                  |
| ----- | ------------------------------------------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------------------------- |
| 0     | `RegisterModuleDeclarations` (all instances), `InternBuiltinTypes`, `PrepareModuleLoweringInputs`, package lowering, port binding lowering | Mutable: SymbolTable, ScopeTable, TypeArena, ConstantArena, hir::Arena                               |
| 1     | `LowerModuleBody` per specialization group                                                                                                 | Read: frozen Phase 0 symbol/type/constant tables, prepared inputs. Write: group-owned body unit only |
| 2     | `CollectModuleInstance` per instance (pure lookup), assemble `hir::Design`                                                                 | Read: frozen Phase 0 tables, body units                                                              |

### HIR->MIR phases

| Phase | Work                                                                                                   | Shared state access                                                                     |
| ----- | ------------------------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------- |
| 0     | `CollectDesignDeclarations`, `InternBuiltinTypes`, package init process lowering, `BuildMirSpecGroups` | Mutable: mir::Arena (design-global places), DesignDeclarations                          |
| 1     | `CollectBodyLocalDecls` + `LowerModule` per specialization group                                       | Read: frozen DesignDeclarations, frozen input tables. Write: group-owned body unit only |
| 2     | Assemble `mir::Design`, build placement map, per-instance const blocks, compile port bindings          | Read: body units, frozen declarations                                                   |

### MIR->LLVM

Sequential for F1. F1's core milestone is parallelizing semantic specialization compilation (AST->HIR and HIR->MIR). LLVM Module/Context are not thread-safe. Keeping LLVM sequential preserves a clean first parallel cut. Later LLVM parallelism can be a separate design using per-group IR modules and linking, if profiling shows LLVM codegen is the bottleneck.

## 2. Identity Contract

### Design-global IDs (must be globally unique and stable)

These IDs are referenced across specialization group boundaries or consumed by design-wide assembly:

| ID type                      | Why global                                                                                                                        | Produced in                        |
| ---------------------------- | --------------------------------------------------------------------------------------------------------------------------------- | ---------------------------------- |
| `SymbolId` (module-level)    | Port bindings, design declarations, placement reference symbols across instances                                                  | Phase 0                            |
| `TypeId`                     | Types are shared reference data. A `bit[31:0]` in group A and group B must resolve to the same TypeId for design-wide consistency | Phase 0                            |
| `ConstId`                    | Constants are shared reference data. Same reasoning as TypeId -- constants are interned with dedup semantics                      | Phase 0 + Phase 1 (see below)      |
| `ModuleBodyId`               | Cross-reference between instance records and shared bodies                                                                        | Phase 2 (assembly-time assignment) |
| `ModuleDefId`                | Definition identity, used across groups                                                                                           | Phase 0                            |
| Design-global `mir::PlaceId` | Package/design-level place references used by bindings and cross-group consumers                                                  | Phase 0                            |

### Body-local IDs (permanently scoped to one body)

These IDs are never referenced outside their owning body. They do not need global uniqueness. They remain body-local permanently -- no rebasing into a global space:

| ID type                                                                              | Why body-local                                                                                | Lifetime  |
| ------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------- | --------- |
| `hir::ExpressionId`, `StatementId`, `ProcessId`, `FunctionId`, `TaskId`, `PatternId` | Referenced only within the body's processes/functions/tasks. No cross-body consumer.          | Body unit |
| Body-local `mir::PlaceId` (kModuleSlot, kLocal, kTemp)                               | Referenced only within body's MIR processes/functions. Backend resolves through body context. | Body unit |
| Body-local `mir::ProcessId`, `mir::FunctionId`                                       | Referenced only from body's `ModuleBody.processes/functions` vectors.                         | Body unit |
| `OriginId`                                                                           | Maps MIR nodes to HIR source. MIR nodes are body-local, so origins are body-local.            | Body unit |

### Body-local lowering symbols

Function/task parameters and process-local variables (loop variables, etc.) are registered during Phase 1 body lowering. These are body-scoped names:

- They are referenced only within the body's HIR expressions (`NameRefExpressionData.symbol`)
- No design-wide consumer references them by SymbolId
- They do not need to be in the global SymbolTable during Phase 1

**Contract**: Phase 1 body lowering must use a group-local symbol environment for body-local names. These symbols are not published to the global SymbolTable. No current downstream consumer requires global identity for body-local names. If a future consumer needs persistent scope identity, that requirement must be explicitly justified.

### Lowering scopes

`ScopeTable` scopes created during body lowering (module scope, block scopes) are ephemeral lowering state. They exist to track the current scope stack during expression/statement lowering. No downstream consumer (HIR->MIR, backend, dumper) reads scope identity from Phase 1.

**Contract**: Body-lowering scopes are ephemeral group-local state. They are not merged into a global ScopeTable.

### TypeId and ConstId: shared reference identity

Types and constants are shared reference data with dedup (interning) semantics. A type interned from two different groups must produce the same TypeId. This makes them inherently global.

**Contract**: TypeIds and ConstIds are design-global. All groups must agree on identity.

**TypeArena target shape**: Built entirely in Phase 0 and frozen before Phase 1. Phase 0 seeds all body-reachable types (via AST visitor prepass) and interns the compile-global builtin semantic type catalog. Phase 1 body lowering reads TypeArena as immutable shared reference data. No synchronized shared interner needed.

Rationale -- measured Phase 1 type interning on representative designs:

| Design   | Phase 1 intern calls | Hits   | Misses | Hit rate |
| -------- | -------------------- | ------ | ------ | -------- |
| ibex     | 45,689               | 45,598 | 91     | 99.8%    |
| pipeline | 185                  | 180    | 5      | 97.3%    |

All misses concentrated in packed-array expression-result types not visited by Phase 0 declaration registration. Body-type seeding closes this gap.

**ConstantArena target shape**: Split into design-global (Phase 0) and body-local (Phase 1) arenas. Phase 0 owns parameter defaults, package constants, port binding constants -- frozen after Phase 0. Each Phase 1 body owns its own arena for behavioral literals and desugared constants. ConstId remains arena-local. Cross-domain constant references (body-local HIR referencing Phase 0 constants) use an explicit reference variant, analogous to DesignFunctionRef and PlaceRoot::kDesignGlobal. Body-local constants resolve from the body-owned arena; design-global constants resolve from the design arena through the explicit cross-domain reference.

Rationale -- measured Phase 1 constant interning on representative designs:

| Design   | Phase 1 intern calls | Hits  | Misses | New constants | Hit rate |
| -------- | -------------------- | ----- | ------ | ------------- | -------- |
| ibex     | 6,640                | 5,178 | 1,462  | +1,462        | 78%      |
| pipeline | 13                   | 6     | 7      | +7            | 46%      |

Phase 1 constant creation is real and body-driven. Misses concentrated in behavioral literals. Top body (ibex_tracer) created 962 constants alone. A synchronized shared interner is not the right shape -- the miss rate is too high and the creation is genuinely body-local. Ownership split is the correct mechanism.

**Note**: The ConstantArena split is target architecture, not yet landed. Today ConstantArena is a single global mutable arena written by both Phase 0 and Phase 1.

## 3. Artifact Ownership Table

Target-state classification by permanent identity, not by implementation container. Rows marked with (\*) are not yet implemented.

| Artifact                                                     | Permanent scope                                     | Produced in        | Mutability after production |
| ------------------------------------------------------------ | --------------------------------------------------- | ------------------ | --------------------------- |
| SymbolTable (module-level entries)                           | Design-global                                       | Phase 0            | Immutable after Phase 0     |
| Body-local symbol environment (function/task params, locals) | Body-local (group-owned, not in global SymbolTable) | Phase 1            | Immutable after Phase 1     |
| TypeArena                                                    | Design-global (shared reference)                    | Phase 0            | Immutable after Phase 0     |
| ConstantArena (design-global) (\*)                           | Design-global (shared reference)                    | Phase 0            | Immutable after Phase 0     |
| ConstantArena (body-local) (\*)                              | Body-local                                          | Phase 1            | Immutable after Phase 1     |
| hir::Arena (body content)                                    | Body-local                                          | Phase 1            | Immutable after Phase 1     |
| hir::Arena (design-level: port bindings)                     | Design-global                                       | Phase 0            | Immutable after Phase 0     |
| hir::ModuleBody                                              | Body-local (in body unit)                           | Phase 1            | Immutable after Phase 1     |
| hir::Module                                                  | Per-instance                                        | Phase 2            | Immutable after Phase 2     |
| DesignDeclarations                                           | Design-global                                       | Phase 0 (HIR->MIR) | Immutable after Phase 0     |
| mir::Arena (design-global places)                            | Design-global                                       | Phase 0 (HIR->MIR) | Immutable after Phase 0     |
| mir::Arena (body content)                                    | Body-local                                          | Phase 1 (HIR->MIR) | Immutable after Phase 1     |
| mir::ModuleBody                                              | Body-local (in body unit)                           | Phase 1 (HIR->MIR) | Immutable after Phase 1     |
| OriginMap entries                                            | Body-local                                          | Phase 1 (HIR->MIR) | Immutable after Phase 1     |
| Diagnostics                                                  | Body-local (during Phase 1)                         | Phase 1            | Collected in Phase 2        |
| mir::Design (placement, modules, slots)                      | Design-global                                       | Phase 2 (HIR->MIR) | Immutable after Phase 2     |
| LLVM Module / Context                                        | Design-global                                       | Sequential (F1)    | N/A for F1                  |

## 4. Per-Group Compilation Product: The Per-Body Owned Unit

The primary Phase 1 product is `hir::ModuleBody` -- the first-class per-body owned artifact. It owns its body-local HIR storage (arena) as internal state. Phase 2 consumes body units without re-lowering.

### HIR per-body unit (`hir::ModuleBody`)

`hir::ModuleBody` owns all body-local compilation products:

- Body structure: processes, functions, tasks (IDs are body-local)
- Body-local arena: all expressions, statements, processes, functions, tasks, patterns
- Body-local symbol environment

**Downstream dependency contract for HIR->MIR**: Lowering one body unit to MIR requires:

- The body unit itself (structure + arena)
- Frozen design-global reference tables from Phase 0: SymbolTable (module-level entries), TypeArena, ConstantArena
- Frozen DesignDeclarations (design-global places, package functions)

It does NOT require:

- Other groups' body units
- Merged symbol/scope tables
- Any Phase 2 assembly product

This is the critical isolation guarantee. If any HIR->MIR lowering path for one body requires another body's products, F1 is broken.

### MIR body unit

Contains everything produced by `CollectBodyLocalDecls` + `LowerModule` for one specialization group:

- `body`: `mir::ModuleBody` (slots, processes, functions -- IDs are body-local)
- `arena`: `mir::Arena` (body-local places, processes, functions)
- `body_decls`: `BodyLocalDecls` (symbol -> PlaceId mapping within body unit's arena)
- `origins`: body-local OriginMap entries
- `diagnostics`: errors/warnings produced during this group's lowering

**Downstream dependency contract for MIR->LLVM**: Compiling one MIR body unit requires:

- The body unit itself (body + arena)
- Frozen design-global MIR arena (package/design-level places and functions)
- Frozen Layout (spatial metadata, computed from design-wide slot descriptors + placement)
- Pre-computed SpecLayout (slot stability classification for this body)

It does NOT require:

- Other groups' body units
- Merged origin maps or diagnostics
- Any body-local declaration map (body_decls is consumed entirely within Phase 1)

## 5. Assembly Contract (Phase 2)

Phase 2 collects body units and produces design-wide artifacts. No re-lowering. Deterministic ordering from SpecializationMap group order.

### Per-body assembly

| Body unit field        | Assembly action                                                                                                                                                                                              | Ordering                           |
| ---------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ---------------------------------- |
| `body` (ModuleBody)    | Stored in the AST->HIR `module_bodies` vector (sibling to `hir::Design`), indexed by `body_id`                                                                                                               | Group order from SpecializationMap |
| `arena`                | Stored alongside body (body unit stays owned)                                                                                                                                                                | Indexed by `body_id`               |
| `symbols` (body-local) | Remain in body unit. No current downstream consumer requires global SymbolTable publication. If a future consumer needs global identity for body-local names, that requirement must be explicitly justified. | N/A (not merged)                   |
| `origins`              | Collected into design-wide OriginMap                                                                                                                                                                         | Group order                        |
| `diagnostics`          | Concatenated into design-wide diagnostic list                                                                                                                                                                | Group order                        |

### Per-instance assembly (Phase 2 only)

For each instance in each group:

- `CollectModuleInstance` produces an `hir::Module` (pure lookup, no lowering)
- `body_id` assigned from the group's `ModuleBodyId`
- Instance added to `hir::Design.modules`

For HIR->MIR Phase 2:

- `mir::Module` created with `body_id` referencing the group's `mir::ModuleBodyId`
- Placement computed (running base from package slot count + body sizes)
- `InstanceConstBlock` built from design-global param inits
- Port bindings compiled (design-global, no body dependency)

### Body-local ID resolution

Body-local IDs (HIR expression/statement/process IDs, MIR place/process/function IDs) remain body-local permanently. Downstream consumers resolve them through the body unit:

- HIR->MIR: for each body, resolves `hir::Arena` from the body unit (indexed by `body_id`)
- MIR->LLVM: for each body, resolves `mir::Arena` from the body unit (indexed by `body_id`)

No ID rebasing. No global arena merge. Body units stay owned and indexed.

### Diagnostic assembly

All groups' diagnostics concatenated in group order. If any diagnostic is an error, assembly still completes (to collect all errors), then the pipeline reports all diagnostics before proceeding to the next layer.

## 6. Sequential Blockers

| Thing                                  | Why sequential                                                                                                                             |
| -------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| `RegisterModuleDeclarations`           | Builds global symbol identity for all instances. Foundation for all Phase 1 work.                                                          |
| `PrepareModuleLoweringInputs`          | Structural walk per instance. Writes to shared registrar. Produces Phase 1 inputs.                                                         |
| Package lowering                       | No specialization groups. Design-global.                                                                                                   |
| Port binding lowering                  | Cross-instance, design-global.                                                                                                             |
| `CollectDesignDeclarations` (HIR->MIR) | Builds design-global place map, slot descriptors. Foundation for Phase 1 MIR lowering.                                                     |
| LLVM codegen (entire layer)            | LLVM Module/Context are not thread-safe. Parallelizable separately via per-group IR modules + linking if profiling shows it is bottleneck. |

## 7. Prep Shape

Goal: make the sequential pipeline produce body units so the future threading change is mechanical. No threading, no mutexes, no std::async.

Four prep areas:

1. **HIR body unit**: `hir::ModuleBody` becomes the per-body owned artifact with its own arena. Body-local IDs. Group-local symbol environment.
2. **MIR body unit**: Same principle at MIR level. Phase 0 design arena frozen. Each group writes to a body-owned arena.
3. **Per-body diagnostics**: Each body lowering call receives its own diagnostic sink. Phase 2 concatenates in group order.
4. **Type/constant arena ownership**: TypeArena frozen after Phase 0. ConstantArena split into design-global (frozen) and body-local arenas.

### Not part of F1 scope

| Thing                                               | Reason                                                                                                   |
| --------------------------------------------------- | -------------------------------------------------------------------------------------------------------- |
| `ParamTransmissionTable` (m3)                       | Used only in Phase 0. Not a parallelization blocker. Fix when reshaping parameter transmission boundary. |
| `LoweringResult` / `LoweringInput` boundary structs | Work fine as-is. Restructuring them does not enable parallelism.                                         |
| HIR->MIR `Context` struct fields                    | Per-process/function lowering context. Already group-scoped in practice.                                 |
