# Specialization

> **Queue rules.** Checkboxes at top for progress. Unchecked = gap. Finished items: remove the section, keep one checked line. No code in items (no function names, variable names, snippets). Items describe what the gap is, where to look, and why it matters -- enough to re-investigate from scratch. Investigation logs and design details belong in conversation history, not here.

Working queue for migrating Lyra toward specialization-based compilation.

For the stable architecture: see [compilation-model.md](../compilation-model.md).

## Progress

> **Scope of completed items (A through m2).** These items completed shared-body migration: MIR deduplication, specialization grouping, per-body codegen, and backend API narrowing. They did NOT complete the full realization boundary. Per-instance LLVM wrapper generation was left in place as a bridge between shared bodies and the runtime dispatch ABI. The G-series items below track the remaining migration to eliminate instance-shaped LLVM artifacts.

- [x] A -- Identity (module def/spec IDs, behavior fingerprint, specialization map)
- [x] B1-B4 -- Body ownership (shared MIR bodies, no instance identity in processes)
- [x] C1-C3 -- Local storage (body-local decls, instance placement, alias resolution eliminated)
- [x] D1-D4 -- Realization extraction (bindings, metadata, design main, instance const blocks)
- [x] E1-E2 -- Per-spec codegen (compile session per spec, variant/template-dedup deleted)
- [x] E3 -- Backend API narrowing (no design input in codegen, layout, realization paths)
- [x] E4 -- Delete compatibility adapters (representative-instance, observer program model)
- [x] M2a -- Storage assignment from within-group variance
- [x] M2b partial -- Declaration-based grouping, param-role deleted
- [x] M2c partial -- Compile-owned discriminator (type-store fingerprint)
  - [x] M2c-2a -- Artifact inventory with generate availability paths
  - [x] M2c-2b -- Definition-owned repertoire descriptor
  - [x] M2c-3 -- Specialization fingerprint from definition-scoped type store
- [x] B6 -- HIR ownership split (per-spec-group body, instance record, ownership-shaped inputs)
- [x] m2 -- Instance paths deferred to runtime
- [ ] G -- Instance-independent LLVM codegen (descriptor-driven realization)
  - [x] G0 -- Investigation and documentation of instance-shaped LLVM artifacts
  - [x] G1 -- Migrate process dispatch from per-instance wrappers to descriptor-driven dispatch
  - [ ] G2 -- Migrate comb dispatch off per-instance LLVM wrappers
  - [ ] G3 -- Move unstable-offset realization out of LLVM globals into constructor/runtime-owned data
  - [ ] G4 -- Remove remaining instance-shaped LLVM residue and re-validate scaling
- [ ] F1 -- Parallel specialization compilation
  - [x] F1-design -- Parallel ownership model
  - [x] F1-prep Cut 1 -- Per-body HIR ownership
  - [x] F1-prep Cut 3a -- Per-body AST-to-HIR diagnostics
  - [x] F1-prep Cut 2 -- Per-body MIR ownership
  - [ ] F1-prep Cut 4 -- Type/constant arena freeze
    - [x] F1-prep Cut 4a -- TypeArena investigation and design decision
    - [x] F1-prep Cut 4b -- Phase 0 body-type seeding (closes AST-reachable type gap)
    - [x] F1-prep Cut 4c -- Builtin semantic type catalog (eliminates fixed synthetic Phase 1 writes)
    - [x] F1-prep Cut 4d -- TypeArena Freeze() enforcement (verify parameterized residual, add freeze gate)
    - [ ] F1-prep Cut 4e -- ConstantArena ownership split (design-global + body-local arenas)
  - [ ] m3 -- Param transmission table: replace raw symbol pointers with group-scoped key
  - [ ] F1-impl -- Per-group isolated compilation with deterministic merge
- [ ] F2 -- Specialization caching
- [ ] Documentation gap: pipeline-contract.md and state-layout.md need type ownership clarification
- [ ] CI policy gates: codegen API check, grouping regression tests, topology-independence test

## G: Instance-independent LLVM codegen

Shared-body migration is complete at the body-dedup level. Module-process dispatch is descriptor-driven: the runtime reads per-instance binding from a constant descriptor table and calls shared bodies directly. Per-instance process wrapper functions are eliminated.

Remaining instance-shaped LLVM artifacts: per-instance comb wrappers (adapted to call through a shared trampoline, not process wrappers), per-instance unstable-offset globals, instance-count-shaped data arrays, and per-process named types. These are tracked as G2-G4 follow-up items. Comb dispatch still uses a temporary trampoline bridge and is not yet migrated to the runtime.

See [investigations/instance-shaped-llvm-artifacts.md](../investigations/instance-shaped-llvm-artifacts.md) for the full investigation.

### G0: Investigation and documentation (done)

Completed full boundary investigation of all per-instance LLVM artifacts. Identified eight artifact classes, traced runtime dispatch paths, documented the root cause (runtime ABI lacks descriptor slot), and defined the clean replacement boundary. Architecture docs updated to describe the target shape and current gap explicitly.

### G1: Migrate process dispatch to descriptor-driven dispatch (done)

Per-instance process wrapper functions (`process_N`) are eliminated. Codegen emits a constant descriptor table (`__lyra_process_descriptors`) with per-module-process binding data (shared body pointer, base byte offset, instance ID, base slot ID, unstable offsets pointer). The runtime dispatch callback reads descriptors and calls shared bodies directly with the 7-arg ABI. `GenerateProcessWrapper` is deleted.

A single non-instance-shaped trampoline (`__lyra_descriptor_dispatch`) bridges comb wrappers to the descriptor table. Comb wrappers are adapted to call the trampoline instead of deleted process wrappers. The trampoline is **not** part of the target architecture -- it exists only because comb dispatch remains on the old per-instance LLVM wrapper path until G2. G2 deletes both the trampoline and the comb wrappers.

Runtime ABI bumped to v7 with descriptor table pointer, descriptor count, and standalone/module process boundary. `LyraRunSimulation` signature unchanged. Connection processes keep the existing 3-arg direct dispatch path.

**Bridge residue left for follow-up items**: `__lyra_module_funcs` array with null module entries (temporary compatibility), per-instance comb wrappers (G2), per-instance unstable-offset globals (G3), per-process state types and init code (G4).

### G2: Migrate comb dispatch off per-instance LLVM wrappers

**Goal**: Remove per-instance comb wrapper LLVM functions. Comb dispatch must call shared body functions through runtime-owned descriptor data without per-instance LLVM code.

**Current state after G1**: Comb wrappers (`__lyra_comb_wrapper_N`) still exist as per-instance LLVM functions, but they no longer call per-instance process wrappers. Each comb wrapper calls the shared `__lyra_descriptor_dispatch` trampoline with a descriptor index. The comb wrapper count still scales with instance count.

**What the migration will change**: Comb dispatch will move into the runtime. The `CombKernel` struct will resolve shared body calls through the descriptor table (the canonical source of process binding). The trampoline, `__lyra_comb_funcs`, and all `__lyra_comb_wrapper_N` functions will be deleted.

**Completion means**: No per-instance comb wrapper LLVM functions. Comb kernel dispatch is runtime-owned. The `__lyra_descriptor_dispatch` trampoline is deleted (no longer needed once comb dispatch is in the runtime).

### G3: Move unstable-offset realization out of LLVM globals

**Goal**: Per-instance unstable-offset tables must be runtime-owned memory populated at construction time, not per-instance LLVM globals.

**Why current state is insufficient**: For parameterized specializations with varying slot sizes, the LLVM backend generates one constant global array per instance containing byte offsets for unstable slots. This data is correct, but its representation as per-instance LLVM globals means LLVM global count scales with instance count.

**What the migration will change**: Unstable-offset data will be computed during design realization and stored in runtime-owned memory (part of the process descriptor or a side table referenced by the descriptor). The LLVM backend will stop emitting per-instance offset globals.

**Completion means**: No per-instance LLVM globals exist. Unstable-offset data is runtime-owned, populated at construction time.

### G4: Remove remaining instance-shaped LLVM residue and re-validate scaling

**Goal**: Clean up any remaining instance-shaped IR artifacts (per-instance named struct types, instance-count-shaped arrays in main, per-process state initialization code) and verify that LLVM IR size and optimization time are instance-count-independent.

**Why current state is insufficient**: Even after G1-G3, secondary artifacts may remain: per-scheduled-process LLVM named types, instance-count-shaped state offset arrays, and per-process initialization code in main. These are smaller in impact but still violate the boundary.

**What the migration will change**: LLVM type generation will use one type per body, not per instance. State allocation and initialization will move to the runtime constructor. The main function will delegate to a runtime entry point that receives metadata, not instance-shaped code.

**Completion means**: Re-running the module-count and generate-expand compile benchmarks shows LLVM instruction count and compile time are flat across instance count changes. Only realization/construction time scales with instances.

## F1: Parallel specialization compilation

See [parallel-compilation.md](../parallel-compilation.md) for the full design.

Core model: Phase 0 (sequential global setup) produces immutable shared reference data. Phase 1 (per-group isolated compilation) produces per-body owned units. Phase 2 (deterministic assembly) collects bodies and builds design-wide artifacts. Body-local IDs stay body-local permanently.

Next step: ConstantArena ownership split (Cut 4e). After prep cuts complete, F1-impl builds the parallel compilation pipeline.

Note: G-series (instance-independent codegen) is a prerequisite for F1-impl. When compilation is truly per-specialization and parallel, there is no opportunity to generate per-instance wrappers during compilation because the instance graph is not available to individual compilation units. G-series must be resolved before or concurrently with F1-impl.

## F2: Specialization caching

Not yet designed. Depends on F1 completing the parallel compilation model.

## Documentation gap: type ownership model

Three docs still blur compile-owned vs constructor-owned type boundaries: pipeline-contract.md ("types are language-level" without ownership distinction), state-layout.md (three phases described without framing as ownership boundaries). The code already implements the correct projection (M2c). The docs need to catch up.

## CI policy gates

Several specialization invariants lack CI enforcement:

- Codegen API has no design input (API signature check)
- Within-group param variance is transmitted per-instance (regression test)
- Compile-owned differences produce distinct specializations (regression test)
- Specialization IR is topology-independent (regression test)
- No instance paths in specialization artifacts (policy check)
- Specialization grouping is deterministic (regression test)
- LLVM artifact count is instance-independent (scaling regression test)

## Open Questions

1. Process body identity is not yet part of the specialization fingerprint. Processes with different code but the same type universe currently share a specialization under the constructor model.
2. Package compilation: packages have no instances. Separate specialization unit or separate concept?
3. Container descriptor format for specialization layout.
