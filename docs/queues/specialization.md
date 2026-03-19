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
  - [ ] G1 -- Migrate process dispatch from per-instance wrappers to descriptor-driven dispatch
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

Shared-body migration is complete only at the body-dedup level: MIR produces one body per specialization, and shared body functions with the 7-arg ABI are compiled once per body. This portion is in the correct long-term shape.

Descriptor-driven instance realization is not yet implemented. Per-instance wrappers and globals remain a bridge shape, not the target architecture. The LLVM backend still generates per-instance wrapper functions, per-instance comb wrappers, per-instance unstable-offset globals, and instance-count-shaped function pointer arrays. These artifacts encode instance-specific binding in LLVM IR, causing LLVM IR size and optimization time to scale linearly with instance count.

This violates the specialization boundary rule: instance count should primarily affect runtime construction work and runtime metadata/object-graph size, not heavy LLVM codegen shape.

See [investigations/instance-shaped-llvm-artifacts.md](../investigations/instance-shaped-llvm-artifacts.md) for the full investigation.

### G0: Investigation and documentation (done)

Completed full boundary investigation of all per-instance LLVM artifacts. Identified eight artifact classes, traced runtime dispatch paths, documented the root cause (runtime ABI lacks descriptor slot), and defined the clean replacement boundary. Architecture docs updated to describe the target shape and current gap explicitly.

### G1: Migrate process dispatch to descriptor-driven dispatch

**Goal**: Process dispatch must consume instance descriptors instead of per-instance wrapper function pointers. Shared body functions must be callable without per-instance wrapper generation.

**Why current state is insufficient**: The runtime dispatch ABI is a bare function pointer call with no descriptor argument. All instance-specific binding (base byte offset, instance ID, signal ID offset, unstable offsets) must be baked into per-instance LLVM wrapper functions. This generates O(instances) LLVM functions, each requiring LLVM function creation, verification, and machine code emission.

**What the migration will change**: The runtime dispatch path will read a per-instance descriptor (containing shared body pointer, base byte offset, instance ID, signal ID offset, unstable offset pointer) and call the shared body function directly. The LLVM backend will stop generating per-instance wrapper functions. The per-instance function pointer array will be replaced by a descriptor table built at construction time.

**Completion means**: Adding module instances does not increase the number of LLVM functions. The per-instance wrapper generation code path is deleted, not just bypassed.

### G2: Migrate comb dispatch off per-instance LLVM wrappers

**Goal**: Remove the per-instance comb wrapper -> process wrapper -> shared body bridge chain. Comb dispatch must call shared body functions directly or through descriptors.

**Why current state is insufficient**: Each comb kernel generates a per-instance LLVM wrapper that allocates a local outcome buffer, calls the underlying process wrapper, and discards the result. This is a second level of per-instance LLVM code on top of G1.

**What the migration will change**: Comb dispatch will use descriptors (same as G1) or the comb ABI will be unified with the process ABI so that no adapter wrapper is needed. The runtime will allocate the outcome buffer and call the shared body directly.

**Completion means**: The comb wrapper generation code path is deleted. Comb kernel count does not increase LLVM function count.

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
