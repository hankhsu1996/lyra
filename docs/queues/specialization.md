# Specialization

> **Queue rules.** Checkboxes at top for progress. Unchecked = gap. Finished items: remove the section, keep one checked line. Every unchecked item must have a short tag/ID for tracking (e.g. G3, F1). Each item should be independently addressable -- it can be investigated and landed as a standalone change. No code in items (no function names, variable names, snippets). Items describe what the gap is, where to look, and why it matters -- enough to re-investigate from scratch. Investigation logs and design details belong in conversation history, not here.

Working queue for migrating Lyra toward specialization-based compilation.

For the stable architecture: see [compilation-model.md](../compilation-model.md).

## Progress

> **Scope of completed items (A through G).** These items completed shared-body migration (A-E, M2, B6, m2) and instance-independent LLVM codegen (G-series). All per-instance LLVM artifacts are eliminated. Codegen emits body-shaped metadata; runtime owns instance-shaped construction. The remaining work is parallel compilation (F1) and caching (F2).

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
- [x] G -- Instance-independent LLVM codegen (descriptor-driven realization)
  - [x] G0 -- Investigation and documentation of instance-shaped LLVM artifacts
  - [x] G1 -- Migrate process dispatch from per-instance wrappers to descriptor-driven dispatch
  - [x] G2 -- Migrate comb dispatch off per-instance LLVM wrappers
    - [x] G2 follow-up: canonical header-access layer
    - [x] G2 follow-up: runtime-owned simulation-header binding initialization
  - [x] G3 -- Move unstable-offset realization out of LLVM globals into constructor/runtime-owned data
  - [x] G4 -- Remove remaining instance-shaped LLVM residue and re-validate scaling
- [ ] F1 -- Parallel specialization compilation
  - [x] F1-design -- Parallel ownership model
  - [x] F1-prep Cut 1 -- Per-body HIR ownership
  - [x] F1-prep Cut 3a -- Per-body AST-to-HIR diagnostics
  - [x] F1-prep Cut 2 -- Per-body MIR ownership
  - [x] F1-prep Cut 4 -- Type/constant arena freeze
    - [x] F1-prep Cut 4a -- TypeArena investigation and design decision
    - [x] F1-prep Cut 4b -- Phase 0 body-type seeding (closes AST-reachable type gap)
    - [x] F1-prep Cut 4c -- Builtin semantic type catalog (eliminates fixed synthetic Phase 1 writes)
    - [x] F1-prep Cut 4d -- TypeArena Freeze() enforcement (verify parameterized residual, add freeze gate)
    - [x] F1-prep Cut 4e -- ConstantArena ownership split (design-global + body-local arenas)
  - [ ] m3 -- Param transmission table: replace raw symbol pointers with group-scoped key
  - [ ] F1-impl -- Per-group isolated compilation with deterministic merge
- [ ] F2 -- Specialization caching
- [ ] Documentation gap: pipeline-contract.md and state-layout.md need type ownership clarification
- [ ] CI policy gates: codegen API check, grouping regression tests, topology-independence test

## G: Instance-independent LLVM codegen (done)

All instance-shaped LLVM construction residue is eliminated. Codegen emits body-shaped metadata and schema-keyed types/functions. Runtime owns all instance-shaped work through the process-state constructor boundary. The shared body ABI is 2-arg `(frame, resume)`. Dispatch uses frame-header-cached binding populated from descriptors at init. ABI v11 with connection-process terminology throughout.

### G4: Remove remaining instance-shaped LLVM residue and re-validate scaling (done)

Canonical process-state constructor boundary established. Codegen emits body-shaped metadata (ProcessStateSchema[], ProcessConstructorRecord[]) and per-schema frame-init functions. Runtime constructor (LyraConstructProcessStates) is the sole executor of instance-shaped allocation and initialization. Generated main contains no per-process construction loops, no offset arithmetic, no null-padded function arrays. LLVM type naming uses schema identity (Body{id}Frame/State, Init{i}, Conn{i}). ABI v11 replaces standalone terminology with connection throughout. Scaling validated: body-shaped artifacts constant across instance count; only constructor records and descriptors grow.

## F1: Parallel specialization compilation

See [parallel-compilation.md](../parallel-compilation.md) for the full design.

Core model: Phase 0 (sequential global setup) produces immutable shared reference data. Phase 1 (per-group isolated compilation) produces per-body owned units. Phase 2 (deterministic assembly) collects bodies and builds design-wide artifacts. Body-local IDs stay body-local permanently.

All prep cuts (4a-4e) are complete. ConstantArena is split into design-global + body-local. TypeArena has Freeze() enforcement. G-series (instance-independent codegen) is complete. Next: m3 (param transmission table) and then F1-impl.

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
