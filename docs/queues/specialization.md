# Specialization

> **Queue rules.** Checkboxes at top for progress. Unchecked = gap. Finished items: remove the section, keep one checked line. Every unchecked item must have a short tag/ID for tracking (e.g. G3, F1). Each item should be independently addressable -- it can be investigated and landed as a standalone change. No code in items (no function names, variable names, snippets). Items describe what the gap is, where to look, and why it matters -- enough to re-investigate from scratch. Investigation logs and design details belong in conversation history, not here.

Working queue for migrating Lyra toward specialization-based compilation.

For the stable architecture: see [compilation-model.md](../compilation-model.md).

## Progress

- [x] A-E -- Shared-body migration (MIR dedup, specialization grouping, per-body codegen, backend API narrowing)
- [x] M2 -- Storage assignment and compile-owned discriminator
- [x] B6 -- HIR ownership split
- [x] m2 -- Instance paths deferred to runtime
- [x] G -- Instance-independent LLVM codegen (per-instance code eliminated, shared-body code paths in place)
- [x] H1 -- Correct specialization end-state: architecture contract still overstates progress
- [x] H2 -- Move constructor/process realization out of compile-time artifacts
- [x] H3 -- Move process metadata realization behind constructor-time expansion
- [x] H4 -- Move trigger/comb realization behind constructor-time expansion
- [x] H5 -- Move slot/trace/path realization behind constructor-time expansion
- [ ] H6 -- Remove compile-time-expanded design-state initialization topology
- [ ] H7 -- Remove remaining topology-sized emitted storage realization
- [ ] H8 -- Re-validate topology-independence with scaling gates
- [ ] F1 -- Parallel specialization compilation
  - [x] F1-design -- Parallel ownership model
  - [x] F1-prep Cuts 1-4 -- Per-body HIR/MIR ownership, type/constant arena freeze
  - [ ] m3 -- Param transmission table: replace raw symbol pointers with group-scoped key
  - [ ] F1-impl -- Per-group isolated compilation with deterministic merge
- [ ] F2 -- Specialization caching
- [ ] Documentation gap: pipeline-contract.md and state-layout.md need type ownership clarification
- [ ] CI policy gates: codegen API check, grouping regression tests, topology-independence test

## H6: Remove compile-time-expanded design-state initialization topology

4-state patch tables (byte offsets and masks for X-encoding initialization) are emitted as fully expanded compile-time globals that scale with the total realized slot count. In a design with N instances of the same body, the patch table contains N copies of the same offset pattern, shifted by each instance's base offset.

This should instead use body-shaped patch schemas (offsets relative to the body's slot base) that the runtime applies per-instance using each instance's base offset at construction time.

## H7: Remove remaining topology-sized emitted storage realization

The design-state arena sizing and any remaining emitted storage decisions still depend on the fully realized topology. The arena size in main is a compile-time constant derived from the total slot count across all instances. While this is a single constant (not a per-instance loop), it represents a design decision that could instead be computed by the runtime constructor from body-shaped slot counts and instance counts.

This item covers any remaining emitted storage objects or sizing decisions that still encode the fully realized topology as if constructor expansion had already happened at compile time.

## H8: Re-validate topology-independence with scaling gates

After the H2-H7 migrations, compile time, optimize time, and object emission time should no longer materially scale with instance count. Only body-shaped and schema-shaped artifacts should appear in the emitted object. Instance-count-dependent work should happen exclusively at runtime construction time.

Validation: re-run the generate-expand compile benchmark at 128 / 1024 / 4096 instances and verify that all three compile phases (lower_llvm, optimize_ir, emit_obj) are flat or near-flat across instance count changes.

## F1: Parallel specialization compilation

See [parallel-compilation.md](../parallel-compilation.md) for the full design. All prep cuts are complete. Next: m3 (param transmission table) then F1-impl (per-group isolated compilation with deterministic merge).

## F2: Specialization caching

Not yet designed. Depends on F1 completing the parallel compilation model.

## Documentation gap: type ownership model

Three docs still blur compile-owned vs constructor-owned type boundaries: pipeline-contract.md ("types are language-level" without ownership distinction), state-layout.md (three phases described without framing as ownership boundaries). The code already implements the correct projection for types. The docs need to catch up, and the constructor-time realization gap (H-series) makes this more urgent.

## CI policy gates

Several specialization invariants lack CI enforcement:

- Within-group param variance is transmitted per-instance (regression test)
- Compile-owned differences produce distinct specializations (regression test)
- Specialization IR is topology-independent (regression test)
- No instance paths in specialization artifacts (policy check)
- Specialization grouping is deterministic (regression test)
- LLVM artifact count is instance-independent (scaling regression test)
