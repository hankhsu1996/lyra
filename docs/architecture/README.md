# Architecture

Binding contracts for each layer and cross-cutting concept of the compiler.

Every doc here is a **contract**. It describes the intended permanent shape of the subject. If the
current code violates a contract, the code is wrong, not the contract.

`north_star.md` sits above every other document in this directory. If a lower-level document
conflicts with a North Star principle, the lower-level document is wrong and must be fixed.

## Reading Order

Read top to bottom on first pass:

1. `north_star.md` -- top-level objective and first-class constraints; constrains every other doc
2. `compiler_overview.md` -- pipeline, worldview, compile-time vs runtime
3. `compilation_unit_model.md` -- what a compilation unit is and owns
4. `runtime_model.md` -- the constructor / simulation execution-context split
5. `specialization_model.md` -- how parameter values refine a unit into specializations
6. `hir.md` -- source-near semantic IR
7. `mir.md` -- object-oriented semantic IR (objects, members, callables)
8. `lir.md` -- execution-oriented IR (CFG, basic blocks, storage)
9. `scheduling.md` -- stratified event scheduler, regions, suspension protocol
10. `hierarchy_and_generate.md` -- hierarchy and generate ownership
11. `identity_and_ownership.md` -- identity rules and forbidden shapes
12. `lowering_boundaries.md` -- what each lowering may and may not do
13. `incremental_build.md` -- query-based incremental compilation and caching
14. `testing_strategy.md` -- test categories and structure

## Concept Index

If you are looking for a concept, this table points to the canonical doc.

| Concept                                                                  | Canonical doc               |
| ------------------------------------------------------------------------ | --------------------------- |
| Primary optimization target; non-negotiable design constraints           | `north_star.md`             |
| Pipeline (HIR -> MIR -> LIR -> LLVM IR); compile-time vs runtime split   | `compiler_overview.md`      |
| Compilation unit; class-level artifacts; instance records                | `compilation_unit_model.md` |
| Constructor vs simulation execution contexts; structural vs process      | `runtime_model.md`          |
| Generate as constructor-time logic; object graph shape                   | `hierarchy_and_generate.md` |
| Parameter values, specialization keys, per-specialization artifacts      | `specialization_model.md`   |
| Identity rules; ownership; forbidden identity shapes                     | `identity_and_ownership.md` |
| Lowering permissions (what each lowering may and may not do)             | `lowering_boundaries.md`    |
| HIR shape (statements, expressions, primaries)                           | `hir.md`                    |
| MIR shape (objects, members, callables, closures)                        | `mir.md`                    |
| LIR shape (CFG, basic blocks, storage)                                   | `lir.md`                    |
| Stratified scheduler; regions; suspension protocol; NBA / closure submit | `scheduling.md`             |
| Incremental compilation; query-based caching                             | `incremental_build.md`      |
| Test categories; suite layout; expectation forms                         | `testing_strategy.md`       |

## Document Shape

Type-contract docs (the IR layers, hierarchy, identity, ownership, lowering boundaries) follow the
template defined in `../style.md`. Behavioral and decision-cluster docs (`scheduling.md`) find their
own structure; the contract discipline still applies. See `../style.md` for which subjects use which
shape.
