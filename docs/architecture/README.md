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
4. `specialization_model.md` -- how parameter values refine a unit into specializations
5. `hir.md` -- source-near semantic IR
6. `mir.md` -- object-oriented semantic IR (objects, members, callables)
7. `lir.md` -- execution-oriented IR (CFG, basic blocks, storage)
8. `scheduling.md` -- stratified event scheduler, regions, suspension protocol
9. `hierarchy_and_generate.md` -- hierarchy and generate ownership
10. `identity_and_ownership.md` -- identity rules and forbidden shapes
11. `lowering_boundaries.md` -- what each lowering may and may not do
12. `incremental_build.md` -- query-based incremental compilation and caching
13. `testing_strategy.md` -- test categories and structure

## Document Shape

Type-contract docs (the IR layers, hierarchy, identity, ownership, lowering boundaries) follow the
template defined in `../style.md`. Behavioral and decision-cluster docs (`scheduling.md`) find their
own structure; the contract discipline still applies. See `../style.md` for which subjects use which
shape.
