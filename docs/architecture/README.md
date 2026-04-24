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
8. `hierarchy_and_generate.md` -- hierarchy and generate ownership
9. `identity_and_ownership.md` -- identity rules and forbidden shapes
10. `lowering_boundaries.md` -- what each lowering may and may not do
11. `incremental_build.md` -- query-based incremental compilation and caching
12. `testing_strategy.md` -- test categories and structure

## Template

Each architecture doc follows the fixed template defined in `../style.md`: Purpose, Owns, Does Not
Own, Core Invariants, Boundary to Adjacent Layers, Forbidden Shapes, Notes / Examples. No section
may be omitted.
