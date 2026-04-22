# HIR

## Purpose

Define the source-near semantic IR. HIR preserves SystemVerilog language constructs with explicit
semantic ownership and without loss of LRM-level information.

## Owns

- Language constructs as first-class nodes: declarations, expressions, statements, processes,
  assertions, functions, tasks, parameters, ports.
- Compilation-unit-local declarations with typed identities, one id kind per kind of declaration
  (variables, nets, parameters, instance members).
- The generate-region tree as a first-class structure inside each module compilation unit.
- The canonical association between a declaration and its references within the same compilation
  unit.
- Source locations for every construct that may produce a diagnostic.

## Does Not Own

- Control-flow graphs, basic blocks, or phi nodes.
- Execution scheduling, dirty tracking, or subscription wiring.
- Low-level storage layout or ABI decisions.
- Hierarchical paths beyond the compilation unit's own generate tree (see
  `hierarchy_and_generate.md`).

## Core Invariants

1. Every compilation-unit-local declaration has a typed id. References to that declaration carry the
   typed id, never a frontend symbol id.
2. A reference that targets a declaration outside the current compilation unit uses a separate,
   explicitly named variant. Local and cross-unit references never share a key space.
3. HIR preserves LRM-level constructs without flattening them into lower-level primitives. Loops are
   loops, assertions are assertions, classes are classes.
4. Each compilation unit is self-contained: resolving any intra-unit reference requires no data
   outside that unit.
5. The generate tree lives inside the module compilation unit and is the sole representation of
   generate-region structure at HIR level.
6. HIR is the canonical semantic identity of the compiler. No frontend id survives past AST-to-HIR.
   Downstream layers consume HIR identity, never frontend identity.

## Boundary to Adjacent Layers

- Consumes slang's AST. AST-to-HIR is a cutover, not a translation: frontend ids are discarded at
  this boundary. HIR establishes the first canonical semantic identity owned by the compiler.
- Produces input to MIR. HIR-to-MIR lowering reads typed HIR ids and produces MIR objects and
  members (see `mir.md`).

## Forbidden Shapes

- `SymbolId` or any frontend-global id used as the identity of a compilation-unit-local declaration
  reference.
- A resolver keyed on `(instance_id, local_symbol)` at HIR level.
- CFG-shaped nodes (basic blocks, successor lists, phi nodes) anywhere in HIR.
- A generate-region representation that lives outside the compilation unit it belongs to.
- Coordinate or ordinal identity for a generate region.
- Side-table reconstruction of declaration ownership from names or paths.
- Dependence on a global lookup table to resolve a local reference.
- Carrying frontend ids into HIR or any downstream layer. Frontend ids end at AST-to-HIR.

## Notes / Examples

A reference to a local variable carries a `VariableId` in the compilation unit's arena. A reference
to a package-level entity or a cross-unit target uses a distinct variant. The two are not
interchangeable and never share a key space.
