# Generic Lowering

Makes the lowering machinery generic while keeping the IR node types typed. Done when every
append-only id-indexed pool in HIR and MIR is one generic arena, and the expression-lowering
handlers shared across the procedural and structural pass classes are single function templates
rather than duplicated twins. The procedural/structural distinction is preserved where the two
genuinely differ (statements versus members and generates) and removed from the expression layer,
where they are identical. See `decisions/generic-lowering-machinery.md` for the rationale and the
rejected alternatives.

## Actionable

Phase 1 (the generic arena) is standalone and can start immediately -- it is a mechanical,
behavior-preserving container swap that touches no handler logic. Phases 2 and 3 build on it.

## Sub-Steps

### Phase 1 -- Generic arena

- [ ] Introduce one generic append-only, id-indexed pool abstraction (a value pool with a typed-id
      lookup and an append that returns the id).
- [ ] Route every hand-rolled HIR pool through it: the procedural body's expression, statement, and
      local pools; every append-only pool on the structural scope.
- [ ] Route every hand-rolled MIR pool through it: the block's expression, statement, and local
      pools, and the unit's append-only pools.
- [ ] Leave deduplicating pools (type interning) on their interning-table shape, not the plain
      arena.

### Phase 2 -- Uniform sub-node access

- [ ] HIR-to-MIR: both pass classes reach a sub-expression through one uniform accessor; the
      divergent read styles are gone.
- [ ] AST-to-HIR: the walk position carries one expression write target instead of two parallel
      ones, with the statement and member write targets kept separate.

### Phase 3 -- Share the expression handlers

- [ ] HIR-to-MIR: each expression family's procedural and structural handlers become one function
      template over the pass class. Pilot with the select family (the largest duplication), then the
      remaining families.
- [ ] AST-to-HIR: the same collapse, sequenced after the in-flight drift-bug alignment lands (see
      Coordination).
- [ ] `lowering_organization.md` records that a handler shared across pass classes is one template,
      never a duplicated twin.

## Coordination

- The AST-to-HIR slice overlaps `refactor.md` R34 (the procedural/structural HIR expression-lowering
  drift bugs, in flight separately). Sequence the AST-to-HIR templating after R34's operand-guard
  and value-realization alignment lands, so the two efforts do not edit the same handlers at once.
  The HIR-to-MIR slice is independent and unblocked.

## Out of Scope

- Merging the pass classes or the scope types. A procedural body lowers to a callable and a
  structural scope to a class; that distinction is real and stays.
- A fully generic uniform IR node. The node types are deliberately typed and SV-faithful; only the
  machinery that carries and traverses them is made generic.
