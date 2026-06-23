# Generic Lowering

Makes the lowering machinery generic while keeping the IR node types typed. Done when every
append-only id-indexed pool in HIR and MIR is one generic arena, and the expression-lowering
handlers shared across the procedural and structural pass classes are single function templates
rather than duplicated twins. The procedural/structural distinction is preserved where the two
genuinely differ (statements versus members and generates) and removed from the expression layer,
where they are identical. See `decisions/generic-lowering-machinery.md` for the rationale and the
rejected alternatives.

## Actionable

Phase 1 (the generic arena) is standalone and can start immediately. The arena swap is mechanical
and behavior-preserving; the bulk of the work is paying down the existing accessor-bypass debt that
the swap surfaces (see below). Phases 2 and 3 build on it.

The arena's surface is fixed by what consumers actually do: a const lookup by typed id, an append
that returns the id, a count and an emptiness check, and an indexed iteration (the dumper prints
each node with its id). It exposes no raw integer index and no mutable lookup -- nothing mutates a
node in place. Three shapes are deliberately not this arena: a deduplicating pool (type interning, a
lookup-or-insert with a key cache); a field that holds a sequence of ids rather than owned nodes (a
body's top-level statement order, an entry-statement id); and the composite append helpers that
build on a pool (declare-a-local, append-an-if), which keep their logic and delegate to the pool
underneath.

## Sub-Steps

### Phase 1 -- Generic arena and uniform access

- [ ] Introduce one generic append-only, id-indexed pool abstraction with the surface above (const
      lookup, append, count, indexed iteration; no raw index, no mutable lookup).
- [ ] Route every hand-rolled HIR pool through it: the procedural body's expression, statement, and
      local pools; every append-only pool on the structural scope.
- [ ] Route every hand-rolled MIR pool through it: the block's expression, statement, and local
      pools, the class's pools, and the unit's append-only pools.
- [ ] Replace the raw-index accessor bypass (sub-nodes read by indexing the pool's backing vector
      directly rather than through the typed lookup) with the typed lookup. This is pre-existing
      debt, not new work; fixing it also collapses the procedural/structural read-access divergence,
      so the read side of sub-node access is uniform once this lands.
- [ ] AST-to-HIR: the walk position carries one expression write target instead of two parallel
      ones, with the statement and member write targets kept separate. With both scopes' expression
      pools now the same arena type, the read and write sides of sub-node access are both uniform.
- [ ] Leave deduplicating pools (type interning), id-sequence fields, and composite append helpers
      in their own shapes (see Actionable).

### Phase 2 -- Share the expression handlers

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
