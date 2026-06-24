# Generic Lowering

Makes the lowering machinery generic while keeping the IR node types typed. Done when every
append-only id-indexed pool in HIR and MIR is one generic arena, and the expression-lowering
handlers shared across the procedural and structural pass classes are single function templates
rather than duplicated twins. The procedural/structural distinction is preserved where the two
genuinely differ (statements versus members and generates) and removed from the expression layer,
where they are identical. See `decisions/generic-lowering-machinery.md` for the rationale and the
rejected alternatives.

## Actionable

The generic arena (Phase 1) and the handler-sharing it enables (Phase 2) have landed in full on both
lowering boundaries. Every context-free expression family -- including the call, `inside`, and
replication families, the last holdouts -- is now a single function template over the pass class, so
a continuous-assign right-hand side lowers the full value-expression set through the same handlers
as a process body. The procedural/structural distinction survives only where the two genuinely
differ (statements versus members and generates, name resolution, and the kinds the LRM allows only
in procedural code) and is gone from the expression layer, where they are identical. See
`../decisions/context-free-call-lowering.md`. The dispatch sharing (Phase 3) is complete at
AST-to-HIR (one dispatcher template) and has one step left at HIR-to-MIR -- collapsing its two
dispatchers, which sit in separate translation units, into one. The context-free handlers are
already shared there, so the only remaining drift surface is the duplicated dispatch switch.

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

- [x] Introduce one generic append-only, id-indexed pool abstraction with the surface above (const
      lookup, append, count, indexed iteration; no raw index, no mutable lookup).
- [x] Route every hand-rolled HIR pool through it: the procedural body's expression, statement, and
      local pools; every append-only pool on the structural scope.
- [x] Route every hand-rolled MIR pool through it: the block's expression, statement, and local
      pools, the class's pools, and the unit's append-only pools.
- [x] Replace the raw-index accessor bypass (sub-nodes read by indexing the pool's backing vector
      directly rather than through the typed lookup) with the typed lookup. This is pre-existing
      debt, not new work; fixing it also collapses the procedural/structural read-access divergence,
      so the read side of sub-node access is uniform once this lands.
- [x] AST-to-HIR: the walk position carries one expression write target instead of two parallel
      ones, with the statement and member write targets kept separate. With both scopes' expression
      pools now the same arena type, the read and write sides of sub-node access are both uniform.
- [x] Leave deduplicating pools (type interning), id-sequence fields, and composite append helpers
      in their own shapes (see Actionable).

### Phase 2 -- Share the expression handlers

- [x] HIR-to-MIR: each expression family's procedural and structural handlers become one function
      template over the pass class. Pilot with the select family (the largest duplication), then the
      remaining families.
- [x] AST-to-HIR: the same collapse. Each context-free expression family's procedural and structural
      handlers become one function template over the pass class, reached through the uniform
      recursion entry point; name resolution and single-context kinds stay per-pass.
- [x] `lowering_organization.md` records that a handler shared across pass classes is one template,
      never a duplicated twin.

### Phase 3 -- Share the dispatch

- [x] The call, `inside`, and replication families -- context-free but left procedural-only --
      become templates over the pass class, and both pass-class dispatchers route their case through
      the shared handler, so a continuous-assign right-hand side (a simulation-time expression the
      structural scope owns) lowers function calls, `$isunknown`, `inside`, and replication through
      the same handlers as a process body. The `with`-clause element and index become co-equal
      closure parameters so the call family carries no procedural-body dependency. See
      `../decisions/context-free-call-lowering.md`.
- [x] AST-to-HIR collapses its two pass-class dispatchers (which share a translation unit) into one
      dispatcher template the two entries delegate to, so even the dispatch switch is written once;
      name resolution and the procedural-only kinds are the only parameterized arms.
- [ ] HIR-to-MIR: collapse its two dispatchers likewise. They live in separate translation units
      today, so this is a translation-unit move, not just a template; the context-free handlers are
      already shared, so the remaining drift surface is only the duplicated dispatch switch.

## Coordination

- The AST-to-HIR slice and `refactor.md` R34 (the procedural/structural expression-lowering drift
  bugs) were the same work and landed together: collapsing each twin to one template per family
  removes the drift surface by construction, so there is no separate guard-alignment step to
  sequence before it. The HIR-to-MIR slice landed independently earlier.

## Out of Scope

- Merging the pass classes or the scope types. A procedural body lowers to a callable and a
  structural scope to a class; that distinction is real and stays.
- A fully generic uniform IR node. The node types are deliberately typed and SV-faithful; only the
  machinery that carries and traverses them is made generic.
