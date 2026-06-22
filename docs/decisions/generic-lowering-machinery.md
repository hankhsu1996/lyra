# Generic lowering machinery, typed IR nodes

Date: 2026-06-22 Status: accepted

## Context

Lowering is organized as a construction pass per unit of work: a procedural body (a process or
subroutine) and a structural scope (a module or class body) are separate passes, because one lowers
to a callable and the other to a class -- object versus callable, a real distinction that survives
into MIR. Expressions, statements, members, and generate constructs all appear inside those scopes.

Two kinds of duplication have accumulated around that split:

1. **The arena pattern is hand-rolled everywhere.** A flat, id-indexed, append-only pool -- a
   `vector<T>` plus `Get(Id)` that indexes it and `Add(T)` that pushes and returns the id -- is
   written out by hand for every pool: the procedural body's expressions, statements, and locals;
   the structural scope's roughly ten pools (members, generates, processes, continuous assigns,
   ...); the MIR block's expressions, statements, and locals. The same three-line shape appears
   twenty-odd times across HIR and MIR.

2. **Every shared handler is duplicated along the procedural/structural axis.** Each expression
   family carries two near-identical handlers -- one taking the procedural pass class, one taking
   the structural pass class -- whose bodies differ only in the pass type and in how a
   sub-expression is reached (read accessor at HIR-to-MIR, write target at AST-to-HIR). The
   expression-lowering logic itself is identical, because an expression's meaning does not depend on
   whether a process or a structural scope encloses it. The twins drift: a guard or a value
   realization added on one path is missed on the other.

The design question is how generic to make this. Two failure modes bracket the answer: leaving the
duplication, and collapsing every IR node into one generic node so that containment and traversal
are uniform but nothing is typed.

## Decision

Make the machinery generic; keep the nodes typed.

1. **A universal `Arena<T>`.** One generic flat, id-indexed, append-only pool replaces every
   hand-rolled `vector<T>` + `Get` + `Add`. It is layer-agnostic and node-agnostic: it carries no
   knowledge of what it stores, only that the pool is indexed by a typed id. A pool that
   deduplicates on insert (type interning) is not this shape -- it is an interning table (an arena
   plus a key-to-id cache) and keeps its own type.

2. **Context-free expression lowering, shared across pass classes.** Because an expression's meaning
   is independent of the enclosing scope, the handler that lowers an expression family is written
   once as a function template parameterized over the pass class, reaching its sub-expressions
   through one uniform accessor. The two pass classes are not merged: they build genuinely different
   things (a callable body versus a class) and accumulate different state. The template shares the
   code without erasing the distinction; it is duck-typed, with no shared base class.

3. **Typed nodes; the distinction lives at the scope level.** HIR nodes stay SV-faithful and typed
   -- loops are loops, generate is generate -- and MIR nodes are the generic programming-language
   set. The procedural/structural distinction belongs where the two genuinely differ: a procedural
   body holds statements and locals, a structural scope holds members, generates, and child scopes.
   It never belongs at the expression level, where the two are identical. The generic `Arena<T>` and
   the shared handler templates are how that uniformity is expressed without flattening the node
   types.

## Rejected

- **Status quo: per-pass-class handler twins and per-pool hand-rolled arenas.** Every new expression
  kind doubles the handler count; every pool re-rolls the same boilerplate; the twin handlers drift
  silently. This is the debt being paid down.
- **Merge the two pass classes (or the two scope types) into one.** A procedural body lowers to a
  callable and a structural scope to a class -- object versus callable, a distinction MIR carries as
  a first-class fact. Merging them blurs it and forces a scope type that is a union of
  statements-or-members, generate-or-control-flow, pushing a `which-kind` branch into every consumer
  that the type system would otherwise carry.
- **A fully generic uniform node (one node kind with generic nested regions).** Maximally DRY, but
  it discards type safety and the SV-faithfulness HIR exists to provide: "loops are loops, generate
  is generate" becomes "everything is a node." HIR's typed SV constructs and MIR's typed entity set
  are deliberate; a uniform node model reverses both. Genericity belongs in the machinery that
  carries and traverses nodes, not in the node identities themselves. This is the line between the
  chosen design and the rejected one: generic container, typed contents.
- **A one-off shared arena for expressions only.** The arena pattern is pervasive, not
  expression-specific; a bespoke expression arena repeats the same hand-rolling at smaller scale.
  The right generic is `Arena<T>`, applied to every pool.

## Consequences

- `Arena<T>` replaces every append-only id-indexed pool across HIR and MIR. Interning tables keep
  their dedup shape.
- With both scopes' expression pools typed as the same `Arena<Expr>`, the procedural and structural
  expression handlers collapse to one template per family, reached through a uniform accessor.
  Statement, generate, and member handlers stay distinct, because those constructs genuinely differ.
- The pass classes stay distinct; the node types stay typed. Generate keeps its SV-faithful form in
  HIR and is lowered at HIR-to-MIR into ordinary `if` and loop statements in a constructor callable,
  so MIR has no generate node -- the SV-distinct HIR constructs converge onto the generic MIR set
  exactly where the layer identities require.
- `lowering_organization.md` gains a clause: a handler shared across pass classes is one function
  template, never a duplicated twin.

## Cross-references

- `architecture/north_star.md` (compile per unit; class-level artifacts versus callables).
- `architecture/hir.md` (SV-faithful, typed constructs).
- `architecture/mir.md` (object versus callable; an expression node whose meaning depends on the
  enclosing callable is forbidden).
- `architecture/lowering_organization.md` (the pass / dispatcher / handler / walk-frame shape this
  refines).
- `progress/generic-lowering.md` (the staged migration).
- `progress/refactor.md` R34 (the AST-to-HIR drift-bug slice, in flight separately).
