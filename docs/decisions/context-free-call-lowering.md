# Context-free call lowering

Date: 2026-06-23 Status: accepted

## Context

`generic-lowering-machinery.md` established that an expression's meaning does not depend on whether
a process body or a structural scope encloses it, so each context-free expression family is one
function template over the pass class (constrained on the `ExprLowerer` duck-typed surface),
instantiated for both pass classes. The operator, select, member-access, concatenation, conversion,
and assignment-pattern families all collapsed to single templates on this rule.

Two things were left unfinished, and they reinforced each other:

1. **The dispatcher was still per-context.** Each lowering boundary kept two dispatchers -- one over
   the procedural pass class, one over the structural pass class -- and each listed its kinds
   independently. Context-free kinds had to be wired into both by hand, so a kind added to one and
   forgotten in the other was a silent gap. The call, `inside`, and replication families were
   exactly that: present procedurally, absent structurally, falling through to an "unsupported
   expression form" diagnostic even though all three are ordinary value expressions.

2. **The call family was procedural-only.** Beyond the dispatcher gap, the call handlers themselves
   took the procedural pass class. The single reason they could not be templates was the LRM 7.12
   `with`-clause iteration variable, which was modeled as a procedural-body local; everything else
   in call lowering already used only the duck-typed surface.

This bites simulation-time expressions a structural scope owns. A continuous assignment is
simulation-time behavior -- it lowers to a process that re-drives its net on a value change -- so
its right-hand side is a simulation-time expression with the full value-expression set: function
calls, the bit-vector query `$isunknown`, the `inside` operator, replication. `always_comb y = f(x)`
lowered; the equivalent `assign y = f(x)` did not. The structural / constructor-time constraint that
a structural expression may not depend on simulation-time state (`runtime_model.md`) applies to
genuine constructor-time expressions -- generate conditions, parameter values, instance-array
dimensions, which the frontend already holds constant -- not to the continuous-assign right-hand
side.

## Decision

### Every context-free kind routes through one shared handler

The silent gaps came from sharing the per-kind handlers but not the dispatch: a context-free kind
wired into the procedural dispatcher and forgotten in the structural one failed only at runtime. The
fix is that every context-free kind -- now including the call, `inside`, and replication families --
is a single template over the pass class, and both pass-class dispatchers route their case to that
one handler. A kind is implemented once; only the genuine differences stay per-context:

- **Name resolution**, where a bare name maps to different storage depending on the enclosing scope.
- **Procedural-only legality** -- increment / decrement, the assignment expression, the
  dynamic-array constructor `new[]`, and the queue `$` -- which the LRM permits only in procedural
  code; the structural side returns an unsupported diagnostic.

At AST-to-HIR the two pass-class dispatchers, sharing a translation unit, collapse further into one
dispatcher template the two entries delegate to, so even the dispatch switch is written once. At
HIR-to-MIR the two dispatchers stay separate functions (they live in different translation units)
but route every context-free kind through the shared handlers; collapsing them likewise is the
remaining symmetric step.

The pass classes themselves stay distinct (`generic-lowering-machinery.md`): one builds a callable,
the other a class. It is the dispatch, not the pass class, that is shared.

### The `with`-clause element and index are co-equal closure parameters

An LRM 7.12 array-manipulation method with a `with` clause iterates an ordered entry stream of
`(index, element)` pairs (`array-manipulation-entry-stream.md`). At HIR-to-MIR the clause becomes a
closure (`closure.md`) whose two per-invocation parameters are the element and the index --
co-equal, exactly as every modern array callback passes them: JavaScript
`arr.map((element, index) => ...)`, Rust `arr.iter().enumerate()` yielding `(index, element)`,
Python `enumerate`, C++23 `views::enumerate`. None of these treats the index as an attribute of the
element; SV's `item.index` member syntax is surface spelling, not a semantic relationship.

So the element (`item`) and the index (`item.index`) both lower to one `IterationBindingRef` value
reference, naming the clause it belongs to and which of the two roles it is. Neither is a variable
of the enclosing scope -- they are the clause's own parameters. Naming the clause by identity (not a
"currently-active" marker) keeps a reference stable however deeply the referencing clause is nested,
so a clause nested in another's body still names the outer clause's parameter; HIR-to-MIR resolves
each identity to the synthesized closure's parameter and captures it when the reference sits in a
deeper clause's closure body. With that, the call handler reaches the pass class only through the
duck-typed surface and becomes one template like every other family, at both lowering boundaries.
The frontend's value system functions on the structural path (`$time` and the like) are the one form
not yet wired and return an unsupported diagnostic.

`$isunknown` falls out as an ordinary instance built-in call `(x).IsUnknown()` returning the SV
`bit` shape (`builtin-call-identity.md`), now reachable in both procedural and continuous-assign
positions.

## Rejected

- **Two per-context dispatchers.** The status quo, and the source of the silent gaps: a context-free
  kind wired into one dispatcher and not the other lowered fine in one position and failed in the
  other with no compile-time signal. Sharing the handlers but not the dispatch left the dispatch
  free to drift.

- **The iteration element as a procedural-body local.** Modeling the `with`-clause iteration
  variable as a local in the enclosing body's arena, resolved through the procedural-var registry,
  coupled call lowering to a procedural body a structural scope does not have. It was the sole
  dependency that kept the call family procedural-only, and it misrepresents the variable: a closure
  parameter scoped to the clause is not a variable of the enclosing scope.

- **The element as a value reference, the index as a member-access call.** Taking SV's `item.index`
  spelling literally models the index as an attribute reached through the element -- a different IR
  shape (a value reference versus a call) and a different resolution path for the two. The modern
  iteration model is unanimous that the index and element are co-equal yielded values; the asymmetry
  is a syntactic artifact, and encoding it splits one concept ("the current iteration's value")
  across two node kinds. One `IterationBindingRef` for both, selected by role, keeps them symmetric.

## Cross-references

- `generic-lowering-machinery.md` -- context-free expression handlers are one template over the pass
  class; this decision completes that migration with the dispatcher and the call family, its last
  holdouts.
- `array-manipulation-entry-stream.md` -- the `(index, element)` entry-stream model the element and
  index refer into.
- `closure.md` -- the `with` clause lowers to a closure whose per-invocation parameters are the
  element and the index.
- `runtime_model.md` -- constructor-time (structural) versus simulation-time (process) expressions;
  a continuous-assign right-hand side is simulation-time, so it carries the full value-expression
  set.
- `builtin-call-identity.md` -- `$isunknown` lowers to a built-in method call on its operand,
  returning the SV `bit` shape.
