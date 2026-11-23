# An elaboration-time value is an input, not a constant

Status: Accepted, 2026-09-18.

## Requirement

The count of compiled artifacts follows distinct units and bodies, never instances
([`north_star.md`](../architecture/north_star.md) invariant 2). The front end evaluates every
expression it can, so beside nearly everything a body states sits an answer free to take. Taking it
changes nothing the program means and decides where the value enters the artifact. A value read at
lowering is compiled in, so two elaborations that settled it differently compile to two artifacts,
while a value left as an expression is supplied to one artifact when it is built. The choice was
being made site by site against no stated rule.

## Choice

- **What a body states is lowered as the expression the source wrote, never as the answer the front
  end settled.** Evaluating it is an optimization, which belongs after the shape is fixed. A clause
  requiring a constant expression changes nothing, since it says the value is known before the
  program runs, which is a fact about the source, not that the artifact has to hold it.
- **What decides whether a value belongs to the class is one question.** If this value were
  different, would this be a different class, or the same class holding a different value? A value
  deciding which members, types or instantiated units exist changes the class, and the class holds
  it. A value that only ends up as a number a field holds, a count a loop runs to or an index a
  query names is supplied at construction.
- **The question is answered by what the artifact does with the value, read from whoever owns its
  representation**, never by how important the value sounds. A declared width sits in a type, so
  whether it belongs there is answered by the value representation
  ([a-value-is-its-machine-data](a-value-is-its-machine-data.md)), not here.
- **The rule reaches what a body states and not the declaration surface.** A unit's header and a
  type declaration are elaborated once per parameterization, which is already its own artifact, so
  an index cannot reach them and reading their settled value costs no second artifact.

## Alternatives turned down

- **Reading a settled value wherever the standard fixes the position, citing the clause.** It fails
  invariant 2 of `north_star.md`, since a fixed position can still differ between elaborations. The
  failure rests on a citation saying the value is known, which says nothing of where it belongs; the
  one candidate for the exception failed the question above on inspection.
- **Settling a value because rarely written code would pay otherwise.** It fails the same invariant,
  and rarity is no argument the rule may weigh, being no cost-benefit judgement.
- **A check matching the spellings of taking a cached answer, read as the rule itself.** It fails
  catching the next spelling, which is outside the list on the day it lands; a parameter's own
  accessor walked past such a check while this record called the list complete. What a site owes is
  the name of what its value varies with, checked against the axes an artifact's identity has.

## Consequences across layers

- Emitted code grows where a value that was a constant becomes an expression the program evaluates.
  A constant a later pass recovers costs nothing anyone must know of; an artifact per elaboration is
  a wall no later pass takes down.
- A loop whose bodies query a dimension by their index, reach back in `$past` by it, declare a
  parameter from it, or select a neighbouring generate block by it compiles to one body
  ([`specialization_model.md`](../architecture/specialization_model.md),
  [`compiling_a_design.md`](../architecture/compiling_a_design.md)).
- A position the standard leaves without a run-time answer takes the reading its clause gives a
  question it cannot answer, so a dimension query naming a dimension of no single extent reads as
  `x` (LRM 20.7, 20.7.1).
