# A structural expression may write

Date: 2026-09-21 Status: accepted

## Context

Expressions reach this compiler from two kinds of position: inside a procedure, where they are what
a running process evaluates, and outside one, where they are what a construction evaluates while it
builds the object graph. Both lowering boundaries carried the same claim about the second kind --
that an assignment and an increment cannot appear there, because the language admits neither outside
procedural code -- and both refused one as a front-end bug rather than lowering it.

The claim is false, and one clause is enough to show it. LRM 27.4 Syntax 27-2 gives a loop
generate's step three forms:

```
genvar_iteration ::= genvar_identifier assignment_operator genvar_expression
                   | inc_or_dec_operator genvar_identifier
                   | genvar_identifier inc_or_dec_operator
```

Every one of them writes, none of them stands inside a procedure, and a loop generate is the
construct the object graph is built with. So the position the refusal called impossible is the
central one of the whole construct.

What the refusal cost was not an error anybody saw. The step was decomposed before it reached the
expression lowering -- a plain assignment read for the value its right side named, an increment read
for the distance it moved -- so the refusal never fired, and instead the forms that could not be
decomposed that way fell back to building one artifact per iteration. Twelve of the seventeen
operator forms the clause admits fell back, including the one it names where it allows the index
array to be sparse.

## Decision

### D1. A write is the same thing wherever it is written

What an assignment writes and what it puts there is decided by the assignment, not by whether a
process or a construction reached it. So one lowering serves both positions, at both boundaries, and
a structural context is not a read-only context.

### D2. What differs is when the write takes place, and only a procedure has a choice

A procedure may defer an update to a later region of the time slot (LRM 10.4). A construction runs
before there is a time slot to defer into, so its write takes place where the construction reaches
it and there is nothing to choose. The deferral machinery is therefore procedural, sitting above a
store both positions share, rather than the assignment being procedural.

This is the whole of the difference, and it is what the old claim was reaching for: intra-assignment
timing and non-blocking updates really are procedural. Neither is the assignment.

### D3. A loop's step is carried as written, not as a value read out of it

A construct that states a step states it as an expression that writes. Reading it instead for "the
value the index takes next" recovers only the forms that name that value outright, so it turns a
complete grammar into a list of the spellings the reader happens to handle -- and a list like that
is indistinguishable from a complete one until somebody writes a spelling nobody enumerated.

A loop written among statements already carried its step this way. Since the standard defines that
step as a superset of a generate loop's, carrying the generate loop's step any other way was a
second answer to a settled question, not an incomplete one.

## Consequences

- Every form LRM 27.4 admits for a loop generate's step now reaches one compiled body built at every
  index the loop counts out. Before this, five did.
- A sparse index array -- what a step that is not an addition of one produces, and what the clause
  allows in as many words -- reaches the shared body for the first time. The index a block carries
  is the value the genvar held when it was elaborated, which is also the coordinate a name from
  outside spells, and neither is an ordinal.
- Nothing decides whether a loop's blocks are one body by looking at its step. The question is asked
  of the blocks alone.
- The refusals this removes were reachable only through a front-end bug, so no accepted program
  changes meaning. What changes is how many artifacts one is compiled into.

## Cross-references

- [one-body-built-at-every-index](one-body-built-at-every-index.md) -- the rule this serves, and the
  measurement of what one compiled body per loop is worth.
- [an-elaboration-time-value-is-an-input](an-elaboration-time-value-is-an-input.md) -- the same move
  one layer up: the compiled form states the expression the source wrote rather than an answer
  computed on its behalf.
- `../architecture/hierarchy_and_generate.md` -- generate as constructor-time logic the runtime
  executes, which is what makes a write at construction time ordinary rather than exceptional.
