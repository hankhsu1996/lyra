# Design Principles

Universal coding patterns for maintainable software.

## No Workarounds

Never accept workarounds or band-aid fixes. We are building a large, complex system at an early stage - every architectural decision compounds. A workaround today becomes expensive technical debt tomorrow.

Signs you're creating a workaround:

- Forcing data into a structure it doesn't naturally fit
- Adding special cases for "just this one thing"
- Saying "this works for now"

The fix: step back, understand why the current design doesn't accommodate the new requirement, and extend the design properly.

## Parameterize, Don't Specialize

When multiple constructs differ only by a value, create one parameterized construct instead of many specialized ones.

Signs you need this:

- Multiple classes/functions with nearly identical structure
- Names that follow a pattern (ThingA, ThingB, ThingC)
- Adding a new variant requires creating a new class

The fix: identify what varies, make it a parameter.

## Capture Behavior at the Source

When information is known at one point but needed at another, capture it as behavior rather than as data that must be reconstructed.

Signs you need this:

- Storing data with a tag, then switching on the tag to interpret it
- Losing context through an interface, then trying to recover it
- Reconstruction logic that mirrors creation logic

The fix: capture a callable (lambda, function object) that embodies the behavior when context is available.

## Unify Before Multiplying

Before adding a new variant, look for the abstraction that unifies existing variants with the new one.

Signs you need this:

- Adding "just one more case" to a switch
- Copy-pasting with small modifications
- Parallel hierarchies that evolve together

The fix: find the common structure, parameterize the differences.

## Friction Reveals Misplaced Responsibility

When a solution creates inconsistency or complexity in one layer, the problem likely belongs in a different layer.

Signs you need this:

- Breaking uniformity to accommodate a requirement (e.g., one function needs a different signature)
- Adding complexity that doesn't fit the abstraction's natural purpose
- The solution feels like it's fighting the existing design

The fix: ask "where does this responsibility naturally belong?" and move it there.

## Follow Established Patterns

Before implementing, find how similar problems are solved elsewhere in the codebase. Mirror existing patterns unless there is a clear reason to diverge.

Signs you need this:

- Writing new code for behavior that existing infrastructure already provides
- Creating a parallel mechanism when an existing one can be extended
- The new code looks simpler but doesn't integrate with the rest of the system

The fix: extend or compose existing infrastructure rather than building alongside it.

## Be Consistent

Consistency reduces cognitive load. Use the same conventions everywhere: naming (dashes vs underscores), formatting, patterns, terminology. When adding something new, match what exists.

## Use Domain Vocabulary

Name things using terminology from the problem domain's authoritative sources. Precision in naming prevents ambiguity and aids discoverability.

## Comments Explain Why, Not What

Code should be self-explanatory. Comments add context the code cannot express: motivation, trade-offs, non-obvious techniques. If code needs explanation, improve the code (better names, clearer structure) rather than adding comments.
