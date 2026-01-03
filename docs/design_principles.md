# Design Principles

Universal coding patterns for maintainable software.

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

## Follow Established Patterns

Before implementing, find how similar problems are solved elsewhere in the codebase. Mirror existing patterns unless there is a clear reason to diverge.

## Use Domain Vocabulary

Name things using terminology from the problem domain's authoritative sources. Precision in naming prevents ambiguity and aids discoverability.
