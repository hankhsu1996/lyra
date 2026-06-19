# Decisions

Logged architectural decisions. Each entry records a decision with its rationale; the entry is
immutable once accepted, and a superseding decision links back to the one it replaces.

Decisions are reserved for choices with real trade-offs: rejected alternatives, load-bearing
invariants, or constraints that bind the codebase going forward. Housekeeping notes (e.g., "this
archive item is subsumed by an existing surface") do not warrant a decisions entry; record the
reason inline at the point it matters.

## Index

Grouped by subject so a decision is findable by concept, not only by filename. One line per entry.

### Value types and representation

- [integral-representation](integral-representation.md) -- one fat `PackedArray` carries integral
  shape (width / signedness / 4-state / dims) as runtime fields, not C++ template parameters.
- [value-assignment-and-moved-from](value-assignment-and-moved-from.md) -- assignment preserves the
  destination's declared shape and copies bits in; a moved-from value stays valid for STL
  relocation.
- [value-type-concepts](value-type-concepts.md) -- the `lyra::value` operator surface is a lattice
  of composable C++ concepts, one per LRM operator family.
- [runtime-shape-and-default-value](runtime-shape-and-default-value.md) -- runtime shape lives on
  `PackedArray`; collections carry one OOB shield slot that is both the canonical-default source and
  the out-of-bounds-write discard target.

### Aggregate types and access

- [unpacked-array-representation](unpacked-array-representation.md) -- representation of a
  fixed-size unpacked array.
- [slice-value-semantics](slice-value-semantics.md) -- a slice read materializes an owned value; the
  access model is value, not borrow / view.
- [queue-operators](queue-operators.md) -- queue access operators (`$`, slice, concatenation,
  equality, append) lower to built-in method calls; read and write are different methods chosen at
  lowering.
- [array-method-dispatch](array-method-dispatch.md) -- how the LRM 7.12 array-manipulation method
  family dispatches per receiver type.
- [format-dispatch](format-dispatch.md) -- value formatting dispatches through `Formatter<T>` and
  `FormatArg`.

### Lowering and IR shape

- [lowering-organization](lowering-organization.md) -- how lowering passes organize their internal
  objects (facts, registries, builders, walk frame).
- [foreach-lowering](foreach-lowering.md) -- the lowering shape of `foreach`.
- [conversion-folding](conversion-folding.md) -- when type conversions are folded.
- [variable-initialization](variable-initialization.md) -- LRM 10.5 variable initialization as a
  constructor-scope statement.
- [variable-lifetime-storage](variable-lifetime-storage.md) -- storage of static-lifetime body
  locals.
- [read-set-inference](read-set-inference.md) -- read-set inference via slang flow analysis.
- [runtime-effects-as-generic-calls](runtime-effects-as-generic-calls.md) -- runtime effects
  (`$display`, `$finish`, file IO) lower to ordinary `CallExpr` with the engine handle as one
  argument.
- [callable-receiver](callable-receiver.md) -- every callable body's first binding is `self`; how it
  is supplied differs per callable form.
- [event-control-unification](event-control-unification.md) -- unified treatment of event control.

### References and construction

- [upward-reference-resolution](upward-reference-resolution.md) -- upward hierarchical references
  resolve by self-climb at construction time.

## File Naming

`kebab-case.md`. The name describes the decision, not when it was made; the date lives inside the
file. Existing example: `integral-representation.md`.

## Shape

There is no fixed template. The existing `integral-representation.md` is the reference for shape:
title, date, status, the model or findings that shaped the decision, the decision itself, and the
consequences that follow. Let the subject drive the structure; a decision with no rejected
alternative or no load-bearing invariant probably should not be a decision entry.
