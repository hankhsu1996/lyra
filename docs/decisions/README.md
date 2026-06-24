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
- [string-packed-conversion](string-packed-conversion.md) -- a `value::String` holds no NUL;
  packed-to-string conversion strips NUL (LRM 6.16) while `%s` formats bits without building a
  string value (LRM 21.2.1.7).

### Aggregate types and access

- [unpacked-array-representation](unpacked-array-representation.md) -- representation of a
  fixed-size unpacked array.
- [slice-value-semantics](slice-value-semantics.md) -- a slice read materializes an owned value; the
  access model is value, not borrow / view.
- [queue-operators](queue-operators.md) -- queue access operators (`$`, slice, concatenation,
  equality, append) lower to built-in method calls; read and write are different methods chosen at
  lowering.
- [array-method-dispatch](array-method-dispatch.md) -- LRM 7.12 array-method runtime semantics
  (empty-reduction zero, selection sort over standard introsort); the original per-family dispatch
  shape is superseded by [builtin-call-identity](builtin-call-identity.md).
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
- [builtin-call-identity](builtin-call-identity.md) -- built-in method calls carry a flat
  closed-namespace identifier (`support::BuiltinFn`) shared between HIR and MIR; per-family enum
  splits are out.
- [event-control-unification](event-control-unification.md) -- unified treatment of event control.

### References and construction

- [hierarchical-reference-resolution](hierarchical-reference-resolution.md) -- how a cross-unit
  hierarchical reference resolves once into a stored pointer: downward by emitted constructor
  navigation, upward by a runtime self-climb at bind, both by name; and why the cross-unit hop is by
  name, not a typed accessor.
- [specialization-identity](specialization-identity.md) -- a specialization's identity is a
  deterministic name (module name + content hash of a canonical, structural serialization of its
  parameter bindings), computed independently by producer and consumer and matched by name;
  injective mangling, body fingerprinting, and a design-global key map are rejected.

### Compile-time model and specialization

- [parameter-code-shape-over-approximation](parameter-code-shape-over-approximation.md) -- every
  parameter is treated as code-shape-affecting for now (a conservative over-approximation of the
  specialization key); parameter classification and constructor-input threading are deferred, and
  the instance-array storage shape stays forward-compatible (vector wrapper, count as a
  constructor-side value, never extent-in-the-type).

### Diagnostics

- [diagnostic-construction](diagnostic-construction.md) -- a diagnostic's kind is a property of its
  code, derived at construction; construction is infallible (no re-supplied value, no validating
  guard); the consumer-less `UnsupportedCategory` axis is removed.

## File Naming

`kebab-case.md`. The name describes the decision, not when it was made; the date lives inside the
file. Existing example: `integral-representation.md`.

## Shape

There is no fixed template. The existing `integral-representation.md` is the reference for shape:
title, date, status, the model or findings that shaped the decision, the decision itself, and the
consequences that follow. Let the subject drive the structure; a decision with no rejected
alternative or no load-bearing invariant probably should not be a decision entry.
