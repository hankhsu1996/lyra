# Values are pure; the store boundary preserves the declared type

## Date

2026-06-29

## Status

Accepted

## Why this decision matters

A runtime value was doing two jobs that pull against each other: being a value, and being a storage
cell that keeps its declared type across assignment. The integral value made part of its declared
type -- its shape (dimensions, signedness, state domain) -- mutable runtime state. So to keep a
variable's declared type across `a = b`, the integral value's own assignment operator preserved the
destination's shape instead of adopting the source's.

That single choice forced a chain of special cases: a guard that the source already matched, an
uninitialized sentinel that adopted a shape on the first store, a hand-written moved-from that
stayed valid, and a relocation-only swap that bypassed the preserving assignment. And it did not
stay contained -- every aggregate or container holding an integral leaf inherited it: a product ran
the preserving assignment member-wise, a container grew a bespoke swap, a container recovered each
element's shape only by holding a prototype. Almost everything in SystemVerilog bottoms out at an
integral leaf, so almost everything paid the tax. Mixing the value role with the storage-cell role
was also the root of a bug class -- a chained multi-dimensional read once dropped its shape and read
back as a single bit.

## The model

Split the two jobs.

A **value is pure.** Its declared type is an immutable descriptor fixed at construction; copy, move,
and assignment all replace the whole value -- descriptor and contents together. No value type
carries a sentinel, a shape-preserving assignment, a hand-written moved-from, or a relocation-only
swap. The genuinely pure leaves -- the real family, the string -- are the shape every value type now
matches.

**Keeping the destination's declared type across assignment lives at the store, not in the value.**
The variable cell owns its declared type, installed once at construction; a store overwrites the
cell's contents while keeping its declared descriptor. The store boundary converts the right-hand
side to the destination's full declared type before it lands, so by the time a value reaches the
cell it already matches. SystemVerilog assignment semantics live here -- at the store boundary and
the cell -- never inside a value type's assignment operator.

The distinction that keeps this honest: two different things in the emitted program both look like
"assignment", and they must never be confused. A **semantic store** -- a SystemVerilog assignment,
an initialization, a port bind, a writeback -- converts its source to the destination's declared
type (the conversion sits upstream) and overwrites a cell that owns its type. An ordinary **value
movement** -- a copy, a move, a container reshuffle, returning a temporary -- adopts the whole value
with no conversion, because a pure value already carries its own type. The first preserves the
destination's type; the second carries the source's. A semantic store that reaches for the bare
whole-value assignment is the defect this split exists to prevent.

## Consequences

- This generalizes [integral-representation](integral-representation.md): the integral value stays
  "fat" (shape as runtime state, one class for every integral), but that shape is now immutable
  after construction, and the job of preserving a destination's shape across assignment has left the
  value entirely.
- A select of a packed-struct field, whose declared type can differ from the part-select's natural
  type, states that difference as an explicit conversion at lowering -- "convert at the boundary,
  keep the value pure" -- rather than letting a value or the runtime re-derive the result type.
- The container out-of-bounds default mechanism (a per-container prototype element) is a separate
  concern, untouched by this split; see
  [runtime-shape-and-default-value](runtime-shape-and-default-value.md).

## Cross-references

- [unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md) -- for the unpacked/container
  family the semantic store is realized as a plain ordinal-payload copy: the payload carries no
  declared range, so nothing needs conforming at the boundary and the `ConformRange` step is
  removed. The store-preserves-the-destination-type principle is unchanged.
- [integral-representation](integral-representation.md) -- the fat-integral representation this
  generalizes.
- [runtime-shape-and-default-value](runtime-shape-and-default-value.md) -- the container default
  mechanism, deliberately out of scope.
