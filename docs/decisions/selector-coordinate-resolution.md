# Array selection is semantic; the selected value resolves its own declared coordinates

## Date

2026-07-02

## Status

Accepted

## Model

A fixed unpacked array is declared over an index range, and the range is part of the array's
identity, not a size in disguise. `logic a[1:7]`, `logic b[7:1]`, and `logic c[0:6]` all hold seven
elements, yet `a[1]`, `b[1]`, and `c[1]` name three different elements. A selection `a[i]` is only
defined relative to the array's own coordinate system: which storage element the source index `i`
names depends on the declared range and its direction.

The selection model follows. A select expression carries a _semantic_ selection -- the value being
selected and the source-level selector -- and nothing more. The selected value owns its declared
coordinate system and resolves a source selector to a storage position. Lowering translates a source
selection faithfully:

```text
a[i]       ->  a.Element(i)
a[hi:lo]   ->  a.Slice(<source selector>)
```

and never computes `i - 1`, `left - i`, or `base +/- (width - 1)`. Resolving a declared coordinate
to a storage position is the selected value's job, done once, inside the value.

## Decision

1. **The selected value owns its declared coordinate system.** A selectable value carries its
   declared range as an immutable descriptor fixed at construction: a packed value carries its
   dimension stack, an unpacked value carries its `UnpackedRange`. `value.Element(i)` is a complete
   operation -- the value alone determines which storage position the source index `i` names.
   Neither the lowering nor a per-selection argument supplies the coordinate system.

   **Amended for the unpacked/container family** by
   [unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md): an unpacked/container
   payload is ordinal-only and carries no range; the coordinate system is a fact of the receiver
   expression's static type, materialized at the select as an explicit MIR operand. This packed
   clause (dimension stack on the value) is unchanged -- packed dims is a storage-representation and
   `SameRepresentation` identity fact, not pure coordinate naming. Decisions 2, 3, 4 below hold for
   both families.

2. **Coordinate resolution runs in the value's wide, X/Z-aware domain, never as narrow selector
   arithmetic.** Rebasing a declared coordinate to a storage position (`i - left`, `left - i`, and
   the indexed-part-select width offset) happens inside the value in a canonical wide domain that
   preserves X/Z. It is never a native operator on the selector's own -- possibly narrow, possibly
   four-state -- type. A narrow rebase can wrap a genuinely out-of-range selector into an apparently
   valid position, and a four-state selector against a two-state bound shares no storage domain;
   both are defects of resolving in the selector's type instead of the value's domain.

3. **The value's access surface is coordinate-facing; the storage position is private.** A
   selectable value exposes element and slice access that take a source-level coordinate. The
   storage position it resolves to is an internal detail below a single resolution point -- never a
   public method, never a coordinate that flows between runtime components. A value is reached
   through its coordinate-facing surface, so no caller re-derives a storage position under the wrong
   coordinate system.

4. **The declared range is selection-only.** Whole-value movement -- assignment, copy, argument
   passing, equality -- is position-wise and range-agnostic (LRM 7.6: element correspondence is
   left-to-right order, so `int a[7:0] = int b[1:8]` assigns `b[1]` to `a[7]`). The range is fixed
   at construction and never adopted from a source; a semantic store carries the destination's
   declared range, converting the source at the store boundary (see
   [value-store-discipline](value-store-discipline.md)). Only element and slice selection consult
   the range.

## Consequences

- A slice read is a materialized owned value whose storage copies position-wise into any
  destination, per decision 4. (For the unpacked/container family, that value is ordinal-only
  payload and carries no declared sub-range -- the result range is non-observable; see
  [unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md). A packed slice value still
  carries its dimension stack.)
- Packed, unpacked, and the zero-based families sit under one model. A packed value carries its
  dimension stack and resolves in its bit domain. A dynamic array and a queue are declared
  zero-based, so their coordinate system is the identity and resolution is a no-op; a queue's slice
  bounds are still resolved and clamped inside the value (LRM 7.10.1), not synthesized at lowering.
- A selector lowering that synthesizes no coordinate arithmetic emits no operator whose operands the
  frontend did not already unify. The general operand reconciliation a lowering otherwise carries
  for such synthesized operators has no input and is removed; the invariant that native binary
  operands share a storage domain is enforced structurally rather than repaired.

## Rejected alternatives

- **Resolve the coordinate at lowering (fold `i - left` into the selector index).** The value
  becomes storage-only and every caller must remember its coordinate system and pre-rebase. The
  rebase is synthesized as an operator on the selector's own narrow, possibly four-state type -- the
  wrap and storage-domain-mismatch defects of decision 2 -- and a select node carries a storage
  computation rather than a semantic selection.
- **Pass the declared range alongside the value as a per-selection argument.** The value's
  `Element(i)` stays under-defined without an external range, so the coordinate system is scattered
  across every call site rather than owned by the value. This is the "shape passed in alongside the
  value" shape rejected for the same reason a packed element carries its own shape (see
  [runtime-shape-and-default-value](runtime-shape-and-default-value.md)). **Superseded for the
  unpacked/container family** by
  [unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md): there the range is a
  type-derived MIR operand owned by the receiver's static _type_, not a runtime quantity scattered
  by callers -- so it is not the rejected shape. It remains rejected for packed, whose dims stay on
  the value.
- **A public storage-position access on the value.** Once a zero-based position is a callable
  protocol, a caller computes one under the wrong coordinate system and the leak returns. The
  position stays private below the single resolution point.

## Cross-references

- LRM 7.4.5 / 7.4.6 (indexing, slicing, operations on arrays), 7.6 (array assignment element
  correspondence), 11.5.1 (bit / part / indexed part-select), 7.10.1 (queue operators and clamping).
- `architecture/mir.md` -- a select is a semantic access primitive; MIR does not own coordinate
  resolution.
- [integral-representation](integral-representation.md),
  [runtime-shape-and-default-value](runtime-shape-and-default-value.md) -- a value carries its own
  declared shape rather than receiving it alongside.
- [value-store-discipline](value-store-discipline.md) -- the store boundary that gives an assignment
  the destination's declared range.
- [unpacked-array-representation](unpacked-array-representation.md) -- the unpacked value that
  carries its `UnpackedRange`.
- [slice-value-semantics](slice-value-semantics.md) -- the slice read that materializes the
  sub-ranged value.
- [queue-operators](queue-operators.md) -- the queue access whose bounds resolve inside the value.
