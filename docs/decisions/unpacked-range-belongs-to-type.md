# Unpacked/container range belongs to the type, not the payload

## Date

2026-07-08

## Status

Accepted

## Scope

Fixed unpacked arrays and variable-size containers. Packed arrays are explicitly out of scope (see
"Packed carve-out"). This decision amends
[selector-coordinate-resolution](selector-coordinate-resolution.md) decision 1 for the unpacked
family.

## Decision

For unpacked/container arrays:

- The runtime **payload is ordinal-only** -- vector-like storage sized by element count, with no
  declared range.
- The **declared coordinate range is a fact of the receiver expression's static type.**
- **Coordinate-consuming operations** (element and slice select, and any locator that reports a
  declared index) **carry the range as an explicit MIR operand**, materialized at HIR-to-MIR from
  the receiver's static type.
- A **whole-value store copies the ordinal payload** into the destination and preserves the
  destination's declaration semantics; it performs no range relabel.
- **`ConformRange` is removed.**

## Invariants (guardrails)

1. **The range operand is part of the MIR Select node's semantic contract, not a C++ backend
   artifact.** Every backend sees the same operand. `Element(recv, index, receiver_range)` and
   `Slice(recv, a, b, form, receiver_range)` are the MIR node shapes; a mechanical LLVM backend
   consumes them identically to the C++ backend. No backend "adds" the range -- MIR carries it.

2. **`Var<T>::Set` stays ordinary whole-`T` assignment.** After this decision, for an unpacked array
   `T` _is_ the range-free payload type, so `Set` remains a plain `T` copy and neither `Var`,
   `ScopedMutation`, `Ref`, nor the scheduler ever knows about a range. This works _only_ because
   `range_` has been removed from `T`; `Set` must never grow semantic-store behavior.

3. **No legal SV operation may require the declared range label of an unpacked range-select
   result.** A slice result's payload is range-free (the result range is non-observable: LRM assigns
   none, and chaining a select after a range-select is illegal). If a future consumer needs a
   range-select result's declared range, it must define its own result-shape rule explicitly rather
   than assume the payload carries one.

## Why this beats recursive `ConformRange` (first principles)

SV unpacked assignment is position-wise (LRM 7.6: element correspondence is left-to-right ordinal).
The declared range is _coordinate naming_ -- which storage ordinal a source index denotes -- not
payload representation: storage is a `std::vector` sized by count, endpoint- independent; there is
no `SameRepresentation` over an unpacked range; every whole-value surface (copy, equality,
`IsBitIdentical`, positional `'{...}` print, sort, reduction) is ordinal. `DynamicArray<T>` already
carries no range and runs the identical surface.

Putting the range inside the movable payload forces whole-value movement (which must not touch
coordinate labels) and selection (which must) to share one object copy. A cross-range store then has
to _repair_ a descriptor a plain copy wrongly carried -- that repair is `ConformRange`. Recursive
`ConformRange` is the honest, correct consequence of _keeping_ the range in the payload: it re-walks
the full dim stack on every cross-range store to relabel what the type system already knows. The
range-free model removes the repair: the store is a payload copy, and the range enters at selection,
the operation that consumes coordinates. Construction already passes a type-derived `UnpackedRange`
to size the payload; selection using a type-derived range is the symmetric read- side operation.

## X/Z-aware resolution is preserved

[selector-coordinate-resolution](selector-coordinate-resolution.md) decision 2 (rebase runs in the
value's wide, X/Z-aware domain, never as narrow selector arithmetic) is unchanged. The runtime
helper that maps a source index to a storage ordinal still runs in the wide domain and still yields
the invalid/shield result for an X/Z or out-of-range index. The only change: the range it resolves
against arrives as an operand instead of a stored field. No rebase moves to the selector's narrow
type; no `i - left` is synthesized at lowering.

## Backend-contract clean

The range operand is materialized at HIR-to-MIR from the receiver's static type and travels as an
ordinary MIR value -- the prototype/companion-value pattern (`backend_contract.md`: a shape a
factory needs travels as a MIR value, never composed by render from a node's type). Render emits it
mechanically; it does not read another node's type to synthesize it. This mirrors how construction
already emits `UnpackedRange{l,r}`. Render stays a fixed function of one node (Inv 1); no
value-emission entry gains a decision (Inv 2); the Inv-5 LLVM falsifier passes.

## Every value boundary works

The governing range is always the local static type at the select site, so no runtime range ever
travels across a boundary or lives on a cell:

- **Module variable** (`Var<Payload>`): the select receiver node carries the variable's static
  `UnpackedArrayType`; the operand comes from it.
- **Automatic local** (plain C++ `T`, no Var/cell): its slot has a static MIR type; the select
  materializes the range from it. This is why the model lands where a declaration-wrapper approach
  could not -- an automatic local has no wrapper.
- **Function return** (bare by-value payload): `f()[i]`'s range comes from the receiver `CallExpr`'s
  static return type; the function returns no `{payload, shape}` pair.
- **`ref`/`const ref` formal and `ref` port**: the governing coordinate system is the callee formal
  / child port local declared static type (LRM 13.5.2 requires only _equivalent_ types),
  materialized at the callee's select. Only payload crosses the boundary.
- **By-value / port**: a position-wise copy into the local formal/port, whose own static type
  governs its selects.

## Exact MIR and C++ shapes

For `int a[1:3][1:4]`, `int b[3:1][4:1]`, `f` returns `int[3:1]`, `s : int[1:7]`:

```
SV:   a[i]
MIR:  Element(recv = a, index = i, range = <operand Range{1,3} from a's static type>)
C++:  a.Get().Element(Int(i), UnpackedRange{1,3})

SV:   a[i][j]
MIR:  Element(Element(a, i, Range{1,3}), j, Range{1,4})   // inner range from a's element type
C++:  a.Get().Element(Int(i), UnpackedRange{1,3}).Element(Int(j), UnpackedRange{1,4})

SV:   s[2:5]
MIR:  Slice(recv = s, a = 2, b = 5, form = const, range = <operand Range{1,7}>)
C++:  s.Get().Slice(2, 5, /*const*/, UnpackedRange{1,7})   // result payload is range-free

SV:   a = b
MIR:  Store(dest = a, src = b)                             // no ConformRange
C++:  a.Set(services, b.Get())                            // plain ordinal payload copy

SV:   f()[i]
MIR:  Element(recv = f(), index = i, range = <operand Range{3,1} from f's return type>)
C++:  f().Element(Int(i), UnpackedRange{3,1})
```

`i`/`j`/`2`/`5` stay raw SV coordinates; the range is a type-derived operand, never `i-1`, never
render-synthesized, never a payload field.

## Packed carve-out

Packed `dims` stays in the packed value. It is not pure coordinate naming: its product is the flat
storage width (a representation requirement) and the full stack participates in
`SameRepresentation`, asserted on every packed store (`[7:0]` and `[3:0][1:0]` share storage yet are
not same-representation and select differently). A packed value stays self-describing; this decision
does not touch it. Applying the unpacked split to packed is a category error.

## Performance

No current runtime performance data exists (tracking is pending the execution backends), so this is
a complexity argument, not a measurement.

- Extra operand to `Element`/`Slice`: a 16-byte `UnpackedRange` by value; no heap, no indirection.
- No allocation added; the payload move is the same vector copy.
- No type-tree walk: the range is a flat operand per select level, materialized at lowering --
  nothing re-derives it by walking a nested descriptor at runtime (which is what recursive
  `ConformRange` would add on every store).
- Bounds are an immediate/local value: a folded constant today; forward-compatible with dims
  becoming constructor-input runtime values (the operand then sources the receiver's runtime dim,
  not a C++ template parameter -- honoring `parameter-code-shape-over-approximation.md`).
- Coordinate resolution stays in the wide X/Z-aware runtime helper, taking the range as an argument.
- Store gets cheaper: `ConformRange` (a whole-value copy plus relabel) becomes a plain payload copy.

## Rejected alternatives

- **Recursive `ConformRange`** -- keep the range in the payload but make the store relabel every
  dimension. Correct, but wrong-shaped: it re-walks the type tree at runtime on every cross-range
  store to restate what the type already knows, and it keeps the range on the movable payload, which
  is the root reason a plain `a = b` needs a repair at all. See "Why this beats recursive
  `ConformRange`" above -- it is the honest consequence of the payload-range model, not a fix for
  it.

- **Range as a C++ template parameter** -- `std::array<T, N>`, or a non-type template parameter
  `Typed<..., Range<1, 3>>` on the value type. Rejected:
  [parameter-code-shape-over-approximation](parameter-code-shape-over-approximation.md) forbids
  baking an extent/range into the type, because the committed direction makes unpacked dimensions
  constructor-input runtime values and anything type-baked would have to be torn out; it would also
  split every type-erased boundary (format, change detection, a value variant) by range. An unpacked
  extent is already not in the C++ type today.

- **Range as a per-selection runtime argument the caller computes** -- `a.Element(i, left, right)`
  where `left` / `right` are values a caller threads in. This is the alternative
  [selector-coordinate-resolution](selector-coordinate-resolution.md) rejected: the coordinate
  system is scattered across call sites rather than owned. The chosen model looks similar at the C++
  call but is not this one -- the range is a MIR operand materialized from the receiver's _static
  type_ at lowering (the prototype pattern), so the type owns it and no caller computes or threads a
  runtime coordinate system.

- **Range on a per-declaration cell wrapper** -- the value holds only payload; a wrapper beside each
  declared variable holds the range; `Var<Payload>` is unchanged. Rejected: it does not land
  uniformly. An automatic local is a plain C++ value with no cell or wrapper to host a range, and a
  function return and a temporary have none either. Since the receiver's static type carries the
  range at every one of those sites anyway, a stored wrapper is both insufficient (locals,
  temporaries) and unnecessary.

## Consequences / amendments

- Multi-dimensional arrays need no special handling. An unpacked value is `UnpackedArray<...>`
  nested one layer per dimension; each select consumes exactly one dimension's range from its
  receiver's static type, so `a[i][j][k]` is three independent single-dimension selects and any rank
  adds no logic. A whole-array store is a plain nested copy at any rank. (Packed multi-dim is
  unrelated -- a flat dim stack on one value, per the carve-out.)
- [selector-coordinate-resolution](selector-coordinate-resolution.md) decision 1: for
  unpacked/container, the receiver's static type owns the coordinate system, materialized at the
  select as a MIR operand; the payload owns ordinal data only. Decisions 2, 3, 4 stand. Packed
  unchanged.
- [value-store-discipline](value-store-discipline.md): the store still preserves the destination's
  declared type, but its realization drops `ConformRange` (a payload copy into a destination-typed
  cell).
- [unpacked-array-representation](unpacked-array-representation.md) invariant 2: "the value carries
  its declared range" becomes "the type carries the range; the value carries ordinal payload".

## Confirmed evidence

- Source-build verified defect of the payload-range model: `int a[1:3][1:4] = b[3:1][4:1]` then
  `a[1][1]` reads 0 (should be 99); `ConformRange` conforms only the outer dim. A symptom of the
  store-repair this decision eliminates, not the justification.
- Not defects (retracted): foreach index is correct (declared 1..7); the differing-range `ref` case
  is a support-policy question (slang accepts, Verilator rejects as requiring matching types), not a
  confirmed bug.
