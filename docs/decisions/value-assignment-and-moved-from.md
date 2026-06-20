# Assignment and Moved-From Contract for Shape-Bearing Value Types

## Date

2026-06-17

## Status

Accepted

## Why this decision matters

`integral-representation.md` decided that an integral's shape -- `bit_width`, `signedness`,
4-state-ness, and packed `dims` -- lives as runtime fields on one `lyra::value::PackedArray` rather
than in C++ template parameters (a real design had ~10k integral variations, so templating
explodes). That makes `PackedArray` a "fat" value: it carries its own type description. A fat value
plays two roles at once -- a SystemVerilog value with SV assignment semantics, and an ordinary C++
object that STL containers (`queue` over `std::deque`, dynamic array over `std::vector`) relocate
with move / copy / assignment. Those two roles pull in opposite directions on `operator=`, and
leaving the conflict unstated produced two concrete bugs: a crash when inserting into a queue, and
silent corruption of `logic [3:0][7:0] arr = '{4{8'hAB}}` where `arr[0]` read `1` instead of `0xAB`.
This record fixes what assignment means and what a moved-from object is.

## Findings that shaped the design

### F1. Shape is type information, not value content

Of everything a `PackedArray` carries, only the value bit-plane (and, for 4-state, the X/Z plane) is
the _value_. `bit_width`, `signedness`, 4-state-ness, and `dims` are the _declared type_, fixed when
the variable is constructed. A SystemVerilog variable keeps its declared type across assignment
(LRM: the right-hand side is coerced to the left-hand variable's type), so assignment must
**preserve the destination's shape and copy only the bits in**. This is the explicit, dims-inclusive
form of `integral-representation.md`'s consequence that "the destination's
`(bit_width, is_signed, is_four_state)` drives" a conversion.

### F2. A vector's size is value; a packed array's width is type

The instructive contrast is `std::vector`: its size is genuine value state, so `v1 = v2` adopts
`v2`'s size, and pure C++ value semantics are correct. A `PackedArray`'s width / dims are _not_
value state -- they are the variable's fixed type -- so assignment must preserve them, not adopt the
source's. This one difference is the whole reason `operator=` cannot be a plain value adopt.

### F3. STL relocation requires a valid moved-from object

`std::deque::insert` / `std::vector::insert` / `erase` relocate elements by move-constructing one
slot and then move-assigning into the vacated slot. The move-assignment reads and writes the
destination's storage. So a moved-from `PackedArray` must remain a fully formed value -- valid
storage of its width, and its `dims` intact -- not an empty husk. A defaulted move that keeps
`bit_width_` but steals the storage and dims leaves an object that crashes the next assignment into
it (`BitView` over zero words) and, if it survives, indexes at the wrong granularity.

### F4. Rejected: make `operator=` a pure C++ value adopt

The tempting "clean" move is to default `operator=` to adopt the source entirely, so `PackedArray`
becomes trivially STL-safe. Rejected: adopt copies the source's `dims`, destroying the destination
variable's declared structure. `logic [3:0][7:0] arr = '{4{8'hAB}}` assigns a flat 32-bit value
whose dims are `[32]`; adopt replaces `arr`'s `[4][8]` dims with `[32]`, so `arr[0]` extracts a
single bit. Pure value-adopt fights the fat-value model and is wrong for SV variable assignment. (A
"thin value" model -- bits only, shape as compile-time type, backend emits per-type indexing --
would make adopt correct, but that is the LLVM lowering's representation, not the C++ backend's; see
`integral-representation.md` F6. It is explicitly out of scope here.)

## Decision

`lyra::value` shape-bearing value types (`PackedArray` first; the same contract governs any future
value type whose shape is declared rather than value-derived) follow these invariants:

1. **Assignment preserves the destination's shape.** `operator=` (copy and move) routes through
   `AssignFrom`, which keeps this object's `bit_width` / `signedness` / 4-state / `dims` and copies
   the source's bit-planes in. It is deliberately NOT a pure C++ value adopt.
2. **Construction adopts the source's shape.** Copy / move constructors take the source's shape,
   because a freshly constructed object has no declared shape to preserve.
3. **A moved-from object is a valid value of the same shape**, never an empty husk: the move
   constructor keeps the source's `dims` and rebuilds its storage to the canonical zero of its
   width. This is the property that makes the type safe inside STL containers; it is the
   C++-mechanics base (`Storable`, the `std::copyable` half of `LyraValue`) asserted on every value
   type. The concept is a relocation-safety guarantee, not a claim that assignment is value-adopt.
4. **Cross-shape coercion is an explicit `ConvertFrom`, not assignment.** When a value must change
   width / state to reach the destination's shape, the lowering emits `PackedArray::ConvertFrom`
   ahead of the store; `AssignFrom` then runs same-shape and asserts (`ExpectSameShape`) that the
   conversion was in fact emitted. A shape mismatch reaching `AssignFrom` is a compiler bug, not a
   user error.
5. **In-container reordering uses `swap`, not assignment.** `sort` / `rsort` / `reverse` move
   elements structurally, which adopts shape and is correct; the ADL `swap` friend is that
   primitive, kept distinct from the shape-preserving `operator=`.

## Consequences

- Queue and dynamic-array `insert` / `erase` / `push` / `pop` use plain STL container operations; no
  per-container element-shuffling workaround is needed for move safety. The fix lives once, in the
  value type's move constructor.
- `operator=` is intentionally not pure C++ value semantics. This is documented on the type so STL
  code and readers are not surprised that assignment throws on a shape mismatch and ignores the
  source's declared shape.
- The shape-mismatch assertion in `AssignFrom` is the safety net for a lowering that forgets a
  `ConvertFrom`; it is intentionally kept even though correct lowering never triggers it.
- A future LLVM backend renders the same MIR assignment as an explicit convert followed by a store;
  the C++ backend's `ConvertFrom` + same-shape `AssignFrom` split is that backend's lowering of the
  identical semantics, consistent with `integral-representation.md` F6.
