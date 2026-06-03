# Packed Array runtime shape and collection OOB shield

Date: 2026-06-03 Status: accepted

## Context

The runtime represents every SystemVerilog integer-family value (`int`, `byte`, `bit [W:0]`,
`logic [W:0]`, packed struct, packed union, enum) as a single C++ class `PackedArray` with three
runtime-mutable shape fields: bit width, signedness, and state kind (2-state vs 4-state). Shape
lives on each instance, not on the C++ type.

This is well-suited to the standalone-value case (e.g., `int x;`): one class handles every
integer-family declaration, and every instance is fully self-describing. It is less well-suited to
the collection case (e.g., `int arr[]`), where the SystemVerilog semantic invariant is that every
element of a given array shares the same shape. The collection sees friction:

- An empty container has no instances to query, so a generic OOB read has no shape source for the
  LRM 7.4.5 default value.
- The C++ type `DynamicArray<PackedArray>` cannot express the SystemVerilog "all elements share one
  shape" invariant.
- LRM 7.4.5 requires both OOB read (returns LRM Table 7-1 default) and OOB write (silent no-op) to
  behave correctly; neither has a natural home on `PackedArray` itself.

Three questions surface:

1. Is the runtime-shape representation still the right choice, or should shape be lifted to the C++
   type level?
2. If runtime-shape stays, how should a container synthesize a shape-correct default value without
   depending on an existing instance?
3. How should the LRM 7.4.5 OOB write-is-no-op semantic be expressed -- on the container, on the
   element value, or at the codegen boundary?

## Decision

1. **Runtime shape representation is retained on `PackedArray`.** SystemVerilog permits arbitrary
   user-defined integer widths (`bit [W:0]` for any `W`), arbitrary packed struct definitions,
   packed unions, and enums. A type-templated representation would produce one distinct C++ type per
   unique shape and one distinct operator template instantiation per cross-shape operand pair. The
   cardinality grows with design complexity in a way that makes compile time, binary size, and
   operator-overload growth infeasible at scale. Runtime-shape is the correct engineering trade-off
   given this cardinality property.

2. **Every container wrapper carries one `oob_slot_` field of type `T`, declared `mutable`.** The
   slot is seeded to the canonical default at construction (so initial OOB reads return the LRM
   Table 7-1 default) and doubles as the discard target for OOB writes (so the proxy / forwarding
   layer the wrapper would otherwise need vanishes). The wrapper restores the slot to canonical
   state via `T::ResetToDefault` on every OOB access before handing out a reference, so any write
   accumulated in the slot is erased before the next access observes it. Single field, single
   responsibility: "the LRM 7.4.5 shield for this container."

3. **Every value type that can be an element provides `ResetToDefault()`.** `PackedArray` in-place
   zero-fills (2-state) or X-fills (4-state) the data bits while preserving shape; `DynamicArray<T>`
   clears `data_`; `UnpackedArray<T>` recurses into each element. The recursive case for fixed-size
   unpacked is O(N) and is the LRM-mandated cost of "Array, all of whose elements have the value
   specified ... for that array's element type" -- there is no way to express that default cheaper
   than initializing N elements.

4. **`ElementAt` returns `T&` directly; element-level proxy classes are absent.** Compound semantics
   (`+=`, `&=`, the shift-assign family, the increment / decrement pair) live on `PackedArray` where
   they belong; the container layer never reimplements them. OOB read is a reference to the shield
   slot in canonical state, OOB write mutates the shield slot which is then reset on the next
   access. The non-const overload returns `T&` for the write path; the const overload returns
   `const T&` and is permitted by the `mutable` qualifier on `oob_slot_`.

5. **Container construction API aligns with `std::vector(n, value)`.** The canonical default is a
   required constructor argument, not optional. Wrappers offer a one-argument form for the
   declare-empty case (`Wrapper(canonical_default)`), a two-argument form for sized construction
   (`Wrapper(n, canonical_default)`), and additional forms as element semantics require. The
   parameter is stored as `oob_slot_` rather than consumed during the fill, so the same value is
   available for later resize and OOB paths.

6. **The canonical default flows from MIR lowering.** `SynthesizeDefaultValueExpr` (HIR-to-MIR)
   already produces shape-correct default expressions from MIR type information. The emit chain is:
   MIR type -> default-value expression -> wrapper-constructor argument -> stored `oob_slot_`
   member. The shape information that is "lost" at the C++ template-parameter level is restored via
   this explicit channel; no separate channel is introduced.

_Rejected alternatives:_

- **Template `PackedArray` on shape.** Distinct C++ types per shape, distinct operator template
  instantiations per cross-shape pair. Cardinality grows with design complexity; not viable for
  compile time, binary size, or operator-overload growth.

- **Per-container element-ref proxy (`UnpackedElementRef<T>`, `DynamicElementRef<T>`) with `valid_`
  flag and forwarded compound operators.** Mixes layer responsibilities -- the container's OOB
  concern leaks into a per-T forwarder set that duplicates `PackedArray`'s compound operators on
  every container type and grows by ~11 method definitions per new container. Replaced by direct
  `T&` return + shield slot.

- **Two separate fields: `default_value_` (immutable canonical) plus `scratch_` (mutable write
  target).** Twice the per-wrapper memory cost for no observable behavior difference -- the
  canonical can be recovered from the shield via `ResetToDefault` whenever needed.

- **Store a callable (`std::function<T()>` or lambda) instead of a `T` instance.** Type-erasure
  overhead, no shape introspection from the stored callable, idiomatic mismatch with the
  `std::vector(n, value)` API that the rest of the wrapper mirrors.

- **`MakeDefaultLike(data_.front())` oracle.** Breaks on empty containers; depends on the "first
  element survives" convention; does not enforce shape uniformity across elements.

- **Lift shape from `PackedArray` to the container.** Breaks `PackedArray`'s encapsulation; element
  instances would lose self-description; every operation handling an individual element would
  require shape passed in alongside the value.

- **Emit-side validity gate (`if (in_bounds(idx)) arr.RawAt(idx) op= rhs;` instead of returning
  `T&`).** Pushes LRM 7.4.5 semantics into every codegen site, complicates NBA capture (validity has
  to be re-derived at fire time), and explodes for multi-dim chains. The shield slot keeps codegen
  uniform across in-bounds and OOB.

- **Throw on empty-container OOB read.** Violates LRM 7.4.5, which mandates the element type's Table
  7-1 default.

## Consequences

- Every container wrapper carries one extra `T` member (`oob_slot_`). The size cost is one
  element-sized object per wrapper, independent of element count.

- Every value type usable as a container element implements `ResetToDefault()`. The contract is
  in-place restoration to the LRM Table 6-7 / Table 7-1 canonical default while preserving any shape
  information embedded in the value (bit width / signedness / state kind for `PackedArray`).

- `UnpackedArray<T>::ResetToDefault` is O(N) where N is the array's fixed size; this cost is paid
  only on OOB access to the outer container and is the LRM-mandated cost of materializing "all
  elements at default." In-bounds access pays no shield cost.

- All container construction sites -- emit-generated and direct C++ -- must provide
  `canonical_default` explicitly. Container wrappers no longer expose a zero-argument default
  constructor when `T` has runtime construction parameters.

- The `MakeDefaultLike` overload set, the `data_.front()` oracle pattern, and the
  `UnpackedElementRef` / `DynamicElementRef` proxy classes are removed.

- The pattern extends naturally to queue, associative array, packed-struct collection,
  unpacked-struct collection, and any future wrapper whose element type requires runtime
  construction parameters. Each new wrapper introduces an `oob_slot_` member following the same
  shape and implements its own `ResetToDefault`. Associative array's "auto-allocate on write" rule
  (LRM 7.8.7) is a different ElementAt contract and requires its own design when that workstream
  opens.

- Shape uniformity within a collection is enforced by convention -- lowering supplies the same
  canonical default for all writes and never mixes shapes in one container -- not by the C++ type
  system. This is an accepted limitation of the runtime-shape choice.

## Cross-references

- LRM 7.4.5 (Indexing and slicing of arrays), 7.4.6 (Operations on arrays), Table 6-7 (Default
  initial values), Table 7-1 (Value read from a nonexistent array entry).
- `src/lyra/lowering/hir_to_mir/default_value.cpp` -- `SynthesizeDefaultValueExpr`.
- `include/lyra/value/packed_array.hpp` -- runtime-shape `PackedArray` definition and
  `ResetToDefault`.
- `include/lyra/value/dynamic_array.hpp`, `include/lyra/value/unpacked_array.hpp` -- first wrappers
  to adopt the shield pattern.
- `docs/progress/aggregate.md` -- variable-size aggregate workstream.
