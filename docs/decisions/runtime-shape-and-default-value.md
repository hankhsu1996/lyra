# Packed Array runtime shape and collection default value

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
- Earlier iterations of the runtime papered this over with a `MakeDefaultLike(data_.front())` oracle
  that consults an existing element for shape -- correct only when the container is non-empty, and
  only by convention rather than enforcement.

Two questions surface:

1. Is the runtime-shape representation still the right choice, or should shape be lifted to the C++
   type level?
2. If runtime-shape stays, how should a container synthesize a shape-correct default value without
   depending on an existing instance?

## Decision

1. **Runtime shape representation is retained on `PackedArray`.** SystemVerilog permits arbitrary
   user-defined integer widths (`bit [W:0]` for any `W`), arbitrary packed struct definitions,
   packed unions, and enums. A type-templated representation would produce one distinct C++ type per
   unique shape and one distinct operator template instantiation per cross-shape operand pair. The
   cardinality grows with design complexity in a way that makes compile time, binary size, and
   operator-overload growth infeasible at scale. Runtime-shape is the correct engineering trade-off
   given this cardinality property.

2. **Every container wrapper carries an explicit `default_value_` field of type `T`.** For wrappers
   whose element type `T` requires runtime construction parameters (`PackedArray`, packed struct,
   packed union, multi-dim packed), the wrapper stores one `T` instance as a `default_value_`
   member. The member is set at construction and persists for the wrapper's lifetime. OOB reads,
   resize-fill operations, and empty- container synthesis all consult `default_value_`. This
   restores the shape information that the C++ template-parameter boundary discards.

3. **Container construction API aligns with `std::vector(n, value)`.** The default value is a
   required constructor argument, not optional. Wrappers offer a one-argument form for the
   declare-empty case (`Wrapper(default_value)`), a two-argument form for sized construction
   (`Wrapper(n, default_value)`), and additional forms as element semantics require. The `value`
   parameter is stored as `default_value_` rather than consumed during the fill, so the same value
   is available for later resize and OOB paths.

4. **The default value flows from MIR lowering.** `SynthesizeDefaultValueExpr` (HIR-to-MIR) already
   produces shape-correct default expressions from MIR type information. The emit chain becomes: MIR
   type -> default-value expression -> wrapper-constructor argument -> stored `default_value_`
   member. The shape information that is "lost" at the C++ template-parameter level is restored via
   this explicit channel; no separate channel is introduced.

_Rejected alternatives:_

- **Template `PackedArray` on shape.** Distinct C++ types per shape, distinct operator template
  instantiations per cross-shape pair. Cardinality grows with design complexity; not viable for
  compile time, binary size, or operator-overload growth.

- **Store a callable (`std::function<T()>` or lambda) instead of a `T` instance.** Type-erasure
  overhead, no shape introspection from the stored callable, idiomatic mismatch with the
  `std::vector(n, value)` API that the rest of the wrapper mirrors.

- **`MakeDefaultLike(data_.front())` oracle.** Breaks on empty containers; depends on the "first
  element survives" convention; does not enforce shape uniformity across elements.

- **Lift shape from `PackedArray` to the container.** Breaks `PackedArray`'s encapsulation; element
  instances would lose self-description; every operation handling an individual element would
  require shape passed in alongside the value.

- **Throw on empty-container OOB read.** Violates LRM 7.4.5, which mandates the element type's Table
  7-1 default.

## Consequences

- Every container wrapper carries one extra `T` member. The size cost is one element-sized object
  per wrapper, independent of element count.

- All container construction sites -- emit-generated and direct C++ -- must provide `default_value`
  explicitly. Container wrappers no longer expose a zero-argument default constructor when `T` has
  runtime construction parameters.

- The `MakeDefaultLike` overload set and the `data_.front()` oracle pattern are removed. Wrappers do
  not consult element storage to recover shape.

- The pattern extends naturally to queue, associative array, packed-struct collection,
  unpacked-struct collection, and any future wrapper whose element type requires runtime
  construction parameters. Each new wrapper introduces a `default_value_` member following the same
  shape.

- Shape uniformity within a collection is enforced by convention -- lowering supplies the same
  `default_value` for all writes and never mixes shapes in one container -- not by the C++ type
  system. This is an accepted limitation of the runtime-shape choice.

## Cross-references

- LRM 7.4.5 (Indexing and slicing of arrays), 7.4.6 (Operations on arrays), Table 6-7 (Default
  initial values), Table 7-1 (Value read from a nonexistent array entry).
- `src/lyra/lowering/hir_to_mir/default_value.cpp` -- `SynthesizeDefaultValueExpr`.
- `include/lyra/value/packed_array.hpp` -- runtime-shape `PackedArray` definition.
- `include/lyra/value/dynamic_array.hpp`, `include/lyra/value/unpacked_array.hpp` -- first wrappers
  to adopt the `default_value_` pattern.
- `docs/progress/aggregate.md` -- variable-size aggregate workstream.
