# Packed Array runtime shape and collection default value

Date: 2026-06-03 (revised 2026-06-22, 2026-09-28) Status: accepted; decision 1 (shape stays a
runtime property of the value) superseded 2026-10-04 by
[a-value-is-its-machine-data](a-value-is-its-machine-data.md), which lifts width, signedness and
state domain to the type -- the question this record's first question asked and answered the other
way. Under [a-packed-value-is-its-words](a-packed-value-is-its-words.md) the one runtime class the
Context and decision 1 describe does not exist: each packed type is a value type of its own, no
instance carries a shape, and the rejected template over shape is what was built.

Decisions 2, 5 and 6 are superseded 2026-10-11 by
[an-arrays-element-default-is-its-element-types](an-arrays-element-default-is-its-element-types.md),
for every array kind. They were derived under decision 1: with shape a run-time property, an empty
container had nothing to ask for its element's shape, so each was handed a default and kept it. With
shape the type's, the element type states its default once, and no array of any kind is handed one
or keeps one. What stands: decision 4, that an element is reached as a reference to it with no proxy
class and that an invalid read and an invalid write are answered with two different things, a value
no write reaches and storage no read reaches (LRM 7.4.5); and the rejections of an element-reference
proxy, of one slot serving both, of an oracle read off the first element, of a validity gate at
every access, and of a failure on an empty container. Each decision below says where it is
superseded.

The 2026-06-22 revision splits the container's single `mutable` default slot into two fields -- an
immutable canonical default and a separate write-discard sink -- so the read path is genuinely
`const`. Decision points 2 and 4 and the rejected-alternatives list are updated accordingly.

The 2026-09-28 revision withdraws point 3. Each value type restoring its own default in place stated
every type's default a second time, beside the one point 6 already carries from MIR, and for a
structure it was the library composing a whole-value operation
([a-structures-operations-are-stated-in-mir](a-structures-operations-are-stated-in-mir.md)). The
sink is now restored by copying `element_default_`, which carries the shape by the same channel.

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

2. **Every container wrapper carries two element-typed fields: an immutable `element_default_` and a
   scratch `discard_sink_`.** Both are seeded to the canonical default at construction.
   `element_default_` is the LRM Table 7-1 value a read of a nonexistent / invalid entry returns and
   the prototype every fill / grow / slice / locator copies; it is only ever read or copied, never
   written, so it stays canonical for the container's life and a const read returns it by direct
   reference with no scrub. `discard_sink_` is the throwaway an invalid write lands on; the
   non-const write path restores it by copying `element_default_` before handing out the reference,
   so a discarded write never leaks into a later access. The two fields are two distinct
   responsibilities -- the canonical default value versus the throwaway write target -- not a
   duplicated value. _(Superseded 2026-10-11: the two responsibilities stand and no container
   carries either. Both are kept once for the element's type, the read's never written and the
   write's restored each time it is handed out.)_

3. Withdrawn 2026-09-28; see the note under the title.

4. **The positional element accessor returns `T&` directly; element-level proxy classes are
   absent.** Compound semantics (`+=`, `&=`, the shift-assign family, the increment / decrement
   pair) live on `PackedArray` where they belong; the container layer never reimplements them. An
   invalid read returns `const T&` to `element_default_`; an invalid write returns `T&` to the
   scrubbed `discard_sink_`. Because the read path never touches the sink and the write path is
   non-const, no field needs the `mutable` qualifier -- the const read is genuinely const.

5. **Container construction API aligns with `std::vector(n, value)`.** The canonical default is a
   required constructor argument, not optional. Wrappers offer a one-argument form for the
   declare-empty case (`Wrapper(canonical_default)`), a two-argument form for sized construction
   (`Wrapper(n, canonical_default)`), and additional forms as element semantics require. The
   parameter seeds both `element_default_` and `discard_sink_` rather than being consumed during the
   fill, so the canonical default is available for later resize and miss paths. _(Superseded
   2026-10-11: no construction takes a default. `std::vector(n, value)` is the form for a caller
   holding a value to fill with; a type that states its own start is `std::vector(n)`.)_

6. **The canonical default flows from MIR lowering.** HIR-to-MIR already produces shape-correct
   default expressions from MIR type information. The emit chain is: MIR type -> default-value
   expression -> wrapper-constructor argument -> stored `element_default_` / `discard_sink_`
   members. The shape information that is "lost" at the C++ template-parameter level is restored via
   this explicit channel; no separate channel is introduced. _(Superseded 2026-10-11: the default is
   still made from the source's type by the lowering, and that is all that stands. It is stated once
   for the element type, and nothing is lost at the template parameter for a channel to restore.)_

_Rejected alternatives:_

- **Template `PackedArray` on shape.** Distinct C++ types per shape, distinct operator template
  instantiations per cross-shape pair. Cardinality grows with design complexity; not viable for
  compile time, binary size, or operator-overload growth.

- **Per-container element-ref proxy (`UnpackedElementRef<T>`, `DynamicElementRef<T>`) with `valid_`
  flag and forwarded compound operators.** Mixes layer responsibilities -- the container's OOB
  concern leaks into a per-T forwarder set that duplicates `PackedArray`'s compound operators on
  every container type and grows by ~11 method definitions per new container. Replaced by direct
  `T&` return + the default / sink fields.

- **A single `mutable` slot fusing the canonical default and the write-discard sink.** Saves one
  element-sized field per wrapper, but because the write path dirties the shared slot the const read
  must scrub it before returning -- a const method that mutates, which forces the `mutable`
  qualifier and pays a `ResetToDefault` on every read miss. The saved field is one `T` per container
  instance (not per element), negligible against the `std::vector` / `std::map` / `std::deque` each
  wrapper already holds; the const-correctness and the eliminated reset-on-read outweigh it. This
  was the original choice and is revised here.

- **Store a callable (`std::function<T()>` or lambda) instead of a `T` instance.** Type-erasure
  overhead, no shape introspection from the stored callable, idiomatic mismatch with the
  `std::vector(n, value)` API that the rest of the wrapper mirrors. _(Answered 2026-10-11 in
  [an-arrays-element-default-is-its-element-types](an-arrays-element-default-is-its-element-types.md):
  a function per type is the type's default constructor and not a callable per container.)_

- **`MakeDefaultLike(data_.front())` oracle.** Breaks on empty containers; depends on the "first
  element survives" convention; does not enforce shape uniformity across elements.

- **Lift shape from `PackedArray` to the container.** Breaks `PackedArray`'s encapsulation; element
  instances would lose self-description; every operation handling an individual element would
  require shape passed in alongside the value.

- **Emit-side validity gate (`if (in_bounds(idx)) arr.RawAt(idx) op= rhs;` instead of returning
  `T&`).** Pushes LRM 7.4.5 semantics into every codegen site, complicates NBA capture (validity has
  to be re-derived at fire time), and explodes for multi-dim chains. The default / sink fields keep
  codegen uniform across in-bounds and OOB.

- **Throw on empty-container OOB read.** Violates LRM 7.4.5, which mandates the element type's Table
  7-1 default.

## Consequences

These are the consequences of decisions 2, 5 and 6 as they were taken. The first, third, fifth and
sixth no longer hold: no container carries either member, no construction site provides a default,
and uniformity within a collection is the element type's.

- Every container wrapper carries two extra `T` members (`element_default_`, `discard_sink_`). The
  size cost is two element-sized objects per wrapper, independent of element count.

- Restoring the sink is a copy of `element_default_`, paid only on an invalid-index write; for a
  fixed-size unpacked array it is O(N), the LRM-mandated cost of materializing "all elements at
  default." In-bounds access and invalid reads pay nothing (the read returns the pristine
  `element_default_` directly).

- All container construction sites -- emit-generated and direct C++ -- must provide
  `canonical_default` explicitly. Container wrappers no longer expose a zero-argument default
  constructor when `T` has runtime construction parameters.

- The `MakeDefaultLike` overload set, the `data_.front()` oracle pattern, and the
  `UnpackedElementRef` / `DynamicElementRef` proxy classes are removed.

- The pattern extends naturally to queue, associative array, packed-struct collection,
  unpacked-struct collection, and any future wrapper whose element type requires runtime
  construction parameters. Each new wrapper introduces `element_default_` / `discard_sink_` members
  following the same shape. (Associative array carries the same immutable shape prototype, paired
  with the value a read of an absent index answers with, which a LRM 7.9.11 `default:` clause names
  and which is otherwise that prototype.) Associative array's "auto-allocate on write" rule (LRM
  7.8.7) is a different element-access contract and requires its own design when that workstream
  opens.

- Shape uniformity within a collection is enforced by convention -- lowering supplies the same
  canonical default for all writes and never mixes shapes in one container -- not by the C++ type
  system. This is an accepted limitation of the runtime-shape choice.

## Cross-references

- LRM 7.4.5 (Indexing and slicing of arrays), 7.4.6 (Operations on arrays), Table 6-7 (Default
  initial values), Table 7-1 (Value read from a nonexistent array entry).
- `src/lyra/lowering/hir_to_mir/default_value.cpp` -- default-expression synthesis.
- `include/lyra/value/dynamic_array.hpp`, `include/lyra/value/unpacked_array.hpp` -- first wrappers
  to adopt the default / sink pattern.
