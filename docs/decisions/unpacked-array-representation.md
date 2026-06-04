# Unpacked Array Representation

## Date

2026-06-02 (revised; original 2026-06-01)

## Status

Accepted

## Why this decision matters

SystemVerilog has multiple array families. The packed family is handled by
[integral-representation.md](integral-representation.md). This document handles the fixed-size
unpacked family -- the gating type for the `datatypes/unpacked` archive item, and the foundation for
the variable-size container types covered by `datatypes/general` (dynamic array, queue, associative
array).

The choices here bind every layer the unpacked surface touches:

- HIR / MIR type shape
- Backend cpp storage type and default-initialization emit
- Expression form carrying the `'{...}` assignment pattern

Subsequent unpacked-family work -- element write, whole-array assignment, slicing, OOB rules,
element-level observability -- extends the surface this document fixes; this record names the
invariants those steps inherit.

## Findings that shaped the design

### F1. LRM defines unpacked multi-dim as "array of arrays"

`LRM 7.4.4`:

> A multidimensional array is an array of arrays. Multidimensional arrays can be declared by
> including multiple dimensions in a single declaration.

`LRM 7.4.4` reinforces the recursive shape via the subarray definition: "A subarray is an array that
is an element of another array."

slang's frontend mirrors the LRM definition. For `int a [4][8]` slang produces a
`FixedSizeUnpackedArrayType` whose `elementType` is itself another `FixedSizeUnpackedArrayType` --
nesting, one level per declared dimension.

**Consequence:** any HIR shape that flattens the unpacked dim stack onto a single node contradicts
the LRM model that HIR is required to preserve (`hir.md` invariant 3). The LRM-faithful shape is
single-dim per type, nested via the element type.

### F2. The packed / unpacked asymmetry comes from element substrate

Packed and unpacked arrays differ in what their elements are made of:

- A packed array's elements are bits. Storage is a flat bit plane; multi-dim is offset arithmetic on
  the same plane. `PackedArray` carries the whole dim stack as `dims: vector<PackedRange>` because
  the substrate is uniform and flat.
- An unpacked array's elements are full data values of arbitrary type: integral (which becomes a
  `PackedArray` instance), real, string, packed struct, or another unpacked array. The substrate is
  heterogeneous in size. There is no flat byte representation that covers every element kind.

**Consequence:** unpacked storage is inherently per-element, and the type shape that drives storage
emit is recursive at every layer. PackedArray's flat-dims shape does not generalise.

### F3. C++ has no standalone array literal; multi-dim only nests

C++ braced-init-list `{...}` is context-typed. Free-standing use yields `std::initializer_list<T>`,
not the destination container type. The only context-free typed form is explicit construction:
`T{...}`.

C++ has no multi-dimensional container type. Multi-dim is always `std::vector<std::vector<T>>` or
`std::array<std::array<T, M>, N>` -- one layer of container type per declared dimension.

**Consequence:** for any IR-to-cpp rendering, one IR type layer maps to one C++ container layer.
Nested IR keeps this mapping straight at every depth; flat IR would force backend cpp to peel a dim
list into nested containers at render time, with no upside.

### F4. Element-aware defaults force ctor-args, not zero-init

`PackedArray`'s default constructor produces a zero-width instance -- not a valid element value for
any SystemVerilog integral type. Element-type-aware default initialization requires the element
type's constructor arguments (e.g. `lyra::value::PackedArray(32, true, false)` for `int`,
`lyra::value::PackedArray(8, false, true)` for `logic [7:0]`).

`std::vector<T>(N, T(args...))` matches this pattern: each of the `N` slots adopts the supplied
prototype. `std::array<T, N>{}` with default-constructed elements does not, because the default ctor
cannot accept element-specific arguments.

**Consequence:** backend emit must know the element type's default ctor args at every depth, and the
storage form must accept a prototype value. The existing `RenderTypeDefaultCtorArgs` chain already
produces these args for packed types; the unpacked path reuses the chain recursively.

### F5. `'{...}` is intrinsically expression-shaped

`LRM 10.9.1`:

> An assignment pattern of the form '{expression, expression, ...} is not self-determined; it shall
> be used in a context in which the type of the assignment is determined from outside the pattern.

slang lowers this form to `SimpleAssignmentPatternExpression`, a member of its expression hierarchy.
The pattern's type is resolved during AST construction from the destination context; by the time HIR
consumes the AST, the expression is fully typed.

The same `'{...}` shape appears in two contexts: declaration initializer (`int arr[3] = '{1,2,3};`)
and procedural assignment (`arr = '{1,2,3};`). They differ only in where the destination type comes
from -- the declaration or the LHS.

**Consequence:** ArrayLiteral lives as a first-class IR expression, not as an init-only `Decl`
variant. The init-only shape would force a parallel introduce-now-rewrite-later cut when procedural
array assignment lands.

## The decision

Unpacked array support follows these invariants:

1. **HIR and MIR represent unpacked arrays with a single dim per type node, nested via element
   type.**

   ```text
   hir::UnpackedArrayType { dim: UnpackedRange, element_type: TypeId }
   mir::UnpackedArrayType { dim: UnpackedRange, element_type: TypeId }
   ```

   Multi-dim `int a[4][8]` becomes an outer `UnpackedArrayType` whose `element_type` resolves to an
   inner `UnpackedArrayType` whose element is `int`. One IR layer per declared dimension. AST -> HIR
   consumes slang's nested `FixedSizeUnpackedArrayType` 1:1.

2. **Backend cpp renders each unpacked type layer as `lyra::value::UnpackedArray<T>`.**

   `mir::UnpackedArrayType { dim, element_type }` -> `lyra::value::UnpackedArray<` +
   `render(element_type)` + `>`. The wrapper owns a private `std::vector<T>` and exposes a surface
   symmetric with `PackedArray` -- `ElementAt(const PackedArray&)`, `Slice(offset, count)`,
   `operator==` and `CaseEqual` returning a 1-bit `PackedArray` -- so the substrate-asymmetric
   operations (equality with X / Z propagation, range selectors, observability hooks) live behind a
   single uniform method surface. LRM 7.4.5 invalid-index handling lives in the wrapper too:
   `ElementAt` returns `T&` (or `const T&` from the const overload); on an invalid index the
   returned reference is to the wrapper's `oob_slot_` shield, restored to canonical state via
   `T::ResetToDefault` immediately before being handed out so any prior OOB write is erased.
   Element-level compound semantics live on `PackedArray` directly -- the wrapper carries no per-T
   forwarder. `Slice` (non-const) returns an `UnpackedArrayRef<T>` proxy for slice writeback; slice
   reads on partially-OOB positions handle the in-range and OOB elements per-element rather than at
   slice granularity, matching `PackedArray::ExtractBits` / `AssignSlice`'s per-bit OOB treatment.
   See `docs/decisions/runtime-shape-and-default-value.md` for the shield contract.

3. **Default initialization emits a `ConstructExpr` whose first positional argument is the
   canonical-default element and whose second is an `ArrayLiteralExpr` rendered as
   `std::array<T, N>{...}`.**

   `default_value.cpp` synthesises an `ArrayLiteralExpr` populated with per-element defaults; the
   backend wraps it in `ConstructExpr` so the wrapper's `(T canonical_default, std::span<const T>)`
   constructor receives both the canonical-default seed for `oob_slot_` and the initial elements.
   The `std::array<T, N>` rvalue produced by the element list implicitly converts to the ctor's
   `std::span<const T>` parameter. For `int arr[3]`:

   ```cpp
   lyra::value::UnpackedArray<lyra::value::PackedArray> arr(
       lyra::value::PackedArray::Int(0),
       std::array<lyra::value::PackedArray, 3>{
           lyra::value::PackedArray::Int(0),
           lyra::value::PackedArray::Int(0),
           lyra::value::PackedArray::Int(0)});
   ```

   Multi-dim composes through the nested element type with the same shape at every layer. The same
   element-list shape applies to `DynamicArray<T>` -- the only thing that varies is the outer
   container type.

4. **`'{...}` is a first-class IR expression -- `hir::ArrayLiteral` / `mir::ArrayLiteral` --
   rendered as `std::array<T, N>{...}`.**

   ```text
   hir::ArrayLiteral { elements: vector<ExprId> }
   mir::ArrayLiteral { elements: vector<ExprId> }
   ```

   The expression's resolved type comes through the `Expr`-common `type` field (the container type;
   the element type is read off it at render time). ArrayLiteral joins concat and replication as a
   value-build primitive at MIR, per `mir.md`. Backend cpp always renders as
   `std::array<elem, N>{...}` -- a context-independent rendering whose value implicitly converts to
   the wrapper ctor's `std::span<const T>` parameter:

   ```cpp
   std::array<lyra::value::PackedArray, 3>{
       lyra::value::PackedArray::Int(10),
       lyra::value::PackedArray::Int(20),
       lyra::value::PackedArray::Int(30)}
   ```

## Consequences

Closed under this decision:

- The AST -> HIR gate at `kUnsupportedFixedSizeUnpackedArrayType` opens. Nested HIR types are
  produced 1:1 from slang's nested AST types.
- `UnpackedArrayType.dims: vector<UnpackedRange>` collapses to a single `dim: UnpackedRange` field
  at both HIR and MIR.
- Backend cpp gains unpacked type rendering, unpacked default-init emit, and unpacked literal-init
  emit. Element-read on unpacked bases routes through
  `UnpackedArray<T>::ElementAt(const PackedArray&)`, symmetric with `PackedArray::ElementAt`.
- 1D and multi-dim fall out at every layer from the recursive shape -- no separate code path per dim
  count.

Out of scope but unblocked by this design:

- Variable-size container types (dynamic array, queue, associative array). They share the wrapper's
  `std::vector` storage spine and extend it with size-mutation semantics.
- Element-level observability for whole-array `Var<>` wrapping. The wrapper's `operator==` already
  returns a 1-bit `PackedArray` (X / Z propagating); observability sits on the same surface when
  `refactor.md` R2 lands.

## Alternatives considered

**Flat dim list on a single unpacked type node.** Rejected. `LRM 7.4.4` explicitly defines multi-dim
unpacked as "array of arrays". A flat list contradicts the LRM model HIR is required to preserve.
C++ has no multi-dim vector type, so backend cpp would need to peel the dim list into nested
`std::vector` at every render. The asymmetry vs PackedArray (flat-dims) is intentional and follows
the substrate: bits are uniform and support flat storage; unpacked elements are heterogeneous and do
not.

**`std::array<T, N>` instead of `std::vector<T>`.** Rejected. Threading `N` into the cpp type string
complicates rendering and breaks the symmetry with the variable-size container path
(`mir::VectorType` already renders to `std::vector<T>`). Using `std::array` for fixed-size and
`std::vector` for dynamic / queue / associative would force every container operation to dispatch on
container kind. Stack-vs-heap allocation is not a measured concern in the simulator; a single heap
alloc per declared unpacked array is acceptable.

**`std::vector<T>` substrate without a wrapper class.** Originally chosen for the U1 scope, on the
grounds that the standard container already provided `operator[]`, list-init, default ctor, deep
copy, and equality. Reopened once U5 (aggregate equality with X / Z propagation) and U6 (slice
returning a value or a write-back proxy) demonstrated that the substrate kept leaking asymmetries
into every new operation: `std::vector::operator==` returns `bool` so an out-of-class
`AggregateEqual` overload set had to fan in by ADL, and `std::vector` cannot host a `Slice` method
to mirror `PackedArray::Slice` because the type is `std`-owned. Wrapping the substrate in
`UnpackedArray<T>` (private `std::vector<T>` storage; public method surface symmetric with
`PackedArray`) collapses both leaks into one place and gives the backend a single uniform call shape
for both substrates. The original rejection's "no caller exercises it" rationale was scoped to the
U1 work-list; subsequent unpacked sub-steps reversed that condition.

**ArrayLiteral as an init-only `Decl` variant rather than a first-class expression.** Rejected.
slang's AST already shapes `'{...}` as an Expression (`SimpleAssignmentPatternExpression`).
Procedural array assignment (`arr = '{1,2,3}`) requires the literal in RHS position; an init-only
form would force a parallel rewrite when that lands. ConcatExpr and ReplicationExpr are first-class
expressions despite also being context-typed -- ArrayLiteral is in the same family per `mir.md`.

**Element list as `std::initializer_list<T>` with a backend special-case that strips the type prefix
when the literal appears inside a `ConstructExpr` argument slot.** Rejected. The backend rendering
becomes context-dependent (the same MIR primitive renders differently as a standalone expression vs
as a `ConstructExpr` argument), which couples `RenderArrayLiteralExpr` and `RenderConstructExpr`.
`std::span<const T>` with `std::array<T, N>{...}` at the call site keeps rendering uniform:
`ArrayLiteralExpr` always emits `std::array<T, N>{...}`, `std::array` implicitly converts to
`std::span<const T>`, and no parent-context inspection is needed. The cost is a slightly longer emit
string, which has no readability cost for emitted code.
