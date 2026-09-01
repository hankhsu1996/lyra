# Unified `PackedArray` Class for Integral Types in `backend::cpp`

## Date

2026-05-24

## Status

Accepted

## Why this decision matters

`backend::cpp` had multiple silent-correctness bugs in how integral types were emitted, and a prior
attempt at a five-bucket fix (native `int{8,16,32,64}_t` plus `BitValue`/`LogicValue` for the wide
path) made the dispatch more complex without removing the root problem: cpp emit was splitting one
slang concept into multiple C++ types and then needing bridges to glue them back together. This
record captures the findings that pushed us to a single-class design and the trade-offs we accepted.

## Findings that shaped the design

### F1. Slang represents every integral as one base class with three attributes

`slang/ast/types/AllTypes.h`:

```cpp
class IntegralType : public Type {
  bitwidth_t bitWidth;
  bool isSigned;
  bool isFourState;
};
```

`ScalarType`, `PredefinedIntegerType`, `PackedArrayType`, `EnumType`, `PackedStructType`,
`PackedUnionType` all derive from `IntegralType`. From slang's perspective, "integral" is one
concept defined by `(bitWidth, isSigned, isFourState)`. Subclass identity is bookkeeping for
syntactic origin (`int` vs `bit signed [31:0]`), not a semantic distinction.

**Consequence:** a lyra backend that wants to be honest about integral semantics has exactly three
attributes to dispatch on. Any further split is the backend's own invention.

### F2. An earlier tree mirrored slang directly

Its integral type carried exactly the same three attributes:

```cpp
struct IntegralInfo {
  uint32_t bit_width;
  bool is_signed;
  bool is_four_state;
};
```

`Type::Integral(bit_width, is_signed, is_four_state)` was the single factory; `AsIntegral()`
returned the three-field info. The legacy LLVM backend then bucketed storage at LLVM IR level
(`i8 / i16 / i32 / i64 / iN`) because that is what LLVM types provide. The bucketing lived in the
IR-emit step, not in the type model.

**Consequence:** the legacy team treated bucketing as a per-backend lowering detail, not a property
of the integral type itself. A new C++ backend should follow the same shape.

### F3. Current MIR `PackedArrayType` is already unified semantically

```cpp
struct PackedArrayType {
  IntegralStateKind state_kind;
  Signedness signedness;
  std::vector<PackedRange> dims;
};
```

`(state_kind, signedness, dims)` carries exactly the three slang attributes (`state_kind` ->
`is_four_state`, `signedness` -> `is_signed`, `BitWidth()` from `dims`). A backend dispatching on
the syntactic origin would be dispatching on something with no semantic content.

**Consequence:** no IR records which keyword a predefined-width integer was written with. LRM 6.11.1
makes `int` and `bit signed [31:0]` one type, so they reach one identity in every layer, and a
consumer asking what an integral value is has exactly the three attributes to ask about.

### F4. The five-bucket split was a `backend::cpp` invention with a real cost

An earlier cut emitted `int{8,16,32,64}_t` for narrow 2-state predefined types,
`BitValue`/`LogicValue` for wide and 4-state, and stitched the two worlds together with
`MakeBitView` / `BitViewToInt64` bridges. Each new operator family then had to be implemented in two
parallel forms (C++ tokens for native, view-based free functions for container), and every
`ConversionExpr` arm had to dispatch over a `(src_is_container?, dst_is_container?)` matrix.

The bridges are pure plumbing. They have no SV semantic content; they exist only because cpp emit
chose to represent one slang concept as two C++ shapes. The five-bucket layout was driven by a
premature optimisation -- "narrow `int` should emit as native `int32_t` to avoid runtime overhead"
-- that we are not yet measuring.

**Consequence:** the five-bucket dispatch is rejected. A single C++ class carries all integrals.

### F5. Internal optimisation is still available without exposing the buckets

The hot path for narrow common cases (e.g., `int x = a + b;`) can still be fast inside a unified
class:

- Storage uses an inline word for `bit_width <= 64` (no heap allocation), heap storage for wider.
  This is the small-buffer pattern `BitValue` already implements.
- Operator overloads are `inline` in the header; the compiler sees the body at every call site and
  can fold dispatch checks when the bit_width is a compile-time constant at the construction.
- If profiling later shows narrow-int overhead is a bottleneck, a specialised `PackedArrayNarrow`
  subclass or constexpr fast path can be added without changing the emit surface. That is
  profile-driven, not first-cut design.

**Consequence:** narrow performance is an internal concern of the value layer. The cpp emit surface
stays simple.

### F6. The unified shape aligns with the future LLVM backend

A future LLVM backend will consume the same MIR. Its natural type for any integral is `iN`. By
keeping cpp emit's surface to a single `PackedArray` (with internal lowering to native ints or
multi-word storage as a private detail), both backends share the same semantic boundary. The
five-bucket choice is something the C++ backend may rediscover internally as a private optimisation,
but it does not leak into MIR or into a hypothetical IR-level API.

## Decision

`backend::cpp` and `lyra::value` follow these invariants:

1. **One C++ class for every integral type**: `lyra::value::PackedArray`, carrying its own declared
   representation. No separate `BitValue` / `LogicValue` / native int types appear in emitted code
   for integral variables.
2. **MIR dispatch axis is `(state_kind, bit_width, signedness)`**. Nothing else about an integral
   type is available to dispatch on, in any backend.
3. **Operations live on `PackedArray`**: operator overloads and member functions (or free functions
   in the same namespace). No view-based helpers exposed to emit. Storage / view abstractions remain
   internal implementation details.
4. **Internal storage chooses inline-versus-heap by `bit_width` at construction**: `<= 64` is inline
   (no heap allocation), `> 64` spills to heap. 4-state types carry a same-width unknown plane. No
   template on `bit_width`.
5. **No native scalar / view bridges in emit**: `MakeBitView`, `BitViewToInt64`, and related helpers
   from earlier cuts are not used and not introduced.
6. **MIR remains backend-agnostic**: no cpp-emit-specific shape leaks into MIR.

## Consequences

- `byte`, `shortint`, `longint`, `time`, `integer`, every `bit [N:0]` / `logic [N:0]`, narrow and
  wide, 2-state and 4-state, all emit as the same C++ type.
- `ConversionExpr` handling collapses to a single case: same-class assignment / construction with
  the destination's `(bit_width, is_signed, is_four_state)` driving sign-extension and truncation
  inside `PackedArray`.
- Operator coverage grows by adding methods or overloads to `PackedArray`, not by branching in emit
  code.
- `lyra::value::BitValue` / `LogicValue` are absorbed into `PackedArray`'s internal storage. Their
  existing view-based helpers (`BitwiseAnd`, `ReductionOr`, etc.) become implementation details
  called from `PackedArray` member functions, not directly emitted.
- Narrow runtime variables (e.g., `int x;`) carry a `PackedArray`-sized object (~24-32 bytes)
  instead of a 4-byte `int32_t`. This is accepted as a clean-design trade and reconsidered only if
  profiling identifies it as a real cost.
- The LLVM backend reads the same MIR on the same dispatch axis; its bucketing to `iN` is its own
  lowering.

## Cross-references

- [unpacked-range-belongs-to-type](unpacked-range-belongs-to-type.md) -- its packed carve-out
  settles what a packed value's declared representation is: the dimension stack rather than a bare
  bit width, because the stack's product is the storage width and the whole stack decides whether
  two values share a representation.
- [packed-array-representation](packed-array-representation.md) -- the recursive HIR packed type
  that HIR-to-MIR flattens onto the MIR shape this decision keeps.
