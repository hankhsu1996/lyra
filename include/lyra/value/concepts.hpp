#pragma once

#include <concepts>
#include <optional>

// Runtime value-layer concept catalogue. Each concept names a contract that a
// `lyra::value::*` type claims via `static_assert(<Concept><T>)` in its own
// header, hard-pinning the signature shape at compile time so any future
// drift becomes a compile failure rather than a silent regression.
//
// Three contract families share this header because they are the same kind of
// artifact -- all live in `lyra::value`, all key off C++20 concept machinery,
// all are pinned the same way:
//
// - Storage mechanics: C++ relocation safety so the runtime can hold the
//   type inside STL containers (`std::vector` / `std::deque` / `std::map`).
// - SV value-type contracts (LRM 11.4 "Any" data row): the equality and
//   change-detection surface every value type supplies, plus per-row opt-ins
//   for case-equality, wildcard-equality, and ordering.
// - SV container-method contracts (LRM 7.x, 6.16): the integer-positioned
//   methods (size, slice, element access, etc.) shared across the array
//   families.

namespace lyra::value {

class PackedArray;

// C++ storage mechanics every runtime value type must satisfy so it can
// live in the STL containers the runtime stores it in (`std::vector` /
// `std::deque` / `std::map`) and survive their relocation (insert / erase /
// grow). `std::copyable` requires move- and copy-construction, move- and
// copy-assignment, and swappability -- and, per the standard library
// contract it builds on, a moved-from object that remains valid for
// assignment and destruction. That moved-from validity is the property that
// actually bites: a value type whose move leaves an unusable husk crashes
// inside container relocation.
//
// This is a C++-mechanics contract, NOT a statement about SystemVerilog
// assignment meaning. Every value type's assignment is an ordinary whole-value
// replacement; the SystemVerilog rule that a store keeps the destination's
// declared type lives at the variable cell and the store boundary, not in the
// value's assignment operator. The concept only guarantees the operations
// exist and relocation is safe.
template <typename T>
concept Storable = std::copyable<T>;

// The SV data type contract every value type realises (LRM Table 11-1 "Any"
// row). A type that satisfies this concept provides the universal equality
// operators plus the engine's bit-pattern change-detection predicate. This
// is the concept `lyra::runtime::Var<T>` requires, so wrapping a
// structural-var in observable storage gates on it.
template <typename T>
concept LyraValue = Storable<T> && requires(const T& a, const T& b) {
  // LRM 11.4.5 `==` / `!=` (Any data type). Uniform return type: every
  // value type yields a 1-bit `PackedArray`; 2-state types yield a 2-state
  // packed result. The uniform type lets lowering and emit treat every
  // equality call identically.
  { a == b } -> std::same_as<PackedArray>;
  { a != b } -> std::same_as<PackedArray>;
  // LRM 9.4.2 update event predicate: did the cell's bit-pattern change.
  // The engine's change-detection hook -- distinct from the LRM `===`
  // operator (`CaseEqualComparable`) even when their internal algorithm
  // coincides.
  { a.IsBitIdentical(b) } -> std::same_as<bool>;
  // LRM 20.9 `$isunknown` predicate: does any bit of this value's
  // representation carry an X or Z. Universal: 2-state types implement
  // it as `return false`; 4-state types scan their bit pattern;
  // aggregate types recurse into elements. Lowering emits the call
  // uniformly across every value type, and downstream optimization
  // constant-folds the 2-state case.
  { a.HasUnknown() } -> std::same_as<bool>;
};

// LRM 11.4.5 `===` / `!==` (Any data type except `real` and `shortreal`). A
// value type opts in when its SV counterpart admits case equality; `real` /
// `shortreal` does not.
template <typename T>
concept CaseEqualComparable = LyraValue<T> && requires(const T& a, const T& b) {
  { a.CaseEqual(b) } -> std::same_as<PackedArray>;
};

// LRM 11.4.5 `==?` / `!=?` wildcard equality (Integral only).
template <typename T>
concept WildcardComparable = LyraValue<T> && requires(const T& a, const T& b) {
  { a.WildcardEquals(b) } -> std::same_as<PackedArray>;
};

// LRM 11.4.4 relational `<` / `<=` / `>` / `>=` (Integral, real /
// shortreal, `String`).
template <typename T>
concept Ordered = LyraValue<T> && requires(const T& a, const T& b) {
  { a < b } -> std::same_as<PackedArray>;
  { a <= b } -> std::same_as<PackedArray>;
  { a > b } -> std::same_as<PackedArray>;
  { a >= b } -> std::same_as<PackedArray>;
};

// Sized: integer-positioned container exposing the SV `.size()` query (LRM
// 7.4.3 dynamic, 7.5 associative, 7.10.2 queue, 7.4.6 unpacked). Result is
// the SV `int` shape -- a 32-bit signed `PackedArray`. PackedArray itself
// opts out: its "size" is ambiguous between bit count and outer-dim count,
// and SV does not expose `.size()` on packed types.
template <typename T>
concept Sized = requires(const T& t) {
  { t.Size() } -> std::same_as<PackedArray>;
};

// Lengthable: String's LRM 6.16.1 `.len()`, the LRM-named sibling of Sized.
// The same shape under a different method name (LRM-mandated spelling).
template <typename T>
concept Lengthable = requires(const T& t) {
  { t.Len() } -> std::same_as<PackedArray>;
};

// Indexable: single-element access by integer position. The container
// exposes a value-form (`Element`) returning a snapshot or const view, and
// a reference-form (`ElementRef`) returning a write-through reference. The
// pair models the bare-vs-`Ref`-suffix naming convention: the bare method
// hands you the element, the `Ref` method hands you a handle to it. String
// participates through character access -- `Element` is the indexed read and
// `ElementRef` a write-through proxy (a `StringCharRef`) -- while the LRM-named
// `Getc` / `Putc` methods (LRM 6.16) stay for the explicit method calls.
template <typename T>
concept Indexable = requires(T& t, const PackedArray& pos) {
  { t.Element(pos) };
  { t.ElementRef(pos) };
};

// AssocIndexable: associative-array indexed access by key. Same bare-vs-Ref
// pair as `Indexable`, but the key type is a free template parameter rather
// than the universal position type. AA's `Element(K)` reads with the LRM
// 7.5 default-on-miss policy; `ElementRef(K)` creates the key on missing
// access (LRM 7.5).
template <typename T, typename K>
concept AssocIndexable = requires(T& t, const K& key) {
  { t.Element(key) };
  { t.ElementRef(key) };
};

// Sliceable: extract a fixed-width sub-window (LRM 7.4.5 / 11.5.2). Conforming
// containers: PackedArray, DynamicArray, UnpackedArray. The shape is
// `Slice(anchor, count, shift)`:
//
// - `anchor` is the SV-declared endpoint the slice hangs from; the container
//   rebases it against its own declared range.
// - `count` is the type-fixed result width (LRM 7.4.5). It flows separately
//   because SV canonical-fills at the type-determined width even when the
//   anchor carries X/Z, and that width is not otherwise recoverable.
// - `shift` is how far below the rebased anchor the low end sits: zero for a
//   constant range or an indexed part-select growing toward the MSB, `count-1`
//   for one growing toward the LSB (LRM 11.5.1).
//
// An X/Z anchor canonical-fills the count-wide window. Bare `Slice` returns the
// value form (an owned snapshot); `SliceableRef` covers the reference form.
// A queue's slice is dynamic-width, derived from its bounds (LRM 7.10.1), and a
// string slices by `Substr(i, j)` (LRM 6.16.8); neither is this fixed-width
// contract, so neither claims Sliceable.
template <typename T>
concept Sliceable = requires(
    const T& t, const PackedArray& p1, const PackedArray& p2,
    const PackedArray& p3) {
  { t.Slice(p1, p2, p3) };
};

// SliceableRef: the reference-form counterpart of `Sliceable`. `SliceRef`
// returns a write-through proxy that, on `operator=`, scatters the value
// back into the receiver's storage. Queue does not satisfy this protocol
// because LRM 7.10 does not define a write-side queue slice.
template <typename T>
concept SliceableRef = requires(
    T& t, const PackedArray& p1, const PackedArray& p2, const PackedArray& p3) {
  { t.SliceRef(p1, p2, p3) };
};

// The unpacked family supplies its declared coordinate range at the select as a
// `[left:right]` operand pair sourced from the receiver's static type, rather
// than carrying it in the value. PackedArray self-describes (its dims are
// storage representation) and a dynamic array is zero-based, so both stay on
// `Indexable` / `Sliceable`; the
// unpacked value pins the range-carrying shape instead.
template <typename T>
concept RangedIndexable = requires(
    T& t, const PackedArray& pos, const PackedArray& left,
    const PackedArray& right) {
  { t.Element(pos, left, right) };
  { t.ElementRef(pos, left, right) };
};

template <typename T>
concept RangedSliceable = requires(
    const T& t, const PackedArray& a, const PackedArray& b,
    const PackedArray& form, const PackedArray& left,
    const PackedArray& right) {
  { t.Slice(a, b, form, left, right) };
};

template <typename T>
concept RangedSliceableRef = requires(
    T& t, const PackedArray& a, const PackedArray& b, const PackedArray& form,
    const PackedArray& left, const PackedArray& right) {
  { t.SliceRef(a, b, form, left, right) };
};

// Ownable: materialise a borrowed view into an owning value. Models Rust's
// `ToOwned` trait -- the result is the owning sibling type (a ref view
// returns its underlying value; an owning value returns itself by copy).
template <typename T>
concept Ownable = requires(const T& t) {
  { t.ToOwned() };
};

// Defaultable: in-place reset to the element-shape default (LRM Table 6-7).
// The receiver's shape (dims, bit_width, signedness, etc.) is preserved;
// only the value bits are zeroed.
template <typename T>
concept Defaultable = requires(T& t) {
  { t.ResetToDefault() };
};

// Sortable: in-place ordering family. Conforming containers expose
// `Sort(F)` / `Rsort(F)` taking a with-clause key closure (LRM 7.12.2) plus
// the no-closure `Reverse()`. C++ concepts cannot probe templated methods
// without instantiation, so the concept enforces `Reverse()` as the family
// marker; drift on the closure-taking siblings is caught when they are
// instantiated by their callers (HIR-to-MIR's with-clause synthesis).
template <typename T>
concept Sortable = requires(T& t) {
  { t.Reverse() };
};

// KeyTraversal: associative-array ordered-key navigation (LRM 7.9.4 --
// 7.9.7). Each call returns the next/previous key relative to the probe
// (or the first/last with no probe), or nullopt on an empty receiver or
// end-of-traversal.
template <typename T, typename K>
concept KeyTraversal = requires(const T& t, const K& probe) {
  { t.FirstKey() } -> std::same_as<std::optional<K>>;
  { t.LastKey() } -> std::same_as<std::optional<K>>;
  { t.NextKey(probe) } -> std::same_as<std::optional<K>>;
  { t.PrevKey(probe) } -> std::same_as<std::optional<K>>;
};

// Reducible and Searchable are documented for completeness but not exposed
// as compile-time concepts: every method in those families is templated on
// a closure F, and C++ concepts cannot probe templated methods without
// providing a concrete F. Their members are instead enforced through their
// callers (HIR-to-MIR's with-clause closure synthesis) -- any drift on
// `Sum/Product/And/Or/Xor` (Reducible, LRM 7.12.3) or
// `Find/FindIndex/.../Min/Max/Unique` (Searchable, LRM 7.12.1) breaks the
// caller, not the concept.
//
// The Writable protocol (write-side analogue of Indexable) is intentionally
// absent from this header: the three array containers today overload
// the write-side overload of `ElementRef`; the runtime-side protocol shape
// is documented by the `Indexable` concept above.

}  // namespace lyra::value
