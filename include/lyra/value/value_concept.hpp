#pragma once

#include <concepts>

namespace lyra::value {

class PackedArray;

// The contract every Lyra runtime value type satisfies so it can live in the
// STL containers the runtime stores it in (`std::vector` / `std::deque` /
// `std::map`) and survive their relocation (insert / erase / grow). `std::
// copyable` requires move- and copy-construction, move- and copy-assignment,
// and swappability -- and, per the standard library contract it builds on, a
// moved-from object that remains valid for assignment and destruction. That
// moved-from validity is the property that actually bites: a value type whose
// move leaves an unusable husk crashes inside container relocation.
//
// This is a C++-mechanics contract, NOT a statement about assignment meaning.
// For shape-bearing types like `PackedArray`, assignment preserves the
// destination's declared shape and copies the source's bits in (it is not a
// pure value adopt); see docs/decisions/integral-representation.md. The
// concept only guarantees the operations exist and relocation is safe.
template <typename T>
concept LyraValue = std::copyable<T>;

// The SV data type contract every value type realises (LRM Table 11-1 "Any"
// row). A type that satisfies this concept provides the universal equality
// operators plus the engine's bit-pattern change-detection predicate. This is
// the concept `lyra::runtime::Var<T>` requires, so wrapping a structural-var
// in observable storage gates on it. See
// `docs/decisions/value-type-concepts.md`.
template <typename T>
concept LyraValueType = LyraValue<T> && requires(const T& a, const T& b) {
  // LRM 11.4.5 `==` / `!=` (Any data type). Uniform return type: every value
  // type yields a 1-bit `PackedArray`; 2-state types yield a 2-state packed
  // result. The uniform type lets lowering and emit treat every equality call
  // identically.
  { a == b } -> std::same_as<PackedArray>;
  { a != b } -> std::same_as<PackedArray>;
  // LRM 9.4.2 update event predicate: did the cell's bit-pattern change. The
  // engine's change-detection hook -- distinct from the LRM `===` operator
  // (`CaseEqualComparable`) even when their internal algorithm coincides.
  { a.IsBitIdentical(b) } -> std::same_as<bool>;
};

// LRM 11.4.5 `===` / `!==` (Any data type except `real` and `shortreal`). A
// value type opts in when its SV counterpart admits case equality; `real` /
// `shortreal` does not.
template <typename T>
concept CaseEqualComparable =
    LyraValueType<T> && requires(const T& a, const T& b) {
      { a.CaseEqual(b) } -> std::same_as<PackedArray>;
    };

// LRM 11.4.5 `==?` / `!=?` wildcard equality (Integral only).
template <typename T>
concept WildcardComparable =
    LyraValueType<T> && requires(const T& a, const T& b) {
      { a.WildcardEquals(b) } -> std::same_as<PackedArray>;
    };

// LRM 11.4.4 relational `<` / `<=` / `>` / `>=` (Integral, real / shortreal,
// `String`).
template <typename T>
concept Ordered = LyraValueType<T> && requires(const T& a, const T& b) {
  { a < b } -> std::same_as<PackedArray>;
  { a <= b } -> std::same_as<PackedArray>;
  { a > b } -> std::same_as<PackedArray>;
  { a >= b } -> std::same_as<PackedArray>;
};

}  // namespace lyra::value
