#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <utility>

#include "lyra/value/integral_fwd.hpp"
#include "lyra/value/integral_words.hpp"

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
//   for case-equality and ordering.
// - SV container-method contracts (LRM 7.x, 6.16): the integer-positioned
//   methods (size, slice, element access, etc.) shared across the array
//   families.

namespace lyra::value {

// C++ storage mechanics every runtime value type must satisfy so a container
// or a cell can copy, move, and relocate it. `std::copyable` requires move- and
// copy-construction, move- and copy-assignment, and swappability -- and, per
// the standard library contract it builds on, a moved-from object that remains
// valid for assignment and destruction. That moved-from validity is the
// property that actually bites: a value type whose move leaves an unusable husk
// crashes wherever it is relocated.
//
// This is a C++-mechanics contract, NOT a statement about SystemVerilog
// assignment meaning. Every value type's assignment is an ordinary whole-value
// replacement; the SystemVerilog rule that a store keeps the destination's
// declared type lives at the variable cell and the store boundary, not in the
// value's assignment operator. The concept only guarantees the operations
// exist and relocation is safe.
template <typename T>
concept Storable = std::copyable<T>;

// What a comparison answers (LRM 11.4.4, 11.4.5): 0, 1 or x. Code compiled
// with the type of what it compares answers a value of the type the comparison
// has -- a bit, or a logic where what is compared can hold x or z -- and code
// compiled without it answers the scalar alone, which whoever knows that type
// holds as a value of it.
template <typename R>
concept ComparisonAnswer =
    std::same_as<R, FourStateBit> || (std::copyable<R> && requires(const R& r) {
      { r.Lsb() } -> std::same_as<FourStateBit>;
    });

// The scalar a comparison answered.
[[nodiscard]] constexpr auto AnswerScalar(FourStateBit answer) -> FourStateBit {
  return answer;
}
template <typename R>
  requires(!std::same_as<R, FourStateBit>) && ComparisonAnswer<R>
[[nodiscard]] constexpr auto AnswerScalar(const R& answer) -> FourStateBit {
  return answer.Lsb();
}

// LRM 11.4.5: `!=` answers the negation of what `==` does, unknown where that
// is.
template <typename R>
  requires(!std::same_as<R, FourStateBit>) && ComparisonAnswer<R>
[[nodiscard]] constexpr auto Inverted(const R& answer) {
  return !answer;
}

// Whether a comparison is known to hold, which is what a condition reads of it
// (LRM 12.4).
template <ComparisonAnswer R>
[[nodiscard]] constexpr auto Holds(const R& answer) -> bool {
  switch (AnswerScalar(answer)) {
    case FourStateBit::kOne:
      return true;
    case FourStateBit::kZero:
    case FourStateBit::kHighImpedance:
    case FourStateBit::kUnknown:
      return false;
  }
  std::unreachable();
}

// The SV data type contract every value type realises (LRM Table 11-1 "Any"
// row). A type that satisfies this concept provides the universal equality
// operators plus the engine's bit-pattern change-detection predicate. This
// is the concept `lyra::runtime::Var<T>` requires, so wrapping a
// structural-var in observable storage gates on it.
template <typename T>
concept LyraValue = Storable<T> && requires(const T& a, const T& b) {
  // LRM 11.4.5 `==` / `!=` (Any data type).
  { a == b } -> ComparisonAnswer;
  { a != b } -> ComparisonAnswer;
  // LRM 9.4.2 update event predicate: did the cell's bit-pattern change.
  // The engine's change-detection hook -- distinct from the LRM `===`
  // operator (`CaseEqualComparable`) even when their internal algorithm
  // coincides.
  { a.IsBitIdentical(b) } -> std::same_as<bool>;
  // LRM 20.9 unknown detection: does any bit of this value's representation
  // carry an X or Z. 2-state types answer no; 4-state types scan their bit
  // pattern; aggregates recurse into elements.
  { a.HasUnknown() } -> std::same_as<bool>;
};

// LRM 11.4.5 `===` / `!==` (Any data type except `real` and `shortreal`). A
// value type opts in when its SV counterpart admits case equality; `real` /
// `shortreal` does not. The answer is never unknown, so it is a bit.
template <typename T>
concept CaseEqualComparable = LyraValue<T> && requires(const T& a, const T& b) {
  { a.CaseEqual(b) } -> std::same_as<Bit>;
};

// LRM 11.4.6 `==?` / `!=?` wildcard equality (integral only), which can be
// unknown where what is compared can hold x or z.
template <typename T>
concept WildcardComparable = LyraValue<T> && requires(const T& a, const T& b) {
  { a.WildcardEquals(b) } -> ComparisonAnswer;
};

// LRM 6.7.1: a value a net may hold. The clause admits a 4-state integral type
// or a fixed-size unpacked array / struct / union whose every element is itself
// such a type, so a net is composed entirely of 4-state bits and combines per
// bit. The recursion is what this concept states: an aggregate is
// net-resolvable exactly when its elements are, and it answers every net
// question by delegating to them. Each of the three truth tables folds one
// driver's contribution into another -- tri-state, wired-and, and wired-or
// (LRM 6.6.1 Table 6-2, LRM 6.6.3 Tables 6-3 and 6-4) -- and each is an
// operation of its own, since which one a net asks is the net's state rather
// than the value's; `Dominating` is what a stronger contribution does to a
// weaker one: it determines every position it drives and leaves the rest (LRM
// 28.12.1); and a value with every bit holding one scalar states both what a
// net shows where nothing drives it and what its type contributes to its own
// resolution (LRM 6.7.1). An integral type fixes how many bits that is, so it
// answers `Filled` of the scalar alone; an aggregate's shape is a value's, so
// it answers `FilledLike` a value of that shape. A dynamically sized container
// is excluded by "fixed-size", and `String` / `Real` by "4-state bits".
template <typename T>
concept ShapedByAValue = requires(const T& a, const Logic& fill) {
  { T::FilledLike(a, fill) } -> std::same_as<T>;
};

template <typename T>
concept NetResolvable =
    LyraValue<T> && (IntegralValue<T> || ShapedByAValue<T>) &&
    requires(const T& a, const T& b) {
      { a.ResolveTriState(b) } -> std::same_as<T>;
      { a.ResolveWiredAnd(b) } -> std::same_as<T>;
      { a.ResolveWiredOr(b) } -> std::same_as<T>;
      { a.Dominating(b) } -> std::same_as<T>;
    };

// A value of `T` with every bit holding the scalar `fill` holds, shaped like
// `prototype` where the type alone does not fix the shape.
template <NetResolvable T, IntegralValue Fill>
[[nodiscard]] constexpr auto FilledAs(const T& prototype, const Fill& fill)
    -> T {
  if constexpr (IntegralValue<T>) {
    return T::Filled(fill.Lsb());
  } else {
    return T::FilledLike(prototype, fill);
  }
}

// Two contributions folded under the table a net's type names. Which table
// that is, is a value the running program holds, so it selects among the three
// operations here, once, for every type a net may hold.
template <NetResolvable T>
[[nodiscard]] auto Resolve(NetResolution fold, const T& a, const T& b) -> T {
  switch (fold) {
    case NetResolution::kTriState:
      return a.ResolveTriState(b);
    case NetResolution::kWiredAnd:
      return a.ResolveWiredAnd(b);
    case NetResolution::kWiredOr:
      return a.ResolveWiredOr(b);
  }
  std::unreachable();
}

// LRM 11.4.11: where a conditional operator's condition is ambiguous it selects
// neither arm, evaluates both, and combines them. A type made of parts that can
// agree defines how -- an integral bit by bit (Table 11-20), an array element
// by element -- and a type with no such parts does not satisfy this, taking the
// Table 7-1 default of its own shape instead.
template <typename T>
concept ConditionallyMergeable =
    LyraValue<T> && requires(const T& a, const T& b) {
      { a.MergeConditional(b) } -> std::same_as<T>;
    };

// LRM 11.4.4 relational `<` / `<=` / `>` / `>=` (Integral, real /
// shortreal, `String`).
template <typename T>
concept Ordered = LyraValue<T> && requires(const T& a, const T& b) {
  { a < b } -> ComparisonAnswer;
  { a <= b } -> ComparisonAnswer;
  { a > b } -> ComparisonAnswer;
  { a >= b } -> ComparisonAnswer;
};

// Sized: integer-positioned container exposing the SV `.size()` query (LRM
// 7.4.3 dynamic, 7.5 associative, 7.10.2 queue, 7.4.6 unpacked), which
// answers an `int`. SV does not expose `.size()` on packed types.
template <typename T>
concept Sized = requires(const T& t) {
  { t.Size() } -> std::same_as<Int>;
};

// Lengthable: String's LRM 6.16.1 `.len()`, the LRM-named sibling of Sized.
// The same shape under a different method name (LRM-mandated spelling).
template <typename T>
concept Lengthable = requires(const T& t) {
  { t.Len() } -> std::same_as<Int>;
};

// BitstreamSizable: the bit-stream types (LRM 6.24.3 -- integral, packed, or
// string, and unpacked / dynamic / associative / queue / struct compositions of
// those, recursively) expose the LRM 20.6.2 `$bits` bit count of what the value
// currently holds, as an `int`. `real` / `shortreal`, chandle, and event are
// not bit-stream types and do not participate. A composite is bit-stream
// sizable exactly when its elements are, so a container's method well-forms
// only over a bit-stream element type; the concept pins the method's shape,
// and element-type drift surfaces when the body instantiates.
//
// The same types answer the LRM 20.9 `$countbits` question, because it counts
// over the same bit stream: how many of those bits carry one of the given
// control-bit values. The control bits are an integral value of any type, one
// control bit per position, and a bit value named by several of them counts
// once, so the count never exceeds the width. Counting every value is the
// width, which is why the two queries share a participation set. `Control` is
// one integral value the type is asked with, standing for all of them: a value
// of a type the claim states, or the form a value takes where the type holding
// it was compiled without its type.
template <typename T, typename Control = Logic>
concept BitstreamSizable = requires(const T& t, const Control& control) {
  { t.BitstreamWidth() } -> std::same_as<Int>;
  { t.CountBits(control) } -> std::same_as<Int>;
};

// Indexable: single-element access by position. The container
// exposes a value-form (`Element`) returning a snapshot or const view, and
// a reference-form (`ElementRef`) returning a write-through reference. The
// pair models the bare-vs-`Ref`-suffix naming convention: the bare method
// hands you the element, the `Ref` method hands you a handle to it. String
// participates through character access -- `Element` is the indexed read and
// `ElementRef` a write-through proxy (a `StringCharRef`) -- while the LRM-named
// `Getc` / `Putc` methods (LRM 6.16) stay for the explicit method calls. A
// position is a value of the position type, which names none where it holds x
// or z or lies outside the container.
template <typename T>
concept Indexable = requires(T& t, const Position& pos) {
  { t.Element(pos) };
  { t.ElementRef(pos) };
};

// AssocIndexable: associative-array indexed access by key. Same bare-vs-Ref
// pair as `Indexable`, but the key type is a free template parameter rather
// than a position. AA's `Element(K)` reads with the LRM 7.5 default-on-miss
// policy; `ElementRef(K)` creates the key on missing access (LRM 7.5).
template <typename T, typename K>
concept AssocIndexable = requires(T& t, const K& key) {
  { t.Element(key) };
  { t.ElementRef(key) };
};

// Sliceable: `count` parts in a row starting at a position (LRM 7.4.5 for the
// elements of an unpacked array). The count is fixed by the type the select
// produces, so it arrives as a number; the start arrives as a position, which
// may also name no position at all, and a start that names none reads every
// part at its default. Bare `Slice` returns the value form (an owned
// snapshot); `SliceableRef` covers the reference form.
//
// A queue's slice is bounded by two positions that the running program can
// move (LRM 7.10.1), and a string slices by `Substr(i, j)` (LRM 6.16.8);
// neither is a fixed count of parts, so neither claims this.
template <typename T>
concept Sliceable =
    requires(const T& t, const Position& start, std::int64_t count) {
      { t.Slice(start, count) };
    };

// SliceableRef: the reference-form counterpart of `Sliceable`. `SliceRef`
// returns a write-through proxy that, on `operator=`, writes the value back
// into the receiver's storage at the positions that exist.
template <typename T>
concept SliceableRef =
    requires(T& t, const Position& start, std::int64_t count) {
      { t.SliceRef(start, count) };
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

// IndexTraversal: associative-array ordered-index navigation (LRM 7.9.4 --
// 7.9.7). Each call returns the next / previous index relative to the probe
// (or the smallest / largest with no probe), or nullopt on an empty receiver
// or end-of-traversal.
template <typename T, typename Index>
concept IndexTraversal = requires(const T& t, const Index& probe) {
  { t.FirstIndex() } -> std::same_as<std::optional<Index>>;
  { t.LastIndex() } -> std::same_as<std::optional<Index>>;
  { t.NextIndex(probe) } -> std::same_as<std::optional<Index>>;
  { t.PrevIndex(probe) } -> std::same_as<std::optional<Index>>;
};

// OrdinalElements: the unpacked family's own elements, read by storage ordinal.
// LRM 7.6 pairs two arrays for assignment by the left-to-right order of their
// elements, which is that ordinal and is the one coordinate a fixed-size array,
// a dynamic array, and a queue all answer to -- a declared range belongs to the
// static type at a select, and a queue has no declared range at all. Reading a
// container this way is therefore what lets one be built from another without
// either naming the other's kind.
template <typename T>
concept OrdinalElements = requires(const T& t, std::size_t ordinal) {
  { t.RawSize() } -> std::same_as<std::size_t>;
  { t.RawAt(ordinal) };
};

// Reducible and Searchable are documented for completeness but not exposed
// as compile-time concepts: every method in those families is templated on
// a closure F, and C++ concepts cannot probe templated methods without
// providing a concrete F. Their members are instead enforced through their
// callers (HIR-to-MIR's with-clause closure synthesis) -- any drift on
// `Sum/Product/And/Or/Xor` (Reducible, LRM 7.12.3) or
// `Find/FindIndex/.../Min/Max/Unique` (Searchable, LRM 7.12.1) breaks the
// caller, not the concept.

}  // namespace lyra::value
