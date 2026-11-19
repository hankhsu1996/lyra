#pragma once

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <type_traits>
#include <utility>

#include "lyra/base/source_location.hpp"
#include "lyra/value/formation.hpp"
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

// What every value a holder keeps answers, whatever its type: whether a value
// written over it changed it, the engine's change-detection predicate (LRM
// 9.4.2), since any variable may be waited on. This is the concept
// `lyra::runtime::Var<T>` requires. The language's operations on the whole
// value -- `==`, `$isunknown`, its stream of bits -- are asked of a type by
// whatever compares or streams it, so a value the library holds of a type it
// was compiled without carries none of them.
template <typename T>
concept LyraValue = Storable<T> && requires(const T& a, const T& b) {
  // The engine's change-detection hook -- distinct from the LRM `===`
  // operator (`CaseEqualComparable`) even when their internal algorithm
  // coincides.
  { a.IsBitIdentical(b) } -> std::same_as<bool>;
};

// LRM 11.4.5 `===` / `!==`. A value type opts in when its SV counterpart
// admits case equality, and a real compares values, as the front end evaluates
// it. The answer is never unknown, so it is a bit.
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
// such a type, so a net is composed entirely of 4-state bits and resolves per
// bit (LRM 6.6.7), which the net does over positions with no type. An integral
// value is its bits, a 2-state one taking an x or z it resolves to as 0 (LRM
// 6.11.2). An aggregate numbers its positions part by part, answers
// all of them as one integral value and has the ones a net resolved laid into
// the parts they lie in, which is the recursion this concept states: an
// aggregate is net-resolvable exactly when its elements are. A dynamically
// sized container is excluded by "fixed-size", and `String` / `Real` by
// "4-state bits".
template <typename T>
concept NetResolvable =
    LyraValue<T> && (IntegralValue<T> ||
                     requires(const T& a, T& b, const ConstIntegralView& in) {
                       a.ToPositions();
                       {
                         b.WritePositions(in, std::int64_t{}, std::int64_t{})
                       } -> std::same_as<bool>;
                     });

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
// control-bit values. There are four such values, so the control bits are a
// four-position four-state value naming each value admitted, and a value named
// by several positions counts once, so the count never exceeds the width.
// Counting every value is the width, which is why the two queries share a
// participation set. Each is handed `at`, where the design wrote the query,
// since a value in a composite may hold no stream to measure.
template <typename T>
concept BitstreamSizable = requires(
    const T& t, const LogicVector<4>& control, base::SourceLocation at) {
  { t.BitstreamWidth(at) } -> std::same_as<Int>;
  { t.CountBits(control, at) } -> std::same_as<Int>;
};

// Indexable: one element reached by position, for reading and for writing,
// as rustc's `Index` and `IndexMut` reach one. `Element` answers a read-only
// reference to the element, or to the element default where the position
// names none; `ElementMut` answers the element, or storage no read reaches
// where the position names none (LRM 7.4.5), saying which in `formed`. A
// position is a value of the position type, which names none where it holds x
// or z or lies outside the container.
template <typename T>
concept Indexable = requires(T& t, const Position& pos, Formation& formed) {
  { t.Element(pos) };
  { t.ElementMut(pos, formed) };
};

// AssocIndexable: the same two by key. A read of a key holding no entry
// answers the array's default and creates nothing; `ElementMut` creates the
// entry (LRM 7.8.6, 7.8.7).
template <typename T, typename K>
concept AssocIndexable = requires(T& t, const K& key, Formation& formed) {
  { t.Element(key) };
  { t.ElementMut(key, formed) };
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
  { t.Count() } -> std::same_as<std::size_t>;
  { t.GetUnchecked(ordinal) };
};

// The element type of an array whose elements are read by storage ordinal,
// which the element at an ordinal is a reference to, so every array -- one of
// the library's own or a container type's -- states it the one way.
template <OrdinalElements Array>
using ElementOf =
    std::remove_cvref_t<decltype(std::declval<const Array&>().GetUnchecked(0))>;

// A held value that compares itself with, and takes, the bytes of a value laid
// out as it is itself: an integral value as wide, or a tuple, a union or an
// array of the same type.
template <class T>
concept TakesBytesInPlace =
    requires(T& held, const T& read, const void* bytes) {
      { read.IsBitIdenticalTo(bytes) } -> std::same_as<bool>;
      held.AssignBytes(bytes);
    };

}  // namespace lyra::value
