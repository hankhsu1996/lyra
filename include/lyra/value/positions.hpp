#pragma once

#include <concepts>
#include <cstdint>

#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/real.hpp"

namespace lyra::value {

// How many positions a procedural continuous assignment may hold some of in a
// value of `T` (LRM 10.6.2): an integral value's bits, a real's IEEE 754 bits
// (LRM 6.12), an aggregate's parts' positions one part after another, its
// first part's first, and one for any other value, which is held whole.
template <class T>
[[nodiscard]] auto PositionCountOf() -> std::int64_t {
  if constexpr (IntegralValue<T>) {
    return static_cast<std::int64_t>(T::kWidth);
  } else if constexpr (std::same_as<T, Real>) {
    return 64;
  } else if constexpr (std::same_as<T, ShortReal>) {
    return 32;
  } else if constexpr (requires { T::PositionCount(); }) {
    return T::PositionCount();
  } else {
    return 1;
  }
}

// `into` with the `count` positions from `first` taken from `from`, a value of
// its type. A value held whole is taken whole, so a run naming any of its
// positions names all of them.
template <class T>
[[nodiscard]] auto PositionsTaken(
    const T& into, const T& from, std::int64_t first, std::int64_t count) -> T {
  if constexpr (IntegralValue<T>) {
    return TakePositions(into, from, first, count);
  } else if constexpr (requires { into.TakePositions(from, first, count); }) {
    return into.TakePositions(from, first, count);
  } else {
    return from;
  }
}

// The positions of a value whose every part is an integral value of either
// state, the ones a net resolves (LRM 6.7.1). A net resolves positions with no
// type and a value of an unpacked type lays its own out part by part, so what
// passes between the two is a view of positions where they lie, never copied,
// whose least significant position stands at position `start` of the value --
// which may lie before the value's first position or after its last.
//
// An integral value with the positions the view reaches of it replaced by the
// view's.
template <IntegralValue T>
[[nodiscard]] auto WithPositions(
    const T& value, const ConstIntegralView& in, std::int64_t start) -> T {
  T out = value;
  WithPositionsInto(out.MutableView(), in, start);
  return out;
}

// A value's positions the view reaches replaced in place by the view's, the
// view holding `count` positions; answers whether any changed (LRM 9.4.2).
// Only the parts the positions lie in are reached.
template <class T>
auto WritePositionsOf(
    T& into, const ConstIntegralView& in, std::int64_t start,
    std::int64_t count) -> bool {
  if constexpr (IntegralValue<T>) {
    return WithPositionsInto(into.MutableView(), in, start);
  } else {
    return into.WritePositions(in, start, count);
  }
}

// All of a value's positions as one integral value of as many bits, the first
// position least significant: a real's are its IEEE 754 bits (LRM 6.12, 20.5).
template <class T>
[[nodiscard]] auto ToPositionsOf(const T& value) {
  if constexpr (IntegralValue<T>) {
    return LogicVector<T::kWidth>::Built([&](LogicVector<T::kWidth>& out) {
      detail::WriteRun(
          out.MutableView(), value.View(), 0, {.lsb = 0, .width = T::kWidth});
    });
  } else if constexpr (std::same_as<T, Real>) {
    return LogicVector<64>::FromInt(value.ToBits());
  } else if constexpr (std::same_as<T, ShortReal>) {
    return LogicVector<32>::FromInt(value.ToBits());
  } else {
    return value.ToPositions();
  }
}

// The value of `T` an integral value of its positions holds, as
// `ToPositionsOf` lays them out.
template <class T, class Positions>
[[nodiscard]] auto FromPositionsOf(const Positions& positions) -> T {
  if constexpr (IntegralValue<T>) {
    return T::Built([&](T& out) {
      detail::WriteRun(
          out.MutableView(), positions.View(), 0,
          {.lsb = 0, .width = T::kWidth});
    });
  } else if constexpr (std::same_as<T, Real> || std::same_as<T, ShortReal>) {
    return T::FromBits(positions.ToInt64());
  } else {
    return T::FromPositions(positions);
  }
}

}  // namespace lyra::value
