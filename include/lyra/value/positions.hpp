#pragma once

#include <cstdint>

#include "lyra/value/integral.hpp"

namespace lyra::value {

// How many positions a procedural continuous assignment may hold some of in a
// value of `T` (LRM 10.6.2): an integral value's bits, an aggregate's parts'
// positions one part after another, its first part's first, and one for any
// other value, which is held whole.
template <class T>
[[nodiscard]] auto PositionCountOf() -> std::int64_t {
  if constexpr (IntegralValue<T>) {
    return static_cast<std::int64_t>(T::kWidth);
  } else if constexpr (requires { T::PositionCount(); }) {
    return T::PositionCount();
  } else {
    return 1;
  }
}

// `into` with the `count` positions from `first` taken from `from`, a value of
// its type. A value held whole is one position, so a run naming any of it
// names all of it.
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

}  // namespace lyra::value
