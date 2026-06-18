#pragma once

#include <concepts>

#include "lyra/value/packed_array.hpp"

namespace lyra::value {

namespace detail {

// LRM 11.4.5 element-level `===` used by every aggregate-container `CaseEqual`
// implementation (`UnpackedArray`, `DynamicArray`, `Queue`, ...). The
// `PackedArray` overload normalises the 0 / 1 result to 4-state when either
// operand is 4-state so the enclosing `&&` reduction stays in the right state
// class; the generic fallback delegates to whichever value type's own
// `CaseEqual` returns the 1-bit packed result (`String`, nested arrays, ...).
inline auto ArrayCaseEqElement(const PackedArray& a, const PackedArray& b)
    -> PackedArray {
  auto raw = a.CaseEqual(b);
  const bool four_state = a.IsFourState() || b.IsFourState();
  return four_state ? PackedArray::ConvertFrom(raw, 1, false, true) : raw;
}

template <typename T>
  requires requires(const T& a, const T& b) {
    { a.CaseEqual(b) } -> std::same_as<PackedArray>;
  }
auto ArrayCaseEqElement(const T& a, const T& b) -> PackedArray {
  return a.CaseEqual(b);
}

}  // namespace detail
}  // namespace lyra::value
