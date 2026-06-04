#pragma once

#include "lyra/value/packed_array.hpp"

namespace lyra::value {

template <typename T>
class UnpackedArray;
template <typename T>
class DynamicArray;

namespace detail {

// LRM 11.4.5 element-level `===` used by both array-container CaseEqual
// implementations. The PackedArray overload normalises the 0 / 1 result to
// 4-state when either operand is 4-state so the enclosing `&&` reduction
// stays in the right state class; the recursive overloads delegate to the
// container's own CaseEqual.
inline auto ArrayCaseEqElement(const PackedArray& a, const PackedArray& b)
    -> PackedArray {
  auto raw = a.CaseEqual(b);
  const bool four_state = a.IsFourState() || b.IsFourState();
  return four_state ? PackedArray::ConvertFrom(raw, 1, false, true) : raw;
}

template <typename U>
auto ArrayCaseEqElement(const UnpackedArray<U>& a, const UnpackedArray<U>& b)
    -> PackedArray {
  return a.CaseEqual(b);
}

template <typename U>
auto ArrayCaseEqElement(const DynamicArray<U>& a, const DynamicArray<U>& b)
    -> PackedArray {
  return a.CaseEqual(b);
}

}  // namespace detail
}  // namespace lyra::value
