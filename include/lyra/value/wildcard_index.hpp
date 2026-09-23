#pragma once

#include <algorithm>
#include <cstdint>

#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// LRM 7.8.1: what an associative array declared with a wildcard index makes of
// the integral expressions used to index it. The clause admits an index of any
// width, makes it self-determined and treated as unsigned, and orders the
// entries by numerical value -- so what two indices mean to each other is fixed
// by the declaration and absent from both of them.
//
// The rule splits in two because the two halves run at different rates. An
// index is reinterpreted once, when it becomes a key; two keys are compared on
// every lookup. A realization that can hold its keys normalized does the first
// at construction and only the second per comparison, and one that reaches an
// index as a bare value does both.

// The unsigned value an index names: the same bits at the same width, so no
// sign extension happens and `-1` and `32'hFFFFFFFF` name one entry. x / z is
// preserved, because an index carrying either names no entry whatever its
// value (LRM 7.8.6) and the check for that runs against the key.
[[nodiscard]] inline auto WildcardIndexValue(const PackedArray& index)
    -> PackedArray {
  return PackedArray::ConvertFrom(
      index, index.BitWidth(), false, index.IsFourState());
}

// Whether `a` sits before `b`. Both are already the unsigned values above, so
// widening them to the wider fills with zeros and the comparison is the
// clause's numerical one across widths -- which is what makes `8'd5` and
// `16'd5` neither before the other, and so one entry.
[[nodiscard]] inline auto WildcardIndexBefore(
    const PackedArray& a, const PackedArray& b) -> bool {
  const std::uint64_t width = std::max(a.BitWidth(), b.BitWidth());
  return static_cast<bool>(
      PackedArray::ConvertFrom(a, width, false, false) <
      PackedArray::ConvertFrom(b, width, false, false));
}

}  // namespace lyra::value
