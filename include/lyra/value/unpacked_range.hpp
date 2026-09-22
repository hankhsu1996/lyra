#pragma once

#include <cstddef>
#include <cstdint>

namespace lyra::value {

// The declared range of one unpacked dimension, the array's coordinate system.
// Element order runs left-to-right (LRM 7.6), so the leftmost element (index
// `left`) is storage ordinal 0. `ToOrdinal` maps a source-declared index onto
// the storage ordinal; `FromOrdinal` is its inverse. Unlike a packed bit range,
// the ordinal counts from the left, not the least-significant end.
//
// It sits in a header of its own because it is what a select on the unpacked
// family takes as its coordinate operand, so everything from the operator
// concepts down to the arrays themselves names it, and the concepts are what
// every value type is defined against.
struct UnpackedRange {
  std::int64_t left;
  std::int64_t right;

  [[nodiscard]] auto IsAscending() const -> bool {
    return left <= right;
  }
  [[nodiscard]] auto ToOrdinal(std::int64_t sv) const -> std::int64_t {
    return IsAscending() ? sv - left : left - sv;
  }
  [[nodiscard]] auto FromOrdinal(std::int64_t ordinal) const -> std::int64_t {
    return IsAscending() ? ordinal + left : left - ordinal;
  }
  // A declared range spans `|left - right| + 1` elements and is never empty.
  // `[0:-1]` is the one exception -- the synthetic empty range standing in for
  // a dimension that does not exist, which no real declared range spells.
  [[nodiscard]] auto Count() const -> std::size_t {
    if (left == 0 && right == -1) {
      return 0;
    }
    return static_cast<std::size_t>(
               IsAscending() ? right - left : left - right) +
           1U;
  }
  // The lowest declared index, which is C index 0 in the DPI layout of an
  // unpacked dimension (LRM Annex H.7.3).
  [[nodiscard]] auto Low() const -> std::int64_t {
    return IsAscending() ? left : right;
  }
  // The highest declared index, the far end of the same span. A walk in
  // ascending address runs `Low()` through here whichever way the dimension was
  // declared (LRM 21.4.3).
  [[nodiscard]] auto High() const -> std::int64_t {
    return IsAscending() ? right : left;
  }
};

}  // namespace lyra::value
