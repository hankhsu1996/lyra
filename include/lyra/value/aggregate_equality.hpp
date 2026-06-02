#pragma once

#include <cstddef>
#include <vector>

#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// LRM 11.2.2 + 11.4.5: aggregate equality compares operands element-by-element
// with X / Z propagation per the element type's equality semantics. The
// overload set is recursive over the element type: the scalar PackedArray arm
// is the terminal case, the std::vector arm dispatches via the same name and
// resolves nested aggregates through ADL.

inline auto AggregateEqual(const PackedArray& a, const PackedArray& b)
    -> PackedArray {
  return a == b;
}

template <typename Elem>
auto AggregateEqual(const std::vector<Elem>& a, const std::vector<Elem>& b)
    -> PackedArray {
  // Slang's binding enforces equivalent type per LRM 11.2.2, so sizes match.
  // Seed the fold from the first element so the result's state-kind tracks
  // the operands' state-kind naturally; fixed-size unpacked arrays per LRM
  // 7.4.2 have non-zero size.
  auto result = AggregateEqual(a[0], b[0]);
  for (std::size_t i = 1; i < a.size(); ++i) {
    result = result && AggregateEqual(a[i], b[i]);
  }
  return result;
}

inline auto AggregateNotEqual(const PackedArray& a, const PackedArray& b)
    -> PackedArray {
  return !AggregateEqual(a, b);
}

template <typename Elem>
auto AggregateNotEqual(const std::vector<Elem>& a, const std::vector<Elem>& b)
    -> PackedArray {
  return !AggregateEqual(a, b);
}

inline auto AggregateCaseEqual(const PackedArray& a, const PackedArray& b)
    -> PackedArray {
  // PackedArray::CaseEqual produces a 2-state 1-bit value (the result is
  // always known per LRM 11.4.5). On aggregate operands slang's binding
  // declares the result state-kind to follow the operand element type, so
  // a 4-state operand needs a state-kind-only conversion to keep the helper
  // output consistent with the MIR-declared result type.
  auto raw = a.CaseEqual(b);
  const bool four_state = a.IsFourState() || b.IsFourState();
  return four_state ? PackedArray::ConvertFrom(raw, 1, false, true) : raw;
}

template <typename Elem>
auto AggregateCaseEqual(const std::vector<Elem>& a, const std::vector<Elem>& b)
    -> PackedArray {
  auto result = AggregateCaseEqual(a[0], b[0]);
  for (std::size_t i = 1; i < a.size(); ++i) {
    result = result && AggregateCaseEqual(a[i], b[i]);
  }
  return result;
}

inline auto AggregateCaseNotEqual(const PackedArray& a, const PackedArray& b)
    -> PackedArray {
  return !AggregateCaseEqual(a, b);
}

template <typename Elem>
auto AggregateCaseNotEqual(
    const std::vector<Elem>& a, const std::vector<Elem>& b) -> PackedArray {
  return !AggregateCaseEqual(a, b);
}

}  // namespace lyra::value
