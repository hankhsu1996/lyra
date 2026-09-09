#pragma once

#include <array>
#include <cstddef>
#include <vector>

namespace lyra::runtime {

// Builds the sequence a declaration standing for several objects holds
// (LRM 23.3.2): the handles it covers, in the order its coordinates count
// them. A sequence is composed whole and never grown, so its one argument is
// the element list, and a declaration covering nothing hands over an empty one.
template <typename T, std::size_t N>
[[nodiscard]] auto MakeSequence(const std::array<T, N>& elements)
    -> std::vector<T> {
  return std::vector<T>(elements.begin(), elements.end());
}

}  // namespace lyra::runtime
