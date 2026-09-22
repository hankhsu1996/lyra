#pragma once

#include <array>
#include <cstddef>
#include <vector>

namespace lyra::runtime {

// Builds the sequence a declaration standing for several objects holds
// (LRM 23.3.2): the handles it covers, in the order its coordinates count
// them. A declaration covering nothing hands over an empty element list rather
// than reaching a second entry.
template <typename T, std::size_t N>
[[nodiscard]] auto MakeSequence(const std::array<T, N>& elements)
    -> std::vector<T> {
  return std::vector<T>(elements.begin(), elements.end());
}

// Extends a sequence still being composed by one element, answering with what
// it became. A member holding a sequence receives it complete, so the sequence
// that grows is never one a member holds; taking it by value and handing it
// back is what keeps that true at every step, and it costs nothing because the
// caller's only copy moves through.
template <typename T>
[[nodiscard]] auto ExtendSequence(std::vector<T> sequence, T element)
    -> std::vector<T> {
  sequence.push_back(std::move(element));
  return sequence;
}

}  // namespace lyra::runtime
