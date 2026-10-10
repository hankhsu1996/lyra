#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"

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

// The unsigned value an index names, as its words with every x or z read as
// 0, and with the words above its most significant set one dropped, so no sign
// extension happens, `-1` and `32'hFFFFFFFF` name one entry, and `8'd5` and
// `16'd5` hold the same words.
[[nodiscard]] inline auto WildcardIndexWords(const ConstIntegralView& index)
    -> std::vector<std::uint64_t> {
  std::vector<std::uint64_t> words(WordCountForBits(index.width));
  for (std::size_t i = 0; i < words.size(); ++i) {
    words[i] = WordAt(index.planes.value, i) & ~WordAt(index.planes.unknown, i);
  }
  while (!words.empty() && words.back() == 0U) {
    words.pop_back();
  }
  return words;
}

// Whether the value the words `a` hold sits before the one `b` holds: the
// clause's numerical comparison, across widths.
[[nodiscard]] inline auto WildcardIndexBefore(
    const std::vector<std::uint64_t>& a, const std::vector<std::uint64_t>& b)
    -> bool {
  if (a.size() != b.size()) {
    return a.size() < b.size();
  }
  return std::ranges::lexicographical_compare(
      a.rbegin(), a.rend(), b.rbegin(), b.rend());
}

}  // namespace lyra::value
