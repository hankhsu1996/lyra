#pragma once

#include <cstddef>
#include <functional>

namespace lyra::base {

// Folds `value` into `seed`. The mixing constant and shifts are Boost's
// `hash_combine`, which spreads the low-entropy inputs a pool keys on -- small
// ids, enum tags, container sizes -- across the whole word.
//
// Every pool in the compiler mixes its entries here, so two of them cannot
// drift onto different constants or different word widths.
inline void HashCombine(std::size_t& seed, std::size_t value) {
  seed ^= value + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
}

// The same over a value of any hashable type, which is what a caller folding
// one field of an entry has in hand.
template <typename T>
void HashField(std::size_t& seed, const T& value) {
  HashCombine(seed, std::hash<T>{}(value));
}

}  // namespace lyra::base
