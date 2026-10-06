#pragma once

#include <algorithm>
#include <cstddef>
#include <optional>
#include <type_traits>
#include <utility>
#include <vector>

#include "lyra/value/value_type.hpp"

// LRM 7.12 array manipulation algorithms, written once over positions. Every
// method of the clause asks, per entry of a container in its natural order,
// what the entry's key is -- the element itself, or what a `with` clause
// answers for it -- and answers with positions: which entries a locator found,
// the order a sort puts them in, or the one fold of their keys. The container
// then shapes the answer out of its own elements and indices, so nothing here
// knows what an element or an index is, and one body serves a container
// compiled with its element type and one compiled with that type's table.
//
// What the algorithms ask of a key is answered by the free functions below,
// found by argument-dependent lookup: an order, whether two keys are the same,
// whether a key holds as a condition, and the five reductions.
namespace lyra::value::detail {

// LRM 7.12.1 / 7.12.2: the order keys compare in. A 4-state key carrying x / z
// compares indeterminate, which reads as not before.
template <typename K>
[[nodiscard]] auto KeyBefore(const K& a, const K& b) -> bool {
  return static_cast<bool>(a < b);
}

// LRM 7.12.1 `unique`: whether two keys are one value. Keys compare bit-exact
// (LRM 11.4.5 `===`), so two x-valued keys are the same value while an x never
// equals a known bit.
template <typename K>
[[nodiscard]] auto KeySame(const K& a, const K& b) -> bool {
  return a.IsBitIdentical(b);
}

// LRM 7.12.1: whether a locator's condition holds. An unknown selects nothing.
template <typename K>
[[nodiscard]] auto KeyHolds(const K& condition) -> bool {
  return static_cast<bool>(condition);
}

// LRM 7.12.3: two keys folded by one of the clause's reductions.
template <typename K>
[[nodiscard]] auto KeyReduce(Reduction reduction, const K& a, const K& b) -> K {
  return Reduced(reduction, a, b);
}

// LRM 7.12.1 `find` and its index form: every position whose condition holds,
// in order.
template <typename Condition>
[[nodiscard]] auto MatchingPositions(std::size_t count, Condition condition)
    -> std::vector<std::size_t> {
  std::vector<std::size_t> found;
  for (std::size_t i = 0; i < count; ++i) {
    if (KeyHolds(condition(i))) {
      found.push_back(i);
    }
  }
  return found;
}

// LRM 7.12.1 `find_first` / `find_last`: the leftmost or rightmost such
// position, none where there is none.
template <typename Condition>
[[nodiscard]] auto FirstMatching(std::size_t count, Condition condition)
    -> std::vector<std::size_t> {
  for (std::size_t i = 0; i < count; ++i) {
    if (KeyHolds(condition(i))) {
      return {i};
    }
  }
  return {};
}
template <typename Condition>
[[nodiscard]] auto LastMatching(std::size_t count, Condition condition)
    -> std::vector<std::size_t> {
  for (std::size_t i = count; i-- > 0;) {
    if (KeyHolds(condition(i))) {
      return {i};
    }
  }
  return {};
}

// LRM 7.12.1 `min` / `max`: the position whose key no other key comes before
// under `outranks`, the first of several, none for no entries.
template <typename KeyOf, typename Outranks>
[[nodiscard]] auto ExtremePosition(
    std::size_t count, KeyOf key, Outranks outranks)
    -> std::vector<std::size_t> {
  using K = std::decay_t<decltype(key(std::size_t{0}))>;
  std::vector<std::size_t> best;
  std::optional<K> best_key;
  for (std::size_t i = 0; i < count; ++i) {
    K candidate = key(i);
    if (!best_key.has_value() || outranks(candidate, *best_key)) {
      best = {i};
      best_key = std::move(candidate);
    }
  }
  return best;
}
template <typename KeyOf>
[[nodiscard]] auto LeastPosition(std::size_t count, KeyOf key)
    -> std::vector<std::size_t> {
  return ExtremePosition(
      count, key, [](const auto& a, const auto& b) { return KeyBefore(a, b); });
}
template <typename KeyOf>
[[nodiscard]] auto GreatestPosition(std::size_t count, KeyOf key)
    -> std::vector<std::size_t> {
  return ExtremePosition(
      count, key, [](const auto& a, const auto& b) { return KeyBefore(b, a); });
}

// LRM 7.12.1 `unique`: the first position of each distinct key, in the order
// first seen.
template <typename KeyOf>
[[nodiscard]] auto UniquePositions(std::size_t count, KeyOf key)
    -> std::vector<std::size_t> {
  using K = std::decay_t<decltype(key(std::size_t{0}))>;
  std::vector<std::size_t> found;
  std::vector<K> seen;
  for (std::size_t i = 0; i < count; ++i) {
    K candidate = key(i);
    const bool repeated = std::ranges::any_of(
        seen, [&](const K& s) { return KeySame(candidate, s); });
    if (!repeated) {
      seen.push_back(std::move(candidate));
      found.push_back(i);
    }
  }
  return found;
}

// LRM 7.12.3: every key folded by `reduction`, the first seeding the fold, so
// a fold of no entries is `empty` (LRM is silent on empty input, so the
// producer supplies the answer of the result's shape rather than this
// inventing one). The result is of the key's type, so a widening `with`
// expression widens it.
template <typename KeyOf, typename K>
[[nodiscard]] auto Folded(
    std::size_t count, KeyOf key, Reduction reduction, K empty) -> K {
  if (count == 0) {
    return empty;
  }
  K folded = key(0);
  for (std::size_t i = 1; i < count; ++i) {
    folded = KeyReduce(reduction, folded, key(i));
  }
  return folded;
}

// LRM 7.12.2 reverse: the position each element comes from once the order is
// reversed.
[[nodiscard]] inline auto ReversedPositions(std::size_t count)
    -> std::vector<std::size_t> {
  std::vector<std::size_t> order(count);
  for (std::size_t k = 0; k < count; ++k) {
    order[k] = count - 1 - k;
  }
  return order;
}

// LRM 7.12.2 sort / rsort: the position each element comes from once ordered
// by its key, ascending or descending, equal keys keeping their order. The SV
// comparison over 4-state keys is not a strict weak ordering -- a key carrying
// an x / z compares indeterminate, so `a < b` and `b < a` can both be false
// while a definite `a < c` still holds -- and a sort assuming one may read
// outside its range. A merge reads only within the two runs it merges whatever
// the comparison answers, so this one is defined for any comparison, and costs
// n log n comparisons.
template <typename K>
[[nodiscard]] auto SortedPositions(const std::vector<K>& keys, bool descending)
    -> std::vector<std::size_t> {
  const auto before = [&](std::size_t a, std::size_t b) {
    return descending ? KeyBefore(keys[b], keys[a])
                      : KeyBefore(keys[a], keys[b]);
  };
  const std::size_t count = keys.size();
  std::vector<std::size_t> order(count);
  for (std::size_t k = 0; k < count; ++k) {
    order[k] = k;
  }
  std::vector<std::size_t> merged(count);
  for (std::size_t width = 1; width < count; width *= 2) {
    for (std::size_t lo = 0; lo < count; lo += 2 * width) {
      const std::size_t mid = std::min(lo + width, count);
      const std::size_t hi = std::min(lo + (2 * width), count);
      std::size_t left = lo;
      std::size_t right = mid;
      std::size_t out = lo;
      while (left < mid && right < hi) {
        // The right run's key goes first only where it is strictly before the
        // left's, so equal keys keep their order.
        if (before(order[right], order[left])) {
          merged[out++] = order[right++];
        } else {
          merged[out++] = order[left++];
        }
      }
      while (left < mid) {
        merged[out++] = order[left++];
      }
      while (right < hi) {
        merged[out++] = order[right++];
      }
    }
    std::swap(order, merged);
  }
  return order;
}

// The keys of positions `0..count-1`, in order.
template <typename KeyOf>
[[nodiscard]] auto KeysOf(std::size_t count, KeyOf key) {
  using K = std::decay_t<decltype(key(std::size_t{0}))>;
  std::vector<K> keys;
  keys.reserve(count);
  for (std::size_t i = 0; i < count; ++i) {
    keys.push_back(key(i));
  }
  return keys;
}

}  // namespace lyra::value::detail
