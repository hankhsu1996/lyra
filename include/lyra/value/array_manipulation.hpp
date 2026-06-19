#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <type_traits>
#include <utility>
#include <vector>

#include "lyra/value/packed_array.hpp"

// LRM 7.12 array manipulation algorithms, shared by every indexable sequence
// container (dynamic array, queue, and -- when wired -- fixed unpacked array).
// The algorithms read the receiver only through `data[i]` and `data.size()`, so
// one template body serves `std::vector` and `std::deque` element storage
// alike. Each container exposes the LRM-named methods as thin wrappers over
// these; the value/index shaping (a result queue of elements vs of `int`) is
// the container's concern, so the algorithms return a plain `std::vector` or a
// scalar and never name a container type.
namespace lyra::value::detail {

// LRM 7.12.1 locator comparison over a key type (the element itself for the
// no-`with` form, or the `with`-expression result otherwise). `PackedArray`
// returns a 1-bit truth value whose X/Z collapses to false in a boolean
// context; `String` / `double` return a plain `bool`.
template <typename K>
[[nodiscard]] auto LocatorKeyLess(const K& a, const K& b) -> bool {
  return static_cast<bool>(a < b);
}

// Uniqueness equality. 4-state integral keys compare bit-exact (LRM 11.4.5
// `===`), so two X-valued keys are the same value while X never equals a known
// bit; non-integral keys have no unknown plane and use value equality.
template <typename K>
[[nodiscard]] auto LocatorKeySame(const K& a, const K& b) -> bool {
  if constexpr (std::is_same_v<K, PackedArray>) {
    return static_cast<bool>(a.CaseEqual(b));
  } else {
    return a.IsBitIdentical(b);
  }
}

// The per-element index handed to every `with`-clause closure. A `PackedArray`
// so `item.index` reads as a regular integral value in SV semantics (LRM
// 7.12.4).
[[nodiscard]] inline auto LocatorIndex(std::size_t i) -> PackedArray {
  return PackedArray::Int(static_cast<int>(i));
}

template <typename Seq>
[[nodiscard]] auto ElementsAtIndices(
    const Seq& data, const std::vector<std::size_t>& indices)
    -> std::vector<typename Seq::value_type> {
  std::vector<typename Seq::value_type> out;
  out.reserve(indices.size());
  for (std::size_t i : indices) {
    out.push_back(data[i]);
  }
  return out;
}

[[nodiscard]] inline auto IndicesAsPacked(
    const std::vector<std::size_t>& indices) -> std::vector<PackedArray> {
  std::vector<PackedArray> out;
  out.reserve(indices.size());
  for (std::size_t i : indices) {
    out.push_back(LocatorIndex(i));
  }
  return out;
}

enum class LocatorScan : std::uint8_t { kAll, kFirst, kLast };

// LRM 7.12.1 find family core: positions satisfying `pred`, collecting every
// match (`kAll`), the leftmost (`kFirst`), or the rightmost (`kLast`).
template <typename Seq, typename F>
[[nodiscard]] auto MatchingIndices(const Seq& data, F& pred, LocatorScan scan)
    -> std::vector<std::size_t> {
  std::vector<std::size_t> out;
  if (scan == LocatorScan::kLast) {
    for (std::size_t i = data.size(); i-- > 0;) {
      if (static_cast<bool>(pred(data[i], LocatorIndex(i)))) {
        out.push_back(i);
        break;
      }
    }
    return out;
  }
  for (std::size_t i = 0; i < data.size(); ++i) {
    if (static_cast<bool>(pred(data[i], LocatorIndex(i)))) {
      out.push_back(i);
      if (scan == LocatorScan::kFirst) {
        break;
      }
    }
  }
  return out;
}

// LRM 7.12.1 min / max core: the single position whose key the `prefer`
// comparator ranks first. Empty receiver yields no position.
template <typename Seq, typename F, typename Prefer>
[[nodiscard]] auto ExtremumIndex(const Seq& data, F& key, Prefer prefer)
    -> std::vector<std::size_t> {
  std::vector<std::size_t> out;
  if (!data.empty()) {
    std::size_t pick = 0;
    auto best = key(data[0], LocatorIndex(0));
    for (std::size_t i = 1; i < data.size(); ++i) {
      auto k = key(data[i], LocatorIndex(i));
      if (prefer(k, best)) {
        best = k;
        pick = i;
      }
    }
    out.push_back(pick);
  }
  return out;
}

template <typename K>
[[nodiscard]] auto SeenContains(const std::vector<K>& seen, const K& k)
    -> bool {
  for (const auto& s : seen) {
    if (LocatorKeySame(k, s)) {
      return true;
    }
  }
  return false;
}

// LRM 7.12.1 unique / unique_index core: the position of the first occurrence
// of each distinct key value, in first-seen order.
template <typename Seq, typename F>
[[nodiscard]] auto UniqueRepresentativeIndices(const Seq& data, F& key)
    -> std::vector<std::size_t> {
  using KeyT = std::invoke_result_t<
      F&, const typename Seq::value_type&, const PackedArray&>;
  std::vector<std::size_t> out;
  std::vector<KeyT> seen;
  for (std::size_t i = 0; i < data.size(); ++i) {
    auto k = key(data[i], LocatorIndex(i));
    if (!SeenContains(seen, k)) {
      seen.push_back(k);
      out.push_back(i);
    }
  }
  return out;
}

template <typename Seq, typename F>
[[nodiscard]] auto ArrayFind(const Seq& data, F pred)
    -> std::vector<typename Seq::value_type> {
  return ElementsAtIndices(
      data, MatchingIndices(data, pred, LocatorScan::kAll));
}
template <typename Seq, typename F>
[[nodiscard]] auto ArrayFindIndex(const Seq& data, F pred)
    -> std::vector<PackedArray> {
  return IndicesAsPacked(MatchingIndices(data, pred, LocatorScan::kAll));
}
template <typename Seq, typename F>
[[nodiscard]] auto ArrayFindFirst(const Seq& data, F pred)
    -> std::vector<typename Seq::value_type> {
  return ElementsAtIndices(
      data, MatchingIndices(data, pred, LocatorScan::kFirst));
}
template <typename Seq, typename F>
[[nodiscard]] auto ArrayFindFirstIndex(const Seq& data, F pred)
    -> std::vector<PackedArray> {
  return IndicesAsPacked(MatchingIndices(data, pred, LocatorScan::kFirst));
}
template <typename Seq, typename F>
[[nodiscard]] auto ArrayFindLast(const Seq& data, F pred)
    -> std::vector<typename Seq::value_type> {
  return ElementsAtIndices(
      data, MatchingIndices(data, pred, LocatorScan::kLast));
}
template <typename Seq, typename F>
[[nodiscard]] auto ArrayFindLastIndex(const Seq& data, F pred)
    -> std::vector<PackedArray> {
  return IndicesAsPacked(MatchingIndices(data, pred, LocatorScan::kLast));
}

template <typename Seq, typename F>
[[nodiscard]] auto ArrayMin(const Seq& data, F key)
    -> std::vector<typename Seq::value_type> {
  return ElementsAtIndices(
      data, ExtremumIndex(data, key, [](const auto& a, const auto& b) {
        return LocatorKeyLess(a, b);
      }));
}
template <typename Seq, typename F>
[[nodiscard]] auto ArrayMax(const Seq& data, F key)
    -> std::vector<typename Seq::value_type> {
  return ElementsAtIndices(
      data, ExtremumIndex(data, key, [](const auto& a, const auto& b) {
        return LocatorKeyLess(b, a);
      }));
}
template <typename Seq, typename F>
[[nodiscard]] auto ArrayUnique(const Seq& data, F key)
    -> std::vector<typename Seq::value_type> {
  return ElementsAtIndices(data, UniqueRepresentativeIndices(data, key));
}
template <typename Seq, typename F>
[[nodiscard]] auto ArrayUniqueIndex(const Seq& data, F key)
    -> std::vector<PackedArray> {
  return IndicesAsPacked(UniqueRepresentativeIndices(data, key));
}

// LRM 7.12.2 reverse: in-place reversal. `iter_swap` exchanges element storage
// through the element type's ADL `swap`, which stays shape-safe for
// `PackedArray`.
template <typename Seq>
auto ArrayReverse(Seq& data) -> void {
  std::ranges::reverse(data);
}

// LRM 7.12.2 sort / rsort: in-place ordering by the closure-projected key.
// Selection sort (O(n^2)) rather than `std::ranges::sort` because libstdc++'s
// introsort move-assigns elements, which trips `PackedArray`'s
// shape-preservation invariant; exchanging storage via the ADL `swap` stays
// shape-safe, and test-sized arrays make the asymptotic cost a non-issue. The
// per-element key is materialised once, then the elements move in sync.
template <typename Seq, typename F, typename Compare>
auto ArraySortByKey(Seq& data, F key, Compare cmp) -> void {
  using KeyT = std::invoke_result_t<
      F&, const typename Seq::value_type&, const PackedArray&>;
  std::vector<KeyT> keys;
  keys.reserve(data.size());
  for (std::size_t i = 0; i < data.size(); ++i) {
    keys.push_back(key(data[i], LocatorIndex(i)));
  }
  using std::swap;
  for (std::size_t i = 0; i + 1 < data.size(); ++i) {
    std::size_t pick = i;
    for (std::size_t j = i + 1; j < data.size(); ++j) {
      if (static_cast<bool>(cmp(keys[j], keys[pick]))) {
        pick = j;
      }
    }
    if (pick != i) {
      swap(keys[i], keys[pick]);
      swap(data[i], data[pick]);
    }
  }
}

// LRM 7.12.3 reduction: fold the closure-projected values with `combine`. The
// result type follows the closure's return type, so a width-widening
// `with`-expression (`sum with (int'(item))`) widens the result (LRM 7.12.3).
// An empty receiver yields the closure-shaped zero (slang behaviour; LRM is
// silent), synthesised from the element shield slot.
template <typename Seq, typename T, typename F, typename Combine>
[[nodiscard]] auto ArrayFold(
    const Seq& data, const T& oob_slot, F key, Combine combine)
    -> std::invoke_result_t<F&, const T&, const PackedArray&> {
  if (data.empty()) {
    T zero = oob_slot;
    zero.ResetToDefault();
    auto acc = key(zero, LocatorIndex(0));
    acc.ResetToDefault();
    return acc;
  }
  auto acc = key(data[0], LocatorIndex(0));
  for (std::size_t i = 1; i < data.size(); ++i) {
    auto v = key(data[i], LocatorIndex(i));
    combine(acc, v);
  }
  return acc;
}

// LRM 7.4.5 contiguous-range gather. The slice is `count` elements starting at
// `offset`; an out-of-range position yields `canonical` and an x / z offset
// (an invalid position) makes the whole result canonical. The flat vector is
// wrapped by the caller as a fixed-size unpacked array.
template <typename T>
[[nodiscard]] auto ArraySliceGather(
    const std::vector<T>& data, const T& canonical, const PackedArray& offset,
    std::uint32_t count) -> std::vector<T> {
  std::vector<T> result;
  if (offset.HasUnknown()) {
    result.assign(count, canonical);
    return result;
  }
  result.reserve(count);
  const auto base = offset.ToInt64();
  const auto size = static_cast<std::int64_t>(data.size());
  for (std::uint32_t i = 0; i < count; ++i) {
    const auto pos = base + static_cast<std::int64_t>(i);
    const bool in_bounds = pos >= 0 && pos < size;
    result.push_back(
        in_bounds ? data[static_cast<std::size_t>(pos)] : canonical);
  }
  return result;
}

// LRM 7.6 + 7.4.5 whole-slice scatter. Each of the `count` values lands at
// `offset + i`; an out-of-range position is skipped and an x / z offset
// performs no operation, matching the invalid-index write contract.
template <typename T>
auto ArraySliceScatter(
    std::vector<T>& data, const PackedArray& offset, std::uint32_t count,
    const std::vector<T>& values) -> void {
  if (offset.HasUnknown()) {
    return;
  }
  const auto base = offset.ToInt64();
  const auto size = static_cast<std::int64_t>(data.size());
  for (std::uint32_t i = 0; i < count; ++i) {
    const auto pos = base + static_cast<std::int64_t>(i);
    if (pos < 0 || pos >= size) {
      continue;
    }
    data[static_cast<std::size_t>(pos)] = values[i];
  }
}

}  // namespace lyra::value::detail
