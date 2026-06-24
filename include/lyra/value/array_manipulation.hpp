#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <ranges>
#include <type_traits>
#include <vector>

#include "lyra/value/packed_array.hpp"

// LRM 7.12 array manipulation algorithms. The locator / reduction / map family
// runs over a container's entry stream -- a lazily iterated sequence of
// `(index, element)` pairs in the container's natural order. The index is the
// container's index type (an ordinal int for the sequence containers, the
// key for an associative array), so these algorithms never synthesize it and
// never name a container type; they are generic combinators over any range
// whose element is an `Entry`. The container supplies that range (see each
// container's `Entries()`), and shapes the result. The ordering family
// (`reverse` / `sort`) is a separate, sequence-only in-place permutation
// that needs random-access storage; it stays on the positional `Seq&` form
// below.
namespace lyra::value::detail {

// One entry of a container's 7.12 stream: the element by const pointer (a
// Regular non-owning handle, so an Entry composes with every generic algorithm)
// and its index value. The handle is a pointer rather than a reference because
// the algorithms below feed entries through `std::ranges` machinery that
// assumes a Regular (copyable, assignable) value; a reference member would
// forfeit that.
template <typename Index, typename Element>
struct Entry {
  using IndexType = Index;
  using ElementType = Element;
  Index index;
  const Element* element;
};

template <typename R>
using EntryOf = std::ranges::range_value_t<std::remove_cvref_t<R>>;
template <typename R>
using IndexOf = typename EntryOf<R>::IndexType;
template <typename R>
using ElementOf = typename EntryOf<R>::ElementType;

// LRM 7.12.1 locator comparison over a key type (the element itself for the
// no-`with` form, or the `with`-expression result otherwise). `PackedArray`
// returns a 1-bit truth value whose X/Z collapses to false in a boolean
// context; `String` / `Real` return a plain `bool`.
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

// Collect a lazy view into a vector. The value layer is built under two
// toolchains (the emit clang and the bazel GCC); `std::ranges::to` is absent on
// the GCC libstdc++ the bazel build uses, and a 7.12 result is shaped into an
// SV container by the caller anyway, so the projected values are gathered here.
template <typename V>
[[nodiscard]] auto ToVector(V view)
    -> std::vector<std::ranges::range_value_t<V>> {
  std::vector<std::ranges::range_value_t<V>> out;
  for (auto&& x : view) {
    out.push_back(static_cast<decltype(x)>(x));
  }
  return out;
}

// LRM 7.12.1 find core: the entries satisfying `pred`, as a lazy view. The
// locator family composes over it -- `find` collects every match, `find_first`
// is `take(1)`, `find_last` is `reverse | take(1)` -- so "leftmost" and
// "rightmost" are view adaptors rather than a hand-rolled scan mode.
template <typename R, typename Pred>
[[nodiscard]] auto Matching(R entries, Pred pred) {
  return entries | std::views::filter([pred](const auto& e) {
           return static_cast<bool>(pred(*e.element, e.index));
         });
}

template <typename R, typename F>
[[nodiscard]] auto ArrayFind(R entries, F pred) -> std::vector<ElementOf<R>> {
  return ToVector(
      Matching(entries, pred) |
      std::views::transform([](const auto& e) { return *e.element; }));
}
template <typename R, typename F>
[[nodiscard]] auto ArrayFindIndex(R entries, F pred)
    -> std::vector<IndexOf<R>> {
  return ToVector(
      Matching(entries, pred) |
      std::views::transform([](const auto& e) { return e.index; }));
}
template <typename R, typename F>
[[nodiscard]] auto ArrayFindFirst(R entries, F pred)
    -> std::vector<ElementOf<R>> {
  return ToVector(
      Matching(entries, pred) | std::views::take(1) |
      std::views::transform([](const auto& e) { return *e.element; }));
}
template <typename R, typename F>
[[nodiscard]] auto ArrayFindFirstIndex(R entries, F pred)
    -> std::vector<IndexOf<R>> {
  return ToVector(
      Matching(entries, pred) | std::views::take(1) |
      std::views::transform([](const auto& e) { return e.index; }));
}
template <typename R, typename F>
[[nodiscard]] auto ArrayFindLast(R entries, F pred)
    -> std::vector<ElementOf<R>> {
  return ToVector(
      Matching(entries, pred) | std::views::reverse | std::views::take(1) |
      std::views::transform([](const auto& e) { return *e.element; }));
}
template <typename R, typename F>
[[nodiscard]] auto ArrayFindLastIndex(R entries, F pred)
    -> std::vector<IndexOf<R>> {
  return ToVector(
      Matching(entries, pred) | std::views::reverse | std::views::take(1) |
      std::views::transform([](const auto& e) { return e.index; }));
}

// LRM 7.12.1 min / max: the element of the entry whose projected key is least
// (`min`) or greatest (`max`), first on ties. Empty receiver yields an empty
// queue.
template <typename R, typename F>
[[nodiscard]] auto ArrayMin(R entries, F key) -> std::vector<ElementOf<R>> {
  auto proj = [&](const auto& e) { return key(*e.element, e.index); };
  auto cmp = [](const auto& a, const auto& b) { return LocatorKeyLess(a, b); };
  std::vector<ElementOf<R>> out;
  auto it = std::ranges::min_element(entries, cmp, proj);
  if (it != std::ranges::end(entries)) {
    auto e = *it;
    out.push_back(*e.element);
  }
  return out;
}
template <typename R, typename F>
[[nodiscard]] auto ArrayMax(R entries, F key) -> std::vector<ElementOf<R>> {
  auto proj = [&](const auto& e) { return key(*e.element, e.index); };
  auto cmp = [](const auto& a, const auto& b) { return LocatorKeyLess(a, b); };
  std::vector<ElementOf<R>> out;
  auto it = std::ranges::max_element(entries, cmp, proj);
  if (it != std::ranges::end(entries)) {
    auto e = *it;
    out.push_back(*e.element);
  }
  return out;
}

// LRM 7.12.1 unique core: the first-occurrence entry of each distinct
// projected-key value, in first-seen order. The standard library offers only
// adjacent de-duplication, so the first-occurrence pass is explicit; the
// element / index split is then the same `transform` the find family uses.
template <typename R, typename F>
[[nodiscard]] auto UniqueEntries(R entries, F key) -> std::vector<EntryOf<R>> {
  using KeyT = std::invoke_result_t<F&, const ElementOf<R>&, const IndexOf<R>&>;
  std::vector<EntryOf<R>> out;
  std::vector<KeyT> seen;
  for (const auto& e : entries) {
    auto k = key(*e.element, e.index);
    if (!SeenContains(seen, k)) {
      seen.push_back(k);
      out.push_back(e);
    }
  }
  return out;
}
template <typename R, typename F>
[[nodiscard]] auto ArrayUnique(R entries, F key) -> std::vector<ElementOf<R>> {
  return ToVector(
      UniqueEntries(entries, key) |
      std::views::transform([](const auto& e) { return *e.element; }));
}
template <typename R, typename F>
[[nodiscard]] auto ArrayUniqueIndex(R entries, F key)
    -> std::vector<IndexOf<R>> {
  return ToVector(
      UniqueEntries(entries, key) |
      std::views::transform([](const auto& e) { return e.index; }));
}

// LRM 7.12.3 reduction: fold the closure-projected values with `comb`, seeded
// by the first projected value so an empty receiver yields `proto` (LRM is
// silent on empty input; the producer supplies the result-shaped zero -- see
// the decision doc). The result type follows the closure's return type, so a
// width-widening `with`-expression widens the result.
template <typename R, typename Key, typename Comb, typename Acc>
[[nodiscard]] auto ArrayFold(R entries, Acc proto, Key key, Comb comb) -> Acc {
  auto vals = entries | std::views::transform([&](const auto& e) {
                return key(*e.element, e.index);
              });
  return std::ranges::fold_left_first(vals, comb).value_or(std::move(proto));
}

// LRM 7.12.5 projection into the closure's return type, in entry order. The
// caller pairs the result with each entry's index when the result container is
// keyed (associative); a sequence container drops the index.
template <typename R, typename F>
[[nodiscard]] auto ArrayMap(R entries, F closure) -> std::vector<
    std::invoke_result_t<F&, const ElementOf<R>&, const IndexOf<R>&>> {
  using U = std::invoke_result_t<F&, const ElementOf<R>&, const IndexOf<R>&>;
  std::vector<U> out;
  for (const auto& e : entries) {
    out.push_back(closure(*e.element, e.index));
  }
  return out;
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
// per-element key is materialised once, then the elements move in sync. The
// ordering family is positional (LRM 7.12.2 is sequence-only), so the index
// passed to the key closure is the ordinal position.
template <typename Seq, typename F, typename Compare>
auto ArraySortByKey(Seq& data, F key, Compare cmp) -> void {
  using KeyT = std::invoke_result_t<
      F&, const typename Seq::value_type&, const PackedArray&>;
  std::vector<KeyT> keys;
  keys.reserve(data.size());
  for (std::size_t i = 0; i < data.size(); ++i) {
    keys.push_back(key(data[i], PackedArray::Int(static_cast<int>(i))));
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
