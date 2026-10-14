#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <type_traits>
#include <utility>
#include <vector>

#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/basic_associative_array.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/index_order.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/queue.hpp"
#include "lyra/value/reduction.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/tuple.hpp"

namespace lyra::value {

// The index type `K` of an associative array compiled with it: a key is a `K`,
// ordered as its traits say. LRM 7.8.6 makes a key carrying x or z name no
// entry, decided by the key's own x/z predicate where it has one (an integral
// or wildcard key reports its unknown bits; a string's is always false); a key
// type with no notion of x/z is always valid.
template <typename K>
struct StaticKey {
  using Stored = K;
  using Probe = K;
  using Less = typename AssocKeyTraits<K>::Less;
  [[nodiscard]] static auto Order() -> Less {
    return Less{};
  }
  [[nodiscard]] static auto Invalid(const K& key) -> bool {
    if constexpr (requires { key.HasUnknown(); }) {
      return key.HasUnknown();
    } else {
      return false;
    }
  }
  [[nodiscard]] static auto Owned(const K& key) -> K {
    return key;
  }
};

// SystemVerilog associative array (LRM 7.8) of the C++ index type `K` and
// element type `V`: the associative array every index and element type
// shares, with its keys and elements read and written as `K` and `V` and the
// LRM 7.12 methods run over the closures the C++ backend writes. `K` is
// `String` for string-indexed arrays, the index type itself for
// integral-indexed ones and `WildcardKey` for the wildcard index. The keys are
// kept in LRM 7.8 key order, so iteration and `%p` formatting follow it and
// stay deterministic.
template <typename K, typename V>
class AssociativeArray {
 public:
  using KeyType = K;
  using ElementType = V;

  AssociativeArray() = default;

  // LRM 7.9.11 associative literal `'{key: value, ...}`: the (key, value)
  // entries. A read of an absent key returns the element type's default (LRM
  // 7.8.6), which is also what an entry a later write allocates starts as (LRM
  // 7.8.7).
  explicit AssociativeArray(std::span<const Tuple<K, V>> entries) {
    for (const auto& entry : entries) {
      core_.Store(
          entry.template Component<0>(), &entry.template Component<1>());
    }
  }

  // LRM 7.9.11: this array's entries in one whose absent keys read a copy of
  // `stated_default`, and whose entries a later write allocates start as one
  // (LRM 7.8.7), as a literal's `default:` clause states.
  [[nodiscard]] auto WithDefault(V stated_default) const -> AssociativeArray {
    AssociativeArray result;
    result.core_ = core_.WithDefault(&stated_default);
    return result;
  }

  AssociativeArray(const AssociativeArray&) = default;
  AssociativeArray(AssociativeArray&&) noexcept = default;
  auto operator=(const AssociativeArray&) -> AssociativeArray& = default;
  auto operator=(AssociativeArray&&) noexcept -> AssociativeArray& = default;
  ~AssociativeArray() = default;

  // LRM 7.9.1: num() and size() both return the entry count as an SV int.
  [[nodiscard]] auto Size() const -> Int {
    return Int::FromInt(static_cast<std::int64_t>(core_.Count()));
  }

  // LRM 7.9.3: exists() yields an SV int 1 / 0.
  [[nodiscard]] auto Exists(const K& key) const -> Int {
    return Int::FromInt(core_.Exists(key) ? 1 : 0);
  }

  // LRM 7.9.2: clearing the whole array and deleting the one element a key
  // names (no warning if absent) are two requests the source spells with one
  // word, so each has a name of its own.
  auto Delete() -> void {
    core_.Clear();
  }
  auto DeleteIndex(const K& key) -> void {
    core_.Erase(key);
  }

  // LRM 7.8.6 / 7.9.11: a read of a nonexistent or invalid key returns the
  // array's own value without allocating.
  [[nodiscard]] auto Element(const K& key) const -> const V& {
    return *static_cast<const V*>(core_.ElementAt(key));
  }

  // LRM 7.8.7 / 7.9.11: a write target allocates the absent entry, and an
  // invalid key (LRM 7.8.6) lands where the write is discarded.
  [[nodiscard]] auto ElementMut(const K& key, Formation& formed) -> V& {
    return *static_cast<V*>(core_.ElementMut(key, formed));
  }
  [[nodiscard]] auto ElementMut(const K& key) -> V& {
    Formation formed{};
    return ElementMut(key, formed);
  }

  template <typename Fn>
  auto ForEachEntry(const Fn& fn) const -> void {
    for (const auto& [key, slot] : core_.Entries()) {
      fn(key, *static_cast<const V*>(slot));
    }
  }

  // LRM 7.9.4 / 7.9.5: the smallest / largest stored index, or absent when the
  // array is empty.
  [[nodiscard]] auto FirstIndex() const -> std::optional<K> {
    return Found(core_.FirstKey());
  }
  [[nodiscard]] auto LastIndex() const -> std::optional<K> {
    return Found(core_.LastKey());
  }

  // LRM 20.7 `$low` / `$high` over an associative dimension: the smallest and
  // largest currently allocated index. With none allocated the dimension has no
  // index to report and the query reads the index type's default, which is
  // `'x` for a 4-state index type, as LRM 20.7 requires.
  [[nodiscard]] auto MinIndex() const -> K {
    return FirstIndex().value_or(detail::DefaultOf<K>());
  }
  [[nodiscard]] auto MaxIndex() const -> K {
    return LastIndex().value_or(detail::DefaultOf<K>());
  }

  // LRM 7.9.6 / 7.9.7: the smallest stored index strictly greater than `probe`
  // (next) or the largest strictly less (prev), or absent when none exists.
  [[nodiscard]] auto NextIndex(const K& probe) const -> std::optional<K> {
    return Found(core_.KeyAfter(probe));
  }
  [[nodiscard]] auto PrevIndex(const K& probe) const -> std::optional<K> {
    return Found(core_.KeyBefore(probe));
  }

  // LRM 7.9.4 -- 7.9.7 traversal: the SV int answer paired with the index
  // visited, which is `probe` unchanged when there is no such index (empty
  // array, or no next / prev). `First` / `Last` ignore `probe`'s value;
  // `Next` / `Prev` read it as the search bound. These are pure value queries
  // -- firing the index variable's LRM 4.3 update event is the caller's
  // separate write-back assignment, not this query's concern.
  [[nodiscard]] auto First(K probe) const -> Tuple<Int, K> {
    return Visited(std::move(probe), FirstIndex());
  }
  [[nodiscard]] auto Last(K probe) const -> Tuple<Int, K> {
    return Visited(std::move(probe), LastIndex());
  }
  [[nodiscard]] auto Next(K probe) const -> Tuple<Int, K> {
    auto visited = NextIndex(probe);
    return Visited(std::move(probe), std::move(visited));
  }
  [[nodiscard]] auto Prev(K probe) const -> Tuple<Int, K> {
    auto visited = PrevIndex(probe);
    return Visited(std::move(probe), std::move(visited));
  }

  // LRM 7.12.3 reduction over the entry stream. LRM 7.12.3 permits reduction on
  // any integral-valued unpacked array, the associative array included; the
  // ordering family (LRM 7.12.2) is excluded and rejected upstream by slang.
  // The closure receives each value and its key; an empty array answers the
  // default of the closure's result type.
  template <typename F>
  [[nodiscard]] auto Sum(F key) const {
    return Folded(key, Reduction::kSum);
  }
  template <typename F>
  [[nodiscard]] auto Product(F key) const {
    return Folded(key, Reduction::kProduct);
  }
  template <typename F>
  [[nodiscard]] auto And(F key) const {
    return Folded(key, Reduction::kAnd);
  }
  template <typename F>
  [[nodiscard]] auto Or(F key) const {
    return Folded(key, Reduction::kOr);
  }
  template <typename F>
  [[nodiscard]] auto Xor(F key) const {
    return Folded(key, Reduction::kXor);
  }

  // LRM 7.12.1 locator family. Value locators return a queue of values; index
  // locators return a queue of the KEY, since an associative receiver's index
  // is its key (LRM 7.12.1), not an ordinal int.
  template <typename F>
  [[nodiscard]] auto Find(F pred) const -> Queue<V> {
    const Snapshot entries = Entries();
    return Values(
        entries,
        detail::MatchingPositions(entries.size(), KeyOf(entries, pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindIndex(F pred) const -> Queue<K> {
    const Snapshot entries = Entries();
    return Keys(
        entries,
        detail::MatchingPositions(entries.size(), KeyOf(entries, pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindFirst(F pred) const -> Queue<V> {
    const Snapshot entries = Entries();
    return Values(
        entries, detail::FirstMatching(entries.size(), KeyOf(entries, pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindFirstIndex(F pred) const -> Queue<K> {
    const Snapshot entries = Entries();
    return Keys(
        entries, detail::FirstMatching(entries.size(), KeyOf(entries, pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindLast(F pred) const -> Queue<V> {
    const Snapshot entries = Entries();
    return Values(
        entries, detail::LastMatching(entries.size(), KeyOf(entries, pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindLastIndex(F pred) const -> Queue<K> {
    const Snapshot entries = Entries();
    return Keys(
        entries, detail::LastMatching(entries.size(), KeyOf(entries, pred)));
  }
  template <typename F>
  [[nodiscard]] auto Min(F key) const -> Queue<V> {
    const Snapshot entries = Entries();
    return Values(
        entries, detail::LeastPosition(entries.size(), KeyOf(entries, key)));
  }
  template <typename F>
  [[nodiscard]] auto Max(F key) const -> Queue<V> {
    const Snapshot entries = Entries();
    return Values(
        entries, detail::GreatestPosition(entries.size(), KeyOf(entries, key)));
  }
  template <typename F>
  [[nodiscard]] auto Unique(F key) const -> Queue<V> {
    const Snapshot entries = Entries();
    return Values(
        entries, detail::UniquePositions(entries.size(), KeyOf(entries, key)));
  }
  template <typename F>
  [[nodiscard]] auto UniqueIndex(F key) const -> Queue<K> {
    const Snapshot entries = Entries();
    return Keys(
        entries, detail::UniquePositions(entries.size(), KeyOf(entries, key)));
  }

  // LRM 7.12.5 projection into a same-key associative array: each value maps
  // through the closure and keeps its key, so the result is keyed identically
  // with the `with`-expression's element type. Mapping writes no `default:`
  // clause of its own, so an absent key of the result reads that type's
  // default.
  template <typename F>
  [[nodiscard]] auto Map(F closure) const {
    using U = std::decay_t<decltype(closure(
        std::declval<const V&>(), std::declval<const K&>()))>;
    std::vector<Tuple<K, U>> pairs;
    for (const auto& [k, value] : Entries()) {
      pairs.emplace_back(*k, closure(*value, *k));
    }
    return AssociativeArray<K, U>(std::span<const Tuple<K, U>>{pairs});
  }

  [[nodiscard]] auto operator==(const AssociativeArray& other) const ->
      typename StaticElem<V>::Equality {
    return StaticElem<V>::Equality::Filled(core_.Equal(other.core_));
  }
  [[nodiscard]] auto operator!=(const AssociativeArray& other) const ->
      typename StaticElem<V>::Equality {
    return !(*this == other);
  }
  [[nodiscard]] auto CaseEqual(const AssociativeArray& other) const -> Bit {
    return core_.CaseEqual(other.core_);
  }
  [[nodiscard]] auto IsBitIdentical(const AssociativeArray& other) const
      -> bool {
    return core_.IsBitIdentical(other.core_);
  }
  [[nodiscard]] auto HasUnknown() const -> bool {
    return core_.HasUnknown();
  }
  [[nodiscard]] auto IsUnknown() const -> Bit {
    return Bit::FromBool(HasUnknown());
  }
  [[nodiscard]] auto BitstreamWidth() const -> Int {
    return Int::FromInt(core_.BitstreamWidth());
  }
  template <IntegralValue Control>
  [[nodiscard]] auto CountBits(const Control& control_bits) const -> Int {
    return Int::FromInt(core_.CountBits(control_bits));
  }

 private:
  [[nodiscard]] static auto Found(const K* key) -> std::optional<K> {
    if (key == nullptr) {
      return std::nullopt;
    }
    return *key;
  }

  static auto Visited(K probe, std::optional<K> visited) -> Tuple<Int, K> {
    if (!visited.has_value()) {
      return Tuple<Int, K>{Int::FromInt(0), std::move(probe)};
    }
    return Tuple<Int, K>{Int::FromInt(1), *std::move(visited)};
  }

  // The entries an LRM 7.12 method walks, in LRM 7.8 key order, so a method's
  // positions name them. The key is the entry's index, so an index locator
  // yields keys and `item.index` reads the key.
  using Snapshot = std::vector<std::pair<const K*, const V*>>;
  [[nodiscard]] auto Entries() const -> Snapshot {
    Snapshot entries;
    entries.reserve(core_.Count());
    for (const auto& [key, slot] : core_.Entries()) {
      entries.emplace_back(&key, static_cast<const V*>(slot));
    }
    return entries;
  }

  // What `closure` answers for the entry at a position: its value and key.
  template <typename F>
  [[nodiscard]] static auto KeyOf(const Snapshot& entries, F& closure) {
    return [&entries, &closure](std::size_t i) {
      return closure(*entries[i].second, *entries[i].first);
    };
  }

  template <typename F>
  [[nodiscard]] auto Folded(F& key, Reduction reduction) const {
    using R = std::remove_cvref_t<std::invoke_result_t<F&, const V&, const K&>>;
    const Snapshot entries = Entries();
    return detail::Folded(
        entries.size(), KeyOf(entries, key), reduction, detail::DefaultOf<R>());
  }

  [[nodiscard]] static auto Values(
      const Snapshot& entries, const std::vector<std::size_t>& positions)
      -> Queue<V> {
    std::vector<V> found;
    found.reserve(positions.size());
    for (const std::size_t i : positions) {
      found.push_back(*entries[i].second);
    }
    return Queue<V>(std::span<const V>{found});
  }
  [[nodiscard]] static auto Keys(
      const Snapshot& entries, const std::vector<std::size_t>& positions)
      -> Queue<K> {
    std::vector<K> found;
    found.reserve(positions.size());
    for (const std::size_t i : positions) {
      found.push_back(*entries[i].first);
    }
    return Queue<K>(std::span<const K>{found});
  }

  BasicAssociativeArray<StaticKey<K>, StaticElem<V>> core_;
};

static_assert(LyraValue<AssociativeArray<String, LogicVector<4>>>);
static_assert(LyraValue<AssociativeArray<Int, LogicVector<4>>>);
static_assert(Sized<AssociativeArray<String, LogicVector<4>>>);
static_assert(BitstreamSizable<AssociativeArray<String, LogicVector<4>>>);
static_assert(AssocIndexable<AssociativeArray<String, LogicVector<4>>, String>);
static_assert(IndexTraversal<AssociativeArray<String, LogicVector<4>>, String>);
static_assert(IndexTraversal<AssociativeArray<Int, LogicVector<4>>, Int>);

}  // namespace lyra::value
