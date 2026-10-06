#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/value/array_manipulation.hpp"
#include "lyra/value/basic_associative_array.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/index_order.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/queue.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/tuple.hpp"
#include "lyra/value/value_type.hpp"

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
// `String` for string-indexed arrays, `PackedArray` for integral-indexed ones
// and `WildcardKey` for the wildcard index. The keys are kept in LRM 7.8 key
// order, so iteration and `%p` formatting follow it and stay deterministic.
template <typename K, typename V>
class AssociativeArray {
 public:
  using KeyType = K;
  using ElementType = V;

  // Sentinel "uninitialized" form -- an empty map holding no element shape yet,
  // used as the declared default state of an associative field before the
  // constructor scope seeds it.
  AssociativeArray() = default;

  // LRM 7.9.11 associative literal `'{key: value, ...}`: seed the map from the
  // (key, value) entries. `element_default` carries the element shape;
  // `user_default` is what a read of an absent key returns (LRM 7.8.6) and the
  // seed for an entry a later write allocates (LRM 7.8.7), which a `default:`
  // clause names and which is otherwise the element type's own default.
  AssociativeArray(
      V element_default, std::span<const Tuple<K, V>> entries,
      const V& user_default)
      : core_(
            StaticKey<K>{}, StaticElem<V>(std::move(element_default)),
            &user_default) {
    for (const auto& entry : entries) {
      core_.Store(
          entry.template Component<0>(), &entry.template Component<1>());
    }
  }

  AssociativeArray(const AssociativeArray&) = default;
  AssociativeArray(AssociativeArray&&) noexcept = default;
  auto operator=(const AssociativeArray&) -> AssociativeArray& = default;
  auto operator=(AssociativeArray&&) noexcept -> AssociativeArray& = default;
  ~AssociativeArray() = default;

  // LRM 7.9.1: num() and size() both return the entry count as an SV int.
  [[nodiscard]] auto Size() const -> PackedArray {
    return PackedArray::Int(static_cast<std::int32_t>(core_.Count()));
  }

  // LRM 7.9.3: exists() yields an SV int 1 / 0.
  [[nodiscard]] auto Exists(const K& key) const -> PackedArray {
    return PackedArray::Int(core_.Exists(key) ? 1 : 0);
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
  [[nodiscard]] auto ElementRef(const K& key, Formation& formed) -> V& {
    return *static_cast<V*>(core_.ElementRef(key, formed));
  }
  [[nodiscard]] auto ElementRef(const K& key) -> V& {
    Formation formed{};
    return ElementRef(key, formed);
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
  // index to report and the query reads `unallocated` -- the index type's
  // default, which is `'x` for a 4-state index type, as LRM 20.7 requires.
  [[nodiscard]] auto MinIndex(const K& unallocated) const -> K {
    return FirstIndex().value_or(unallocated);
  }
  [[nodiscard]] auto MaxIndex(const K& unallocated) const -> K {
    return LastIndex().value_or(unallocated);
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
  [[nodiscard]] auto First(K probe) const -> Tuple<PackedArray, K> {
    return Visited(std::move(probe), FirstIndex());
  }
  [[nodiscard]] auto Last(K probe) const -> Tuple<PackedArray, K> {
    return Visited(std::move(probe), LastIndex());
  }
  [[nodiscard]] auto Next(K probe) const -> Tuple<PackedArray, K> {
    auto visited = NextIndex(probe);
    return Visited(std::move(probe), std::move(visited));
  }
  [[nodiscard]] auto Prev(K probe) const -> Tuple<PackedArray, K> {
    auto visited = PrevIndex(probe);
    return Visited(std::move(probe), std::move(visited));
  }

  // LRM 7.12.3 reduction over the entry stream. LRM 7.12.3 permits reduction on
  // any integral-valued unpacked array, the associative array included; the
  // ordering family (LRM 7.12.2) is excluded and rejected upstream by slang.
  // The closure receives each value and its key; `proto` is the
  // producer-supplied result default returned for an empty array.
  template <typename F, typename R>
  [[nodiscard]] auto Sum(F key, R proto) const -> R {
    return Folded(key, Reduction::kSum, std::move(proto));
  }
  template <typename F, typename R>
  [[nodiscard]] auto Product(F key, R proto) const -> R {
    return Folded(key, Reduction::kProduct, std::move(proto));
  }
  template <typename F, typename R>
  [[nodiscard]] auto And(F key, R proto) const -> R {
    return Folded(key, Reduction::kAnd, std::move(proto));
  }
  template <typename F, typename R>
  [[nodiscard]] auto Or(F key, R proto) const -> R {
    return Folded(key, Reduction::kOr, std::move(proto));
  }
  template <typename F, typename R>
  [[nodiscard]] auto Xor(F key, R proto) const -> R {
    return Folded(key, Reduction::kXor, std::move(proto));
  }

  // LRM 7.12.1 locator family. Value locators return a queue of values; index
  // locators return a queue of the KEY, since an associative receiver's index
  // is its key (LRM 7.12.1), not an ordinal int. Both seed the result with the
  // producer-supplied `proto`.
  template <typename F>
  [[nodiscard]] auto Find(F pred, V proto) const -> Queue<V> {
    const Snapshot entries = Entries();
    return Values(
        entries, std::move(proto),
        detail::MatchingPositions(entries.size(), KeyOf(entries, pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindIndex(F pred, K proto) const -> Queue<K> {
    const Snapshot entries = Entries();
    return Keys(
        entries, std::move(proto),
        detail::MatchingPositions(entries.size(), KeyOf(entries, pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindFirst(F pred, V proto) const -> Queue<V> {
    const Snapshot entries = Entries();
    return Values(
        entries, std::move(proto),
        detail::FirstMatching(entries.size(), KeyOf(entries, pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindFirstIndex(F pred, K proto) const -> Queue<K> {
    const Snapshot entries = Entries();
    return Keys(
        entries, std::move(proto),
        detail::FirstMatching(entries.size(), KeyOf(entries, pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindLast(F pred, V proto) const -> Queue<V> {
    const Snapshot entries = Entries();
    return Values(
        entries, std::move(proto),
        detail::LastMatching(entries.size(), KeyOf(entries, pred)));
  }
  template <typename F>
  [[nodiscard]] auto FindLastIndex(F pred, K proto) const -> Queue<K> {
    const Snapshot entries = Entries();
    return Keys(
        entries, std::move(proto),
        detail::LastMatching(entries.size(), KeyOf(entries, pred)));
  }
  template <typename F>
  [[nodiscard]] auto Min(F key, V proto) const -> Queue<V> {
    const Snapshot entries = Entries();
    return Values(
        entries, std::move(proto),
        detail::LeastPosition(entries.size(), KeyOf(entries, key)));
  }
  template <typename F>
  [[nodiscard]] auto Max(F key, V proto) const -> Queue<V> {
    const Snapshot entries = Entries();
    return Values(
        entries, std::move(proto),
        detail::GreatestPosition(entries.size(), KeyOf(entries, key)));
  }
  template <typename F>
  [[nodiscard]] auto Unique(F key, V proto) const -> Queue<V> {
    const Snapshot entries = Entries();
    return Values(
        entries, std::move(proto),
        detail::UniquePositions(entries.size(), KeyOf(entries, key)));
  }
  template <typename F>
  [[nodiscard]] auto UniqueIndex(F key, K proto) const -> Queue<K> {
    const Snapshot entries = Entries();
    return Keys(
        entries, std::move(proto),
        detail::UniquePositions(entries.size(), KeyOf(entries, key)));
  }

  // LRM 7.12.5 projection into a same-key associative array: each value maps
  // through the closure and keeps its key, so the result is keyed identically
  // with the `with`-expression's element type. `proto` seeds that element
  // type's canonical default (producer-supplied).
  template <typename F, typename U>
  [[nodiscard]] auto Map(F closure, U proto) const -> AssociativeArray<K, U> {
    std::vector<Tuple<K, U>> pairs;
    for (const auto& [k, value] : Entries()) {
      pairs.emplace_back(*k, closure(*value, *k));
    }
    // Mapping writes no `default:` clause of its own, so what a read of an
    // absent key answers with is the projected element type's own default.
    const U miss = proto;
    return AssociativeArray<K, U>(
        std::move(proto), std::span<const Tuple<K, U>>{pairs}, miss);
  }

  [[nodiscard]] auto operator==(const AssociativeArray& other) const
      -> PackedArray {
    return core_.Equal(other.core_);
  }
  [[nodiscard]] auto operator!=(const AssociativeArray& other) const
      -> PackedArray {
    return !(*this == other);
  }
  [[nodiscard]] auto CaseEqual(const AssociativeArray& other) const
      -> PackedArray {
    return core_.CaseEqual(other.core_);
  }
  [[nodiscard]] auto IsBitIdentical(const AssociativeArray& other) const
      -> bool {
    return core_.IsBitIdentical(other.core_);
  }
  [[nodiscard]] auto HasUnknown() const -> bool {
    return core_.HasUnknown();
  }
  [[nodiscard]] auto IsUnknown() const -> PackedArray {
    return PackedArray::Bit(HasUnknown());
  }
  [[nodiscard]] auto BitstreamWidth() const -> PackedArray {
    return core_.BitstreamWidth();
  }
  [[nodiscard]] auto CountBits(const PackedArray& control_bits) const
      -> PackedArray {
    return core_.CountBits(control_bits);
  }

 private:
  [[nodiscard]] static auto Found(const K* key) -> std::optional<K> {
    if (key == nullptr) {
      return std::nullopt;
    }
    return *key;
  }

  static auto Visited(K probe, std::optional<K> visited)
      -> Tuple<PackedArray, K> {
    if (!visited.has_value()) {
      return Tuple<PackedArray, K>{PackedArray::Int(0), std::move(probe)};
    }
    return Tuple<PackedArray, K>{PackedArray::Int(1), *std::move(visited)};
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

  template <typename F, typename R>
  [[nodiscard]] auto Folded(F& key, Reduction reduction, R empty) const -> R {
    const Snapshot entries = Entries();
    return detail::Folded(
        entries.size(), KeyOf(entries, key), reduction, std::move(empty));
  }

  [[nodiscard]] static auto Values(
      const Snapshot& entries, V proto,
      const std::vector<std::size_t>& positions) -> Queue<V> {
    std::vector<V> found;
    found.reserve(positions.size());
    for (const std::size_t i : positions) {
      found.push_back(*entries[i].second);
    }
    return Queue<V>(std::move(proto), found);
  }
  [[nodiscard]] static auto Keys(
      const Snapshot& entries, K proto,
      const std::vector<std::size_t>& positions) -> Queue<K> {
    std::vector<K> found;
    found.reserve(positions.size());
    for (const std::size_t i : positions) {
      found.push_back(*entries[i].first);
    }
    return Queue<K>(std::move(proto), found);
  }

  BasicAssociativeArray<StaticKey<K>, StaticElem<V>> core_;
};

static_assert(LyraValue<AssociativeArray<String, PackedArray>>);
static_assert(LyraValue<AssociativeArray<PackedArray, PackedArray>>);
static_assert(Sized<AssociativeArray<String, PackedArray>>);
static_assert(BitstreamSizable<AssociativeArray<String, PackedArray>>);
static_assert(AssocIndexable<AssociativeArray<String, PackedArray>, String>);
static_assert(IndexTraversal<AssociativeArray<String, PackedArray>, String>);
static_assert(
    IndexTraversal<AssociativeArray<PackedArray, PackedArray>, PackedArray>);

}  // namespace lyra::value
