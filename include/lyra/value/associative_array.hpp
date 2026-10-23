#pragma once

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <utility>

#include "lyra/value/basic_associative_array.hpp"
#include "lyra/value/bit_vec.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/index_order.hpp"
#include "lyra/value/integral.hpp"
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
  // LRM 6.24.3: the elements written below the `filled` most significant
  // positions of `stream`, the lowest index first, and as a run of bits of
  // their own.
  [[nodiscard]] auto WriteToStream(
      const IntegralView& stream, std::uint64_t filled) const -> std::uint64_t {
    return core_.WriteToStream(stream, filled);
  }
  [[nodiscard]] auto ToBitVec() const -> BitVec {
    return StreamOf(*this);
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
