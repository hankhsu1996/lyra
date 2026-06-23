#pragma once

#include <concepts>
#include <iterator>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <utility>

#include "lyra/value/array_case_equal.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::value {

// Numerical key ordering for integral-indexed associative arrays (LRM 7.8.4):
// the SystemVerilog `<` operator respects the shared signedness of the index
// type, so a 1-bit result of 1 means strictly-less. Every key carries the same
// declared index shape (slang casts each index expression to the index type),
// so the comparison is total over the keys actually stored.
struct PackedArrayKeyLess {
  [[nodiscard]] auto operator()(
      const PackedArray& a, const PackedArray& b) const -> bool {
    return static_cast<bool>(a < b);
  }
};

// LRM 7.8.2 string-keyed associative array: keys order lexicographically. The
// value-type `<` returns a 1-bit `PackedArray`; the host predicate `std::map`
// needs is recovered with the explicit-bool conversion.
struct StringKeyLess {
  [[nodiscard]] auto operator()(const String& a, const String& b) const
      -> bool {
    return static_cast<bool>(a < b);
  }
};

template <typename K>
struct AssocKeyTraits;

template <>
struct AssocKeyTraits<String> {
  using Less = StringKeyLess;
};

// LRM 7.8.4: integral keys order by signed/unsigned numerical value.
template <>
struct AssocKeyTraits<PackedArray> {
  using Less = PackedArrayKeyLess;
};

// SystemVerilog associative array (LRM 7.8): a sparse lookup table allocated
// entry-by-entry. `K` is the index type (`String` for string-indexed arrays,
// `PackedArray` for integral-indexed arrays) and `V` the element type. Storage
// is an ordered `std::map` so iteration and `%p` formatting follow the LRM 7.8
// key ordering and stay deterministic.
//
// `element_default_` holds the element-type default -- the LRM Table 7-1 value
// read from a nonexistent array entry. It carries the element's runtime shape
// (supplied at construction because `V = PackedArray` carries shape the C++
// type cannot recover; see `docs/decisions/runtime-shape-and-default-value.md`)
// and is only ever read or copied, never written, so a read returns it
// directly. A read of a nonexistent or invalid key (LRM 7.8.6) returns it
// unless `user_default_` overrides it (LRM 7.9.11). `discard_sink_` is the
// throwaway an invalid-key write lands on, scrubbed to canonical by the
// non-const write path. There is no out-of-bounds concept here: an associative
// array has no index bounds, so the trigger is a missing or invalid key, never
// a boundary.
template <typename K, typename V>
class AssociativeArray {
 public:
  using KeyType = K;
  using ElementType = V;

  // Sentinel "uninitialized" form -- empty map with default-constructed
  // default / sink slots, used as the declared default state of an associative
  // field before the constructor scope seeds the element shape.
  AssociativeArray() = default;

  // Empty map with the default / sink slots seeded, used for declarations like
  // `int m[string];` where the element shape is known at lowering time.
  explicit AssociativeArray(V element_default)
      : element_default_(element_default),
        discard_sink_(std::move(element_default)) {
  }

  // LRM 7.9.11 associative literal `'{key: value, ...}`: seed the map from the
  // (key, value) entries. The default slots still carry the element-type
  // default for a read of an absent key.
  AssociativeArray(V element_default, std::span<const std::tuple<K, V>> entries)
      : element_default_(element_default),
        discard_sink_(std::move(element_default)) {
    for (const auto& [key, value] : entries) {
      data_.insert_or_assign(key, value);
    }
  }

  // LRM 7.9.11 with an explicit `default:`: the persistent fallback that a read
  // of an absent key returns (LRM 7.8.6) and the seed for an entry allocated on
  // a later write (LRM 7.8.7).
  AssociativeArray(
      V element_default, std::span<const std::tuple<K, V>> entries,
      V user_default)
      : element_default_(element_default),
        discard_sink_(std::move(element_default)),
        user_default_(std::move(user_default)) {
    for (const auto& [key, value] : entries) {
      data_.insert_or_assign(key, value);
    }
  }

  AssociativeArray(const AssociativeArray&) = default;
  AssociativeArray(AssociativeArray&&) noexcept = default;
  auto operator=(const AssociativeArray&) -> AssociativeArray& = default;
  auto operator=(AssociativeArray&&) noexcept -> AssociativeArray& = default;
  ~AssociativeArray() = default;

  // LRM 7.9.1: num() and size() both return the entry count as an SV int.
  [[nodiscard]] auto Size() const -> PackedArray {
    return PackedArray::Int(static_cast<std::int32_t>(data_.size()));
  }

  // LRM 7.9.3: exists() yields an SV int 1 / 0. An invalid integral key never
  // matches an entry, so it reports absent.
  [[nodiscard]] auto Exists(const K& key) const -> PackedArray {
    if (IsInvalidKey(key)) {
      return PackedArray::Int(0);
    }
    return PackedArray::Int(data_.contains(key) ? 1 : 0);
  }

  // LRM 7.9.2: delete a single entry (no warning if absent) or, via the
  // no-argument overload, clear the whole array. An invalid key is a no-op.
  auto Delete(const K& key) -> void {
    if (IsInvalidKey(key)) {
      return;
    }
    data_.erase(key);
  }
  auto Delete() -> void {
    data_.clear();
  }

  // LRM Table 6-7: an associative array's default is empty. When this container
  // is itself the canonical-default slot of an outer container, the outer
  // restores it to canonical state before handing out a reference.
  auto ResetToDefault() -> void {
    data_.clear();
  }

  // LRM 7.8.6 / 7.9.11: a read of a nonexistent or invalid key returns the
  // user-specified default if one was set, otherwise the element-type default,
  // without allocating.
  [[nodiscard]] auto Element(const K& key) const -> const V& {
    if (IsInvalidKey(key)) {
      return MissValue();
    }
    auto it = data_.find(key);
    if (it == data_.end()) {
      return MissValue();
    }
    return it->second;
  }

  // LRM 7.8.7 / 7.9.11: a write target allocates the absent entry seeded with
  // the user-specified default if one was set, otherwise the element-type
  // default, then yields a reference the caller stores into. An invalid key
  // (LRM 7.8.6) yields the discard sink instead, so the write is discarded.
  [[nodiscard]] auto ElementRef(const K& key) -> V& {
    if (IsInvalidKey(key)) {
      discard_sink_.ResetToDefault();
      return discard_sink_;
    }
    auto it = data_.find(key);
    if (it == data_.end()) {
      it = data_
               .emplace(
                   key, user_default_.has_value() ? *user_default_
                                                  : element_default_)
               .first;
    }
    return it->second;
  }

  template <typename Fn>
  auto ForEachEntry(const Fn& fn) const -> void {
    for (const auto& [key, value] : data_) {
      fn(key, value);
    }
  }

  // LRM 7.9.4 / 7.9.5: the smallest / largest stored key, or absent when the
  // array is empty.
  [[nodiscard]] auto FirstKey() const -> std::optional<K> {
    if (data_.empty()) {
      return std::nullopt;
    }
    return data_.begin()->first;
  }
  [[nodiscard]] auto LastKey() const -> std::optional<K> {
    if (data_.empty()) {
      return std::nullopt;
    }
    return data_.rbegin()->first;
  }

  // LRM 7.9.6 / 7.9.7: the smallest stored key strictly greater than `probe`
  // (next) or the largest strictly less (prev), or absent when none exists.
  // `probe` shares the stored keys' index type, so the bound lookup runs
  // directly in key space.
  [[nodiscard]] auto NextKey(const K& probe) const -> std::optional<K> {
    auto it = data_.upper_bound(probe);
    if (it == data_.end()) {
      return std::nullopt;
    }
    return it->first;
  }
  [[nodiscard]] auto PrevKey(const K& probe) const -> std::optional<K> {
    auto it = data_.lower_bound(probe);
    if (it == data_.begin()) {
      return std::nullopt;
    }
    return std::prev(it)->first;
  }

  // LRM 7.9.4 -- 7.9.7 traversal: write the visited key into `probe` and return
  // SV int 1, or return 0 and leave `probe` unchanged when there is no such key
  // (empty array, or no next / prev). `First` / `Last` ignore `probe`'s input;
  // `Next` / `Prev` read it as the search bound. These are pure value queries
  // -- firing the index variable's LRM 4.3 update event is the caller's
  // separate write-back assignment, not this query's concern.
  auto First(K& probe) const -> PackedArray {
    return WriteVisited(probe, FirstKey());
  }
  auto Last(K& probe) const -> PackedArray {
    return WriteVisited(probe, LastKey());
  }
  auto Next(K& probe) const -> PackedArray {
    return WriteVisited(probe, NextKey(probe));
  }
  auto Prev(K& probe) const -> PackedArray {
    return WriteVisited(probe, PrevKey(probe));
  }

  // LRM 11.2.2 aggregate equality / 11.4.5: same key set and each paired value
  // compares equal. A different key set or a different size yields 0; matching
  // empties yield 1. `==` / `!=` propagate X / Z through the per-value `==`;
  // `CaseEqual` matches X / Z as values and is deterministic.
  [[nodiscard]] auto operator==(const AssociativeArray& other) const
      -> PackedArray {
    if (data_.size() != other.data_.size()) {
      return PackedArray::FromInt(0, 1, false, false);
    }
    if (data_.empty()) {
      return PackedArray::FromInt(1, 1, false, false);
    }
    PackedArray result = PackedArray::FromInt(1, 1, false, false);
    for (const auto& [k, v] : data_) {
      auto it = other.data_.find(k);
      if (it == other.data_.end()) {
        return PackedArray::FromInt(0, 1, false, false);
      }
      result = result && (v == it->second);
    }
    return result;
  }
  [[nodiscard]] auto operator!=(const AssociativeArray& other) const
      -> PackedArray {
    return !(*this == other);
  }

  [[nodiscard]] auto CaseEqual(const AssociativeArray& other) const
      -> PackedArray {
    if (data_.size() != other.data_.size()) {
      return PackedArray::FromInt(0, 1, false, false);
    }
    if (data_.empty()) {
      return PackedArray::FromInt(1, 1, false, false);
    }
    PackedArray result = PackedArray::FromInt(1, 1, false, false);
    for (const auto& [k, v] : data_) {
      auto it = other.data_.find(k);
      if (it == other.data_.end()) {
        return PackedArray::FromInt(0, 1, false, false);
      }
      result = result && detail::ArrayCaseEqElement(v, it->second);
    }
    return result;
  }

  // LRM 9.4.2 update event predicate (engine change-detection hook): the
  // persistent default, the key set, and each paired value all match. The
  // default is part of the value, so changing it alone is an observable change.
  [[nodiscard]] auto IsBitIdentical(const AssociativeArray& other) const
      -> bool {
    if (user_default_.has_value() != other.user_default_.has_value()) {
      return false;
    }
    if (user_default_.has_value() &&
        !user_default_->IsBitIdentical(*other.user_default_)) {
      return false;
    }
    if (data_.size() != other.data_.size()) {
      return false;
    }
    for (const auto& [k, v] : data_) {
      auto it = other.data_.find(k);
      if (it == other.data_.end()) {
        return false;
      }
      if (!v.IsBitIdentical(it->second)) {
        return false;
      }
    }
    return true;
  }

  // LRM 20.9: any value carrying an unknown bit propagates up. Keys with
  // unknown bits are rejected at write time (LRM 7.8.1), so they cannot
  // appear here.
  [[nodiscard]] auto HasUnknown() const -> bool {
    for (const auto& [k, v] : data_) {
      if (v.HasUnknown()) return true;
    }
    return false;
  }

  [[nodiscard]] auto IsUnknown() const -> PackedArray {
    return PackedArray::Bit(HasUnknown());
  }

 private:
  [[nodiscard]] auto IsInvalidKey(const K& key) const -> bool {
    if constexpr (std::same_as<K, PackedArray>) {
      return key.HasUnknown();
    } else {
      return false;
    }
  }

  static auto WriteVisited(K& probe, const std::optional<K>& key)
      -> PackedArray {
    if (!key.has_value()) {
      return PackedArray::Int(0);
    }
    probe = *key;
    return PackedArray::Int(1);
  }

  // The value a read of an absent or invalid key yields (LRM 7.8.6 / 7.9.11):
  // the persistent user default when one was set, otherwise the element-type
  // default. `element_default_` is never written, so it is returned directly.
  [[nodiscard]] auto MissValue() const -> const V& {
    return user_default_.has_value() ? *user_default_ : element_default_;
  }

  V element_default_;
  V discard_sink_;
  std::optional<V> user_default_;
  std::map<K, V, typename AssocKeyTraits<K>::Less> data_;
};

// LRM 21.2.1.6 aggregate format: `'{key:value, ...}` in key order. Each key and
// value defers to its own `Formatter`, so string keys print quoted and integral
// keys / values print in the element format.
template <typename K, typename V>
struct Formatter<AssociativeArray<K, V>> {
  static auto Format(
      const FormatSpec& spec, const AssociativeArray<K, V>& value)
      -> std::string {
    std::string out = "'{";
    bool first = true;
    value.ForEachEntry([&](const K& key, const V& elem) {
      if (!first) {
        out += ", ";
      }
      first = false;
      out += lyra::value::Format(spec, MakeFormatArg(key));
      out += ":";
      out += lyra::value::Format(spec, MakeFormatArg(elem));
    });
    out += "}";
    return out;
  }
};

static_assert(LyraValue<AssociativeArray<String, PackedArray>>);
static_assert(LyraValue<AssociativeArray<PackedArray, PackedArray>>);
static_assert(Sized<AssociativeArray<String, PackedArray>>);
static_assert(AssocIndexable<AssociativeArray<String, PackedArray>, String>);
static_assert(Defaultable<AssociativeArray<String, PackedArray>>);
static_assert(KeyTraversal<AssociativeArray<String, PackedArray>, String>);
static_assert(
    KeyTraversal<AssociativeArray<PackedArray, PackedArray>, PackedArray>);

}  // namespace lyra::value
