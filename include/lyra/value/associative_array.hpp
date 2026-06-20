#pragma once

#include <concepts>
#include <iterator>
#include <map>
#include <optional>
#include <string>
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
// `oob_slot_` seeds the element default (supplied at construction because
// `V = PackedArray` carries runtime shape the C++ type cannot recover; see
// `docs/decisions/runtime-shape-and-default-value.md`). It doubles as the
// LRM 7.8.6 shield: a read of a nonexistent or invalid index returns it, and a
// write through an invalid index lands on it and is discarded.
template <typename K, typename V>
class AssociativeArray {
 public:
  using KeyType = K;
  using ElementType = V;

  // Sentinel "uninitialized" form -- empty map with a default-constructed OOB
  // slot, used as the declared default state of an associative field before
  // the constructor scope seeds the element shape.
  AssociativeArray() = default;

  // Empty map with the shield slot seeded, used for declarations like
  // `int m[string];` where the element shape is known at lowering time.
  explicit AssociativeArray(V oob_slot) : oob_slot_(std::move(oob_slot)) {
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
  // is itself the OOB shield slot of an outer container, the outer restores it
  // to canonical state before handing out a reference.
  auto ResetToDefault() -> void {
    data_.clear();
  }

  // LRM 7.8.6: a read of a nonexistent or invalid index returns the element
  // default without allocating. The shield slot is restored first so a prior
  // discarded write does not leak into the result.
  [[nodiscard]] auto Element(const K& key) const -> const V& {
    if (IsInvalidKey(key)) {
      oob_slot_.ResetToDefault();
      return oob_slot_;
    }
    auto it = data_.find(key);
    if (it == data_.end()) {
      oob_slot_.ResetToDefault();
      return oob_slot_;
    }
    return it->second;
  }

  // LRM 7.8.7: a write target allocates the entry with the element default if
  // absent, then yields a reference the caller stores into. An invalid index
  // (LRM 7.8.6) yields the shield slot instead, so the write is discarded.
  [[nodiscard]] auto ElementRef(const K& key) -> V& {
    if (IsInvalidKey(key)) {
      oob_slot_.ResetToDefault();
      return oob_slot_;
    }
    auto it = data_.find(key);
    if (it == data_.end()) {
      it = data_.emplace(key, oob_slot_).first;
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

  // LRM 9.4.2 update event predicate (engine change-detection hook): same key
  // set and each paired value is bit-identical.
  [[nodiscard]] auto IsBitIdentical(const AssociativeArray& other) const
      -> bool {
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

  mutable V oob_slot_;
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
