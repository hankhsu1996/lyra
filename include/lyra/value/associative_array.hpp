#pragma once

#include <cstddef>
#include <functional>
#include <map>
#include <string>
#include <utility>

#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_concept.hpp"

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

template <typename K>
struct AssocKeyTraits;

// LRM 7.8.2: string keys order lexicographically.
template <>
struct AssocKeyTraits<String> {
  using Less = std::less<String>;
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

  // LRM 7.9.1: num() and size() both return the number of entries.
  [[nodiscard]] auto Size() const -> std::size_t {
    return data_.size();
  }

  // LRM 7.9.3: an element exists at this key. An invalid integral key never
  // matches an entry, so it reports absent.
  [[nodiscard]] auto Exists(const K& key) const -> bool {
    if (IsInvalidKey(key)) {
      return false;
    }
    return data_.contains(key);
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
  [[nodiscard]] auto Read(const K& key) const -> const V& {
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

 private:
  [[nodiscard]] auto IsInvalidKey(const K& key) const -> bool {
    if constexpr (std::same_as<K, PackedArray>) {
      return key.HasUnknown();
    } else {
      return false;
    }
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

}  // namespace lyra::value
