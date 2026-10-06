#pragma once

#include <functional>
#include <string>

#include "lyra/value/chandle.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/wildcard_index.hpp"

// The order an associative array keeps the indices of each index type in (LRM
// 7.8), stated once for the array compiled with its index type and for the
// type's table the library reads it through.
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

// LRM 7.8.1 wildcard index `[*]`: a key this container holds normalized, so
// that the per-comparison work is the comparison alone.
//
// The conversion from a plain index value is implicit so that every key-taking
// operation (element access, `exists`, `delete`) accepts the index expression
// directly, with no caller and no per-call-site wrap to distinguish a wildcard
// array from a string- or integral-keyed one.
class WildcardKey {
 public:
  WildcardKey(const PackedArray& index)  // NOLINT(google-explicit-constructor)
      : value_(WildcardIndexValue(index)) {
  }

  [[nodiscard]] auto Value() const -> const PackedArray& {
    return value_;
  }
  [[nodiscard]] auto HasUnknown() const -> bool {
    return value_.HasUnknown();
  }

 private:
  PackedArray value_;
};

struct WildcardKeyLess {
  [[nodiscard]] auto operator()(
      const WildcardKey& a, const WildcardKey& b) const -> bool {
    return WildcardIndexBefore(a.Value(), b.Value());
  }
};

// LRM 6.14: a chandle may key an associative array, and the relative ordering
// of two entries is explicitly allowed to vary between runs. The order is
// therefore a host storage choice, not an SV operator -- `<` is not defined on
// a chandle -- and `std::less` supplies the total order `std::map` needs over
// unrelated pointers.
struct ChandleKeyLess {
  [[nodiscard]] auto operator()(const Chandle& a, const Chandle& b) const
      -> bool {
    return std::less<>{}(a.Ptr(), b.Ptr());
  }
};

// LRM 7.8.3: a class may key an associative array, its entries order
// deterministically but arbitrarily, and null is a valid index. Which object a
// handle names is therefore the order, which no SV operator states -- `<` is
// not defined on a handle -- so `std::less` over the identity supplies the
// total order the storage needs, and null takes its place in it like any
// other.
struct ObjectRefKeyLess {
  [[nodiscard]] auto operator()(const ObjectRef& a, const ObjectRef& b) const
      -> bool {
    return std::less<>{}(a.Handle().Share().get(), b.Handle().Share().get());
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

template <>
struct AssocKeyTraits<WildcardKey> {
  using Less = WildcardKeyLess;
};

template <>
struct AssocKeyTraits<Chandle> {
  using Less = ChandleKeyLess;
};

template <>
struct AssocKeyTraits<ObjectRef> {
  using Less = ObjectRefKeyLess;
};

// A wildcard key formats as its underlying integral value (LRM 21.2.1.6 prints
// associative entries in key order; the key prints in the element format).
template <>
struct Formatter<WildcardKey> {
  static auto Format(const FormatSpec& spec, const WildcardKey& key)
      -> std::string {
    return lyra::value::Format(spec, MakeFormatArg(key.Value()));
  }
};

}  // namespace lyra::value
