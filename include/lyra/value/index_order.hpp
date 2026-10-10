#pragma once

#include <cstdint>
#include <functional>
#include <string>
#include <vector>

#include "lyra/value/chandle.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/wildcard_index.hpp"

// The order an associative array keeps the indices of each index type in (LRM
// 7.8), stated once for the array compiled with its index type and for the
// type's table the library reads it through.
namespace lyra::value {

// Numerical key ordering for integral-indexed associative arrays (LRM 7.8.4):
// the SystemVerilog `<` operator respects the signedness of the index type, so
// a known 1 means strictly-less. Every key is of the declared index type
// (slang casts each index expression to it), so the comparison is total over
// the keys actually stored.
template <IntegralValue K>
struct IntegralKeyLess {
  [[nodiscard]] auto operator()(const K& a, const K& b) const -> bool {
    return (a < b).IsTruthy();
  }
};

// LRM 7.8.2 string-keyed associative array: keys order lexicographically. The
// value-type `<` answers a one-bit value; the host predicate `std::map` needs
// is recovered with the explicit-bool conversion.
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
  template <IntegralValue I>
  WildcardKey(const I& index)  // NOLINT(google-explicit-constructor)
      : WildcardKey(index.Load().View()) {
  }
  explicit WildcardKey(const ConstIntegralView& index)
      : value_(index.planes.value.begin(), index.planes.value.end()),
        unknown_(index.planes.unknown.begin(), index.planes.unknown.end()),
        width_(index.width),
        order_(WildcardIndexWords(index)) {
  }

  // The index as the program wrote it, read as unsigned.
  [[nodiscard]] auto View() const -> ConstIntegralView {
    return ConstIntegralView{
        .planes = ConstPlanes{.value = value_, .unknown = unknown_},
        .width = width_,
        .signedness = Signedness::kUnsigned};
  }
  [[nodiscard]] auto Order() const -> const std::vector<std::uint64_t>& {
    return order_;
  }
  [[nodiscard]] auto HasUnknown() const -> bool {
    return lyra::value::HasUnknown(View().planes);
  }

 private:
  std::vector<std::uint64_t> value_;
  std::vector<std::uint64_t> unknown_;
  std::uint64_t width_;
  std::vector<std::uint64_t> order_;
};

struct WildcardKeyLess {
  [[nodiscard]] auto operator()(
      const WildcardKey& a, const WildcardKey& b) const -> bool {
    return WildcardIndexBefore(a.Order(), b.Order());
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
template <IntegralValue K>
struct AssocKeyTraits<K> {
  using Less = IntegralKeyLess<K>;
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
    return FormatIntegralOperand(spec, key.View(), FormatContext{});
  }
};

}  // namespace lyra::value
