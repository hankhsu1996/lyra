#pragma once

#include <cstddef>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// Runtime representation of the SystemVerilog `chandle` (LRM 6.14): storage for
// a pointer passed across the DPI-C boundary, at least wide enough to hold a
// host pointer. The value is opaque to SystemVerilog, which may only compare
// two chandles, compare one against `null`, and test one for a boolean value.
//
// A chandle is a value type rather than a bare host pointer because it appears
// as an associative-array element, a class member, and a struct field, and
// every such element must satisfy `LyraValue`. Its operator surface therefore
// follows LRM Table 11-1's "Any data type" row: equality yields a 1-bit
// `PackedArray`, as it does for every other value type. LRM 6.14 gives `===` /
// `!==` the same semantics as `==` / `!=`, so `CaseEqualComparable` holds and
// `CaseEqual` shares pointer identity with `IsBitIdentical`. Relational
// operators are not defined on a chandle, so `Ordered` does not.
//
// There is deliberately no `Formatter<Chandle>`: LRM 6.14 permits a chandle
// only in the equality family and a boolean test, so a chandle never reaches a
// format argument -- HIR-to-MIR rejects that program.
class Chandle {
 public:
  Chandle() = default;

  // `null` is the only literal a chandle admits, and it arrives as the host
  // `nullptr` that a null literal renders to. The conversion is implicit so a
  // comparison against `null` and a store of `null` each bind without a cast.
  // The parameter is a type tag carrying no value, so it stays unnamed.
  // NOLINTNEXTLINE(google-explicit-constructor,readability-named-parameter)
  Chandle(std::nullptr_t) {
  }

  explicit Chandle(void* p) : p_(p) {
  }

  // The foreign-side view of the handle, read when it crosses the DPI-C
  // boundary as its `void*` carrier.
  [[nodiscard]] auto Ptr() const -> void* {
    return p_;
  }

  // LRM 11.4.5 `==` / `!=` (Any data type), compared as pointer identity.
  [[nodiscard]] auto operator==(const Chandle& o) const -> PackedArray {
    return PackedArray::Bit(p_ == o.p_);
  }
  [[nodiscard]] auto operator!=(const Chandle& o) const -> PackedArray {
    return PackedArray::Bit(p_ != o.p_);
  }

  // LRM 6.14: `===` / `!==` on a chandle carry the same semantics as `==` /
  // `!=`. Coincides with `IsBitIdentical` because a chandle's value is its bit
  // pattern.
  [[nodiscard]] auto CaseEqual(const Chandle& o) const -> PackedArray {
    return PackedArray::Bit(p_ == o.p_);
  }

  // LRM 9.4.2 update event predicate (engine change-detection hook).
  [[nodiscard]] auto IsBitIdentical(const Chandle& o) const -> bool {
    return p_ == o.p_;
  }

  // A chandle holds a pointer and has no X/Z plane.
  [[nodiscard]] static auto HasUnknown() -> bool {
    return false;
  }

  // LRM 6.14: a chandle is always initialized to `null`. Satisfies the
  // container OOB-shield contract so a chandle can be an associative-array
  // element.
  auto ResetToDefault() -> void {
    p_ = nullptr;
  }

  // LRM 6.14: a chandle tested for a boolean value is 0 when null, 1 otherwise.
  explicit operator bool() const {
    return p_ != nullptr;
  }

 private:
  void* p_ = nullptr;
};

static_assert(LyraValue<Chandle>);
static_assert(CaseEqualComparable<Chandle>);
static_assert(!Ordered<Chandle>);
static_assert(!WildcardComparable<Chandle>);

}  // namespace lyra::value
