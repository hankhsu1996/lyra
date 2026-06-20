#pragma once

#include <bit>
#include <cmath>
#include <cstdint>
#include <type_traits>

#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/concepts.hpp"

namespace lyra::value {

// Runtime representation of the SystemVerilog real family (LRM 6.12).
// `RealValue<double>` is `real` / `realtime` (LRM 6.12.1 makes them one type);
// `RealValue<float>` is `shortreal`. The two differ only in host precision, so
// one template carries both. The operator surface mirrors LRM Table 11-1: real
// satisfies the universal-equality and relational families but NOT case
// equality (`===` / `!==` are excluded from real operands), so this type has no
// `CaseEqual` and does not model `CaseEqualComparable`.
template <typename Host>
class RealValue {
 public:
  using HostType = Host;

  RealValue() = default;
  explicit RealValue(Host v) : v_(v) {
  }

  // LRM 6.12.1: a `shortreal` <-> `real` reshape is a host float-precision
  // cast. Explicit so the two precisions never convert implicitly.
  template <typename Other>
  explicit RealValue(const RealValue<Other>& o)
      : v_(static_cast<Host>(o.Value())) {
  }

  [[nodiscard]] auto Value() const -> Host {
    return v_;
  }

  // LRM 6.12.1: integer-to-real keeps the integer value (the caller collapses
  // X/Z bits to 0 before this); real-to-integer rounds to nearest, ties away
  // from zero, which is `std::llround`.
  [[nodiscard]] static auto FromInt64(std::int64_t i) -> RealValue {
    return RealValue{static_cast<Host>(i)};
  }
  [[nodiscard]] auto Round() const -> std::int64_t {
    return std::llround(v_);
  }

  // LRM 11.3.1 arithmetic on real produces real.
  [[nodiscard]] auto operator+(const RealValue& o) const -> RealValue {
    return RealValue{static_cast<Host>(v_ + o.v_)};
  }
  [[nodiscard]] auto operator-(const RealValue& o) const -> RealValue {
    return RealValue{static_cast<Host>(v_ - o.v_)};
  }
  [[nodiscard]] auto operator*(const RealValue& o) const -> RealValue {
    return RealValue{static_cast<Host>(v_ * o.v_)};
  }
  [[nodiscard]] auto operator/(const RealValue& o) const -> RealValue {
    return RealValue{static_cast<Host>(v_ / o.v_)};
  }
  [[nodiscard]] auto operator-() const -> RealValue {
    return RealValue{static_cast<Host>(-v_)};
  }
  [[nodiscard]] auto Pow(const RealValue& o) const -> RealValue {
    return RealValue{static_cast<Host>(std::pow(v_, o.v_))};
  }

  // LRM 11.4.1 Table 11-1: `+= -= *= /=` apply to real operands.
  auto operator+=(const RealValue& o) -> RealValue& {
    v_ = static_cast<Host>(v_ + o.v_);
    return *this;
  }
  auto operator-=(const RealValue& o) -> RealValue& {
    v_ = static_cast<Host>(v_ - o.v_);
    return *this;
  }
  auto operator*=(const RealValue& o) -> RealValue& {
    v_ = static_cast<Host>(v_ * o.v_);
    return *this;
  }
  auto operator/=(const RealValue& o) -> RealValue& {
    v_ = static_cast<Host>(v_ / o.v_);
    return *this;
  }

  // LRM 11.4.2: increment / decrement on a real operand changes it by 1.0.
  auto operator++() -> RealValue& {
    v_ = static_cast<Host>(v_ + 1);
    return *this;
  }
  auto operator--() -> RealValue& {
    v_ = static_cast<Host>(v_ - 1);
    return *this;
  }
  auto operator++(int) -> RealValue {
    RealValue old = *this;
    ++(*this);
    return old;
  }
  auto operator--(int) -> RealValue {
    RealValue old = *this;
    --(*this);
    return old;
  }

  // LRM 11.4.4 relational: a 1-bit 2-state result (real carries no x/z).
  [[nodiscard]] auto operator<(const RealValue& o) const -> PackedArray {
    return PackedArray::Bit(v_ < o.v_);
  }
  [[nodiscard]] auto operator<=(const RealValue& o) const -> PackedArray {
    return PackedArray::Bit(v_ <= o.v_);
  }
  [[nodiscard]] auto operator>(const RealValue& o) const -> PackedArray {
    return PackedArray::Bit(v_ > o.v_);
  }
  [[nodiscard]] auto operator>=(const RealValue& o) const -> PackedArray {
    return PackedArray::Bit(v_ >= o.v_);
  }

  // LRM 11.4.5 `==` / `!=` (Any data type), compared as real values.
  [[nodiscard]] auto operator==(const RealValue& o) const -> PackedArray {
    return PackedArray::Bit(v_ == o.v_);
  }
  [[nodiscard]] auto operator!=(const RealValue& o) const -> PackedArray {
    return PackedArray::Bit(v_ != o.v_);
  }

  // LRM 9.4.2 update event predicate (engine change-detection hook): compares
  // the raw bit pattern, so +0.0 / -0.0 differ and a NaN classifies by its
  // bits, both required for "did the storage cell's bits change". This is an
  // engine hook, not an SV operator: real has no LRM `===` (Table 11-1 excludes
  // it), so there is no `CaseEqual` here for it to coincide with or diverge
  // from.
  [[nodiscard]] auto IsBitIdentical(const RealValue& o) const -> bool {
    return std::bit_cast<BitsType>(v_) == std::bit_cast<BitsType>(o.v_);
  }

  // LRM Table 6-7: the real default is 0.0. This is the container OOB-shield
  // contract (docs/decisions/runtime-shape-and-default-value.md), so a real can
  // be an unpacked-array element.
  auto ResetToDefault() -> void {
    v_ = Host{0};
  }

  // LRM 11.4.7 / 12.4: a real in a boolean context is true when non-zero.
  explicit operator bool() const {
    return v_ != Host{0};
  }

 private:
  using BitsType =
      std::conditional_t<sizeof(Host) == 8, std::uint64_t, std::uint32_t>;

  Host v_ = Host{0};
};

using Real = RealValue<double>;
using ShortReal = RealValue<float>;
using RealTime = Real;

// LRM 21.2.1 real formatting. Delegates to the host-precision formatter
// (`Formatter<double>` / `Formatter<float>`); the %f / %e / %g precision comes
// from the spec, not the host width.
template <typename Host>
struct Formatter<RealValue<Host>> {
  static auto Format(
      const FormatSpec& spec, const RealValue<Host>& value,
      const FormatContext& ctx) -> std::string {
    return Formatter<Host>::Format(spec, value.Value(), ctx);
  }
};

static_assert(LyraValueType<Real>);
static_assert(LyraValueType<ShortReal>);
static_assert(Ordered<Real>);
static_assert(Ordered<ShortReal>);
static_assert(!CaseEqualComparable<Real>);
static_assert(!CaseEqualComparable<ShortReal>);

}  // namespace lyra::value
