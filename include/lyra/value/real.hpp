#pragma once

#include <bit>
#include <cmath>
#include <cstdint>
#include <string>
#include <type_traits>

#include "lyra/value/concepts.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/packed_array.hpp"

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
  [[nodiscard]] static auto FromInt(std::int64_t i) -> RealValue {
    return RealValue{static_cast<Host>(i)};
  }

  // LRM 6.12.1 cross-precision reshape, named so a conversion states which of
  // the two it is rather than leaving the operand's type to say.
  template <typename Other>
  [[nodiscard]] static auto ConvertFrom(const RealValue<Other>& o)
      -> RealValue {
    return RealValue{o};
  }
  [[nodiscard]] auto Round() const -> std::int64_t {
    return std::llround(v_);
  }

  // LRM 20.5 `$rtoi` drops the fraction instead of rounding it, so a value
  // moves toward zero rather than toward the nearest integer.
  [[nodiscard]] auto Truncate() const -> std::int64_t {
    return static_cast<std::int64_t>(v_);
  }

  // LRM 20.5 `$realtobits` / `$bitstoreal`: the IEEE 754 pattern the value is
  // stored as, carried as an integer. This preserves the bits where LRM 6.12.1
  // conversion preserves the number, so the two are different operations on
  // the same pair of types and a value carried out and back is unchanged.
  [[nodiscard]] auto ToBits() const -> std::int64_t {
    return static_cast<std::int64_t>(std::bit_cast<BitsType>(v_));
  }
  [[nodiscard]] static auto FromBits(std::int64_t bits) -> RealValue {
    return RealValue{std::bit_cast<Host>(static_cast<BitsType>(bits))};
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
  // LRM 11.4.3 `**` on real operands and the LRM 20.8.2 `$pow` row ask for the
  // same operation, so both spellings arrive here.
  [[nodiscard]] auto Pow(const RealValue& o) const -> RealValue {
    return RealValue{static_cast<Host>(std::pow(v_, o.v_))};
  }

  // LRM 20.8.2 Table 20-4 cross-lists each of these with one C standard math
  // library function and defines its behavior to be that function's, edge
  // cases included, so each one delegates rather than deciding anything of its
  // own.
  [[nodiscard]] auto Ln() const -> RealValue {
    return RealValue{static_cast<Host>(std::log(v_))};
  }
  [[nodiscard]] auto Log10() const -> RealValue {
    return RealValue{static_cast<Host>(std::log10(v_))};
  }
  [[nodiscard]] auto Exp() const -> RealValue {
    return RealValue{static_cast<Host>(std::exp(v_))};
  }
  [[nodiscard]] auto Sqrt() const -> RealValue {
    return RealValue{static_cast<Host>(std::sqrt(v_))};
  }
  [[nodiscard]] auto Floor() const -> RealValue {
    return RealValue{static_cast<Host>(std::floor(v_))};
  }
  [[nodiscard]] auto Ceil() const -> RealValue {
    return RealValue{static_cast<Host>(std::ceil(v_))};
  }
  [[nodiscard]] auto Sin() const -> RealValue {
    return RealValue{static_cast<Host>(std::sin(v_))};
  }
  [[nodiscard]] auto Cos() const -> RealValue {
    return RealValue{static_cast<Host>(std::cos(v_))};
  }
  [[nodiscard]] auto Tan() const -> RealValue {
    return RealValue{static_cast<Host>(std::tan(v_))};
  }
  [[nodiscard]] auto Asin() const -> RealValue {
    return RealValue{static_cast<Host>(std::asin(v_))};
  }
  [[nodiscard]] auto Acos() const -> RealValue {
    return RealValue{static_cast<Host>(std::acos(v_))};
  }
  [[nodiscard]] auto Atan() const -> RealValue {
    return RealValue{static_cast<Host>(std::atan(v_))};
  }
  [[nodiscard]] auto Atan2(const RealValue& o) const -> RealValue {
    return RealValue{static_cast<Host>(std::atan2(v_, o.v_))};
  }
  [[nodiscard]] auto Hypot(const RealValue& o) const -> RealValue {
    return RealValue{static_cast<Host>(std::hypot(v_, o.v_))};
  }
  [[nodiscard]] auto Sinh() const -> RealValue {
    return RealValue{static_cast<Host>(std::sinh(v_))};
  }
  [[nodiscard]] auto Cosh() const -> RealValue {
    return RealValue{static_cast<Host>(std::cosh(v_))};
  }
  [[nodiscard]] auto Tanh() const -> RealValue {
    return RealValue{static_cast<Host>(std::tanh(v_))};
  }
  [[nodiscard]] auto Asinh() const -> RealValue {
    return RealValue{static_cast<Host>(std::asinh(v_))};
  }
  [[nodiscard]] auto Acosh() const -> RealValue {
    return RealValue{static_cast<Host>(std::acosh(v_))};
  }
  [[nodiscard]] auto Atanh() const -> RealValue {
    return RealValue{static_cast<Host>(std::atanh(v_))};
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

  // LRM 6.12 reals have no X/Z plane.
  [[nodiscard]] static auto HasUnknown() -> bool {
    return false;
  }

  [[nodiscard]] static auto IsUnknown() -> PackedArray {
    return PackedArray::Bit(false);
  }

  // LRM Table 6-7: the real default is 0.0. Satisfies the container
  // OOB-shield contract so a real can be an unpacked-array element.
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

static_assert(LyraValue<Real>);
static_assert(LyraValue<ShortReal>);
static_assert(Ordered<Real>);
static_assert(Defaultable<Real>);
static_assert(Defaultable<ShortReal>);
static_assert(Ordered<ShortReal>);
static_assert(!CaseEqualComparable<Real>);
static_assert(!CaseEqualComparable<ShortReal>);

}  // namespace lyra::value
