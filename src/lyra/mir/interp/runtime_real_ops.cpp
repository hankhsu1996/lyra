#include "lyra/mir/interp/runtime_real_ops.hpp"

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>

#include "lyra/common/internal_error.hpp"
#include "lyra/mir/interp/runtime_integral_ops.hpp"
#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

auto RealAdd(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeReal {
  return {.value = lhs.value + rhs.value};
}

auto RealSub(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeReal {
  return {.value = lhs.value - rhs.value};
}

auto RealMul(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeReal {
  return {.value = lhs.value * rhs.value};
}

auto RealDiv(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeReal {
  // IEEE 754: div by zero produces infinity, NaN propagates
  return {.value = lhs.value / rhs.value};
}

auto RealNeg(const RuntimeReal& op) -> RuntimeReal {
  return {.value = -op.value};
}

auto RealPlus(const RuntimeReal& op) -> RuntimeReal {
  return {.value = op.value};
}

namespace {
// Helper to create a 2-state 1-bit result (consistent with integral
// comparisons)
auto Make1BitResult(bool value) -> RuntimeIntegral {
  return std::get<RuntimeIntegral>(MakeIntegral(value ? 1 : 0, 1));
}
}  // namespace

auto RealEq(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeIntegral {
  // IEEE 754: NaN != NaN
  return Make1BitResult(lhs.value == rhs.value);
}

auto RealNe(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeIntegral {
  return Make1BitResult(lhs.value != rhs.value);
}

auto RealLt(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeIntegral {
  return Make1BitResult(lhs.value < rhs.value);
}

auto RealLe(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeIntegral {
  return Make1BitResult(lhs.value <= rhs.value);
}

auto RealGt(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeIntegral {
  return Make1BitResult(lhs.value > rhs.value);
}

auto RealGe(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeIntegral {
  return Make1BitResult(lhs.value >= rhs.value);
}

auto RealLogicalAnd(const RuntimeReal& lhs, const RuntimeReal& rhs)
    -> RuntimeIntegral {
  // Truthiness: value != 0.0
  bool lhs_true = lhs.value != 0.0;
  bool rhs_true = rhs.value != 0.0;
  return Make1BitResult(lhs_true && rhs_true);
}

auto RealLogicalOr(const RuntimeReal& lhs, const RuntimeReal& rhs)
    -> RuntimeIntegral {
  bool lhs_true = lhs.value != 0.0;
  bool rhs_true = rhs.value != 0.0;
  return Make1BitResult(lhs_true || rhs_true);
}

auto RealLogicalNot(const RuntimeReal& op) -> RuntimeIntegral {
  return Make1BitResult(op.value == 0.0);
}

auto RealPower(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeReal {
  return {.value = std::pow(lhs.value, rhs.value)};
}

auto RealLn(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::log(op.value)};
}

auto RealLog10(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::log10(op.value)};
}

auto RealExp(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::exp(op.value)};
}

auto RealSqrt(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::sqrt(op.value)};
}

auto RealFloor(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::floor(op.value)};
}

auto RealCeil(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::ceil(op.value)};
}

auto RealSin(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::sin(op.value)};
}

auto RealCos(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::cos(op.value)};
}

auto RealTan(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::tan(op.value)};
}

auto RealAsin(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::asin(op.value)};
}

auto RealAcos(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::acos(op.value)};
}

auto RealAtan(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::atan(op.value)};
}

auto RealSinh(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::sinh(op.value)};
}

auto RealCosh(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::cosh(op.value)};
}

auto RealTanh(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::tanh(op.value)};
}

auto RealAsinh(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::asinh(op.value)};
}

auto RealAcosh(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::acosh(op.value)};
}

auto RealAtanh(const RuntimeReal& op) -> RuntimeReal {
  return {.value = std::atanh(op.value)};
}

auto RealAtan2(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeReal {
  return {.value = std::atan2(lhs.value, rhs.value)};
}

auto RealHypot(const RuntimeReal& lhs, const RuntimeReal& rhs) -> RuntimeReal {
  return {.value = std::hypot(lhs.value, rhs.value)};
}

auto RealToIntegral(
    const RuntimeReal& src, uint32_t target_width, bool target_signed)
    -> RuntimeIntegral {
  if (target_width == 0) {
    throw common::InternalError(
        "RealToIntegral", "target_width must be positive");
  }

  // Truncate toward zero (C++ static_cast semantics for floating -> int)
  // Handle overflow by clamping to representable range
  double val = std::trunc(src.value);

  // Handle special cases first
  if (std::isnan(val)) {
    return MakeKnownIntegral(target_width);  // NaN -> 0
  }
  if (std::isinf(val)) {
    // +Inf -> max value, -Inf -> min value (or 0 for unsigned)
    auto result = MakeKnownIntegral(target_width);
    if (val > 0) {
      // Fill with all 1s, then mask for correct width
      for (auto& word : result.value) {
        word = ~uint64_t{0};
      }
      // For signed, clear sign bit to get max positive
      if (target_signed && target_width > 0) {
        size_t sign_word = (target_width - 1) / 64;
        size_t sign_bit = (target_width - 1) % 64;
        if (sign_word < result.value.size()) {
          result.value[sign_word] &= ~(uint64_t{1} << sign_bit);
        }
      }
    } else if (target_signed) {
      // -Inf -> min negative (sign bit set, rest 0)
      size_t sign_word = (target_width - 1) / 64;
      size_t sign_bit = (target_width - 1) % 64;
      if (sign_word < result.value.size()) {
        result.value[sign_word] = uint64_t{1} << sign_bit;
      }
    }
    // -Inf with unsigned -> 0 (already initialized)
    return result;
  }

  // For widths <= 64, use direct conversion with clamping
  if (target_width <= 64) {
    if (target_signed) {
      int64_t max_val = (target_width == 64)
                            ? std::numeric_limits<int64_t>::max()
                            : (int64_t{1} << (target_width - 1)) - 1;
      int64_t min_val = (target_width == 64)
                            ? std::numeric_limits<int64_t>::min()
                            : -(int64_t{1} << (target_width - 1));

      int64_t int_val = 0;
      if (val >= static_cast<double>(max_val)) {
        int_val = max_val;
      } else if (val <= static_cast<double>(min_val)) {
        int_val = min_val;
      } else {
        int_val = static_cast<int64_t>(val);
      }

      auto result = MakeKnownIntegral(target_width);
      result.value[0] = static_cast<uint64_t>(int_val);
      return result;
    }

    // Unsigned
    uint64_t max_val = (target_width == 64)
                           ? std::numeric_limits<uint64_t>::max()
                           : (uint64_t{1} << target_width) - 1;

    uint64_t int_val = 0;
    if (val < 0.0) {
      int_val = 0;
    } else if (val >= static_cast<double>(max_val)) {
      int_val = max_val;
    } else {
      int_val = static_cast<uint64_t>(val);
    }

    auto result = MakeKnownIntegral(target_width);
    result.value[0] = int_val;
    return result;
  }

  // For widths > 64: double has ~53 bits of precision, so we convert to
  // int64_t/uint64_t and sign-extend/zero-extend to fill wider targets.
  auto result = MakeKnownIntegral(target_width);

  if (target_signed) {
    // Clamp to int64_t range, then sign-extend
    int64_t int_val = 0;
    if (val >= static_cast<double>(std::numeric_limits<int64_t>::max())) {
      int_val = std::numeric_limits<int64_t>::max();
    } else if (
        val <= static_cast<double>(std::numeric_limits<int64_t>::min())) {
      int_val = std::numeric_limits<int64_t>::min();
    } else {
      int_val = static_cast<int64_t>(val);
    }

    result.value[0] = static_cast<uint64_t>(int_val);
    // Sign-extend: if negative, fill upper words with all 1s
    if (int_val < 0) {
      for (size_t i = 1; i < result.value.size(); ++i) {
        result.value[i] = ~uint64_t{0};
      }
    }
  } else {
    // Clamp to uint64_t range, zero-extend (already initialized to 0)
    uint64_t int_val = 0;
    if (val < 0.0) {
      int_val = 0;
    } else if (
        val >= static_cast<double>(std::numeric_limits<uint64_t>::max())) {
      int_val = std::numeric_limits<uint64_t>::max();
    } else {
      int_val = static_cast<uint64_t>(val);
    }
    result.value[0] = int_val;
    // Upper words already 0 from MakeKnownIntegral
  }

  return result;
}

auto IntegralToReal(const RuntimeIntegral& src, bool src_is_signed)
    -> RuntimeReal {
  if (!src.IsKnown()) {
    // X/Z -> 0.0 (matches SV semantics for 4-state to real conversion)
    return {.value = 0.0};
  }

  // For 64-bit or less, simple conversion
  if (src.bit_width <= 64) {
    uint64_t val = src.value.empty() ? 0 : src.value[0];
    if (src_is_signed) {
      // Sign-extend if needed
      if (src.bit_width < 64) {
        uint64_t sign_bit = uint64_t{1} << (src.bit_width - 1);
        if ((val & sign_bit) != 0) {
          // Sign extend
          uint64_t mask =
              (src.bit_width == 64) ? 0 : ~((uint64_t{1} << src.bit_width) - 1);
          val |= mask;
        }
      }
      return {.value = static_cast<double>(static_cast<int64_t>(val))};
    }
    return {.value = static_cast<double>(val)};
  }

  // For wider values, accumulate (may lose precision)
  double result = 0.0;
  double multiplier = 1.0;
  for (uint64_t word : src.value) {
    result += static_cast<double>(word) * multiplier;
    multiplier *= static_cast<double>(uint64_t{1} << 32) *
                  static_cast<double>(uint64_t{1} << 32);
  }

  // Handle sign for wide signed values
  if (src_is_signed && src.bit_width > 0) {
    size_t sign_word_idx = (src.bit_width - 1) / 64;
    size_t sign_bit_pos = (src.bit_width - 1) % 64;
    if (sign_word_idx < src.value.size() &&
        ((src.value[sign_word_idx] >> sign_bit_pos) & 1) != 0) {
      // Negative value: compute 2^n - result
      double max_val = std::pow(2.0, static_cast<double>(src.bit_width));
      result = result - max_val;
    }
  }

  return {.value = result};
}

auto RealIsTrue(const RuntimeReal& op) -> bool {
  return op.value != 0.0;
}

auto ShortRealAdd(const RuntimeShortReal& lhs, const RuntimeShortReal& rhs)
    -> RuntimeShortReal {
  return {.value = lhs.value + rhs.value};
}

auto ShortRealSub(const RuntimeShortReal& lhs, const RuntimeShortReal& rhs)
    -> RuntimeShortReal {
  return {.value = lhs.value - rhs.value};
}

auto ShortRealMul(const RuntimeShortReal& lhs, const RuntimeShortReal& rhs)
    -> RuntimeShortReal {
  return {.value = lhs.value * rhs.value};
}

auto ShortRealDiv(const RuntimeShortReal& lhs, const RuntimeShortReal& rhs)
    -> RuntimeShortReal {
  return {.value = lhs.value / rhs.value};
}

auto ShortRealNeg(const RuntimeShortReal& op) -> RuntimeShortReal {
  return {.value = -op.value};
}

auto ShortRealPlus(const RuntimeShortReal& op) -> RuntimeShortReal {
  return {.value = op.value};
}

auto ShortRealEq(const RuntimeShortReal& lhs, const RuntimeShortReal& rhs)
    -> RuntimeIntegral {
  return Make1BitResult(lhs.value == rhs.value);
}

auto ShortRealNe(const RuntimeShortReal& lhs, const RuntimeShortReal& rhs)
    -> RuntimeIntegral {
  return Make1BitResult(lhs.value != rhs.value);
}

auto ShortRealLt(const RuntimeShortReal& lhs, const RuntimeShortReal& rhs)
    -> RuntimeIntegral {
  return Make1BitResult(lhs.value < rhs.value);
}

auto ShortRealLe(const RuntimeShortReal& lhs, const RuntimeShortReal& rhs)
    -> RuntimeIntegral {
  return Make1BitResult(lhs.value <= rhs.value);
}

auto ShortRealGt(const RuntimeShortReal& lhs, const RuntimeShortReal& rhs)
    -> RuntimeIntegral {
  return Make1BitResult(lhs.value > rhs.value);
}

auto ShortRealGe(const RuntimeShortReal& lhs, const RuntimeShortReal& rhs)
    -> RuntimeIntegral {
  return Make1BitResult(lhs.value >= rhs.value);
}

auto ShortRealLogicalAnd(
    const RuntimeShortReal& lhs, const RuntimeShortReal& rhs)
    -> RuntimeIntegral {
  return Make1BitResult(lhs.value != 0.0F && rhs.value != 0.0F);
}

auto ShortRealLogicalOr(
    const RuntimeShortReal& lhs, const RuntimeShortReal& rhs)
    -> RuntimeIntegral {
  return Make1BitResult(lhs.value != 0.0F || rhs.value != 0.0F);
}

auto ShortRealLogicalNot(const RuntimeShortReal& op) -> RuntimeIntegral {
  return Make1BitResult(op.value == 0.0F);
}

auto ShortRealPower(const RuntimeShortReal& lhs, const RuntimeShortReal& rhs)
    -> RuntimeShortReal {
  return {.value = std::pow(lhs.value, rhs.value)};
}

auto ShortRealToIntegral(
    const RuntimeShortReal& src, uint32_t target_width, bool target_signed)
    -> RuntimeIntegral {
  // Reuse real logic by converting to double
  RuntimeReal r{.value = static_cast<double>(src.value)};
  return RealToIntegral(r, target_width, target_signed);
}

auto IntegralToShortReal(const RuntimeIntegral& src, bool src_is_signed)
    -> RuntimeShortReal {
  RuntimeReal r = IntegralToReal(src, src_is_signed);
  return {.value = static_cast<float>(r.value)};
}

auto ShortRealToReal(const RuntimeShortReal& src) -> RuntimeReal {
  return {.value = static_cast<double>(src.value)};
}

auto RealToShortReal(const RuntimeReal& src) -> RuntimeShortReal {
  return {.value = static_cast<float>(src.value)};
}

auto ShortRealIsTrue(const RuntimeShortReal& op) -> bool {
  return op.value != 0.0F;
}

}  // namespace lyra::mir::interp
