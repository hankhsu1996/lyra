#include "lyra/mir/interp/runtime_integral_ops.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/mir/interp/runtime_integral_words.hpp"
#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

namespace {

// Returns: -1 if lhs < rhs, 0 if equal, 1 if lhs > rhs
// Requires: both operands normalized to bit_width
auto CompareMultiWordUnsigned(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t bit_width)
    -> int {
  AssertNormalized(lhs, bit_width);
  AssertNormalized(rhs, bit_width);

  size_t num_words = WordsNeeded(bit_width);
  for (size_t i = num_words; i > 0; --i) {
    uint64_t a = lhs.value[i - 1];
    uint64_t b = rhs.value[i - 1];
    if (a < b) {
      return -1;
    }
    if (a > b) {
      return 1;
    }
  }
  return 0;
}

auto CompareMultiWordSigned(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t bit_width)
    -> int {
  AssertNormalized(lhs, bit_width);
  AssertNormalized(rhs, bit_width);

  bool lhs_neg = GetSignBit(lhs, bit_width);
  bool rhs_neg = GetSignBit(rhs, bit_width);

  if (lhs_neg != rhs_neg) {
    return lhs_neg ? -1 : 1;  // negative < positive
  }

  // Same sign: unsigned comparison gives correct signed result.
  // For two's complement with the normalization invariant (bits above width =
  // 0):
  //   - Both positive: a < b (signed) iff a < b (unsigned)
  //   - Both negative: a = 2^n + a_s, b = 2^n + b_s where a_s, b_s < 0
  //     a_s < b_s iff (2^n + a_s) < (2^n + b_s) iff a < b (unsigned)
  return CompareMultiWordUnsigned(lhs, rhs, bit_width);
}

struct DivModResult {
  RuntimeIntegral quotient;
  RuntimeIntegral remainder;
};

auto DivModMultiWord(
    const RuntimeIntegral& dividend, const RuntimeIntegral& divisor,
    uint32_t bit_width) -> DivModResult {
  assert(bit_width > 0 && "DivModMultiWord: bit_width must be positive");

  auto quotient = MakeKnownIntegral(bit_width);
  auto remainder = MakeKnownIntegral(bit_width);

  // Normalize both inputs (self-contained, no caller assumptions)
  auto dividend_n = NormalizeToWidth(dividend, bit_width);
  auto divisor_n = NormalizeToWidth(divisor, bit_width);
  assert(!divisor_n.IsZero() && "DivModMultiWord: division by zero");

  // Shift amount for left shift by 1
  auto one = MakeKnownIntegral(bit_width);
  one.value[0] = 1;

  for (int32_t bit_pos = static_cast<int32_t>(bit_width) - 1; bit_pos >= 0;
       --bit_pos) {
    // remainder = (remainder << 1) | bit(dividend, bit_pos)
    remainder = IntegralShl(remainder, one, bit_width);
    if (GetBit(dividend_n, bit_pos, bit_width)) {
      remainder.value[0] |= 1;
    }
    MaskTopWord(remainder.value, bit_width);

    if (CompareMultiWordUnsigned(remainder, divisor_n, bit_width) >= 0) {
      remainder = IntegralSub(remainder, divisor_n, bit_width);
      SetBit(quotient, bit_pos);
    }
  }

  return {.quotient = quotient, .remainder = remainder};
}

// Returns the unsigned magnitude of a signed value.
// Note: For INT_MIN (-2^(n-1)), the result has the same bit pattern (2^(n-1)),
// which still has the sign bit set. This is correct because the caller
// (DivModMultiWord) interprets the result as unsigned, where 2^(n-1) is the
// correct magnitude. Do NOT check GetSignBit on the result.
auto AbsoluteValue(const RuntimeIntegral& val, uint32_t bit_width)
    -> RuntimeIntegral {
  assert(val.IsKnown() && "AbsoluteValue requires 2-state input");
  AssertNormalized(val, bit_width);

  if (!GetSignBit(val, bit_width)) {
    return val;  // Already non-negative
  }
  // Two's complement negate: ~val + 1
  // For INT_MIN, this returns INT_MIN (same bit pattern = unsigned 2^(n-1))
  return IntegralNeg(val, bit_width);
}

auto DivModMultiWordSigned(
    const RuntimeIntegral& dividend, const RuntimeIntegral& divisor,
    uint32_t bit_width) -> DivModResult {
  assert(bit_width > 0 && "DivModMultiWordSigned: bit_width must be positive");
  AssertNormalized(dividend, bit_width);
  AssertNormalized(divisor, bit_width);

  bool dividend_neg = GetSignBit(dividend, bit_width);
  bool divisor_neg = GetSignBit(divisor, bit_width);

  auto abs_dividend = AbsoluteValue(dividend, bit_width);
  auto abs_divisor = AbsoluteValue(divisor, bit_width);

  auto [quotient, remainder] =
      DivModMultiWord(abs_dividend, abs_divisor, bit_width);

  // quotient sign = dividend_sign XOR divisor_sign
  if (dividend_neg != divisor_neg) {
    quotient = IntegralNeg(quotient, bit_width);
  }

  // remainder sign = dividend_sign (SV truncates toward zero)
  if (dividend_neg) {
    remainder = IntegralNeg(remainder, bit_width);
  }

  return {.quotient = quotient, .remainder = remainder};
}

}  // namespace

auto MakeKnownIntegral(uint32_t bit_width) -> RuntimeIntegral {
  RuntimeIntegral result;
  result.bit_width = bit_width;
  size_t num_words = WordsNeeded(bit_width);
  result.value.resize(num_words, 0);
  result.x_mask.resize(num_words, 0);
  result.z_mask.resize(num_words, 0);
  return result;
}

auto MakeUnknownIntegral(uint32_t bit_width) -> RuntimeIntegral {
  RuntimeIntegral result;
  result.bit_width = bit_width;
  size_t num_words = WordsNeeded(bit_width);
  result.value.resize(num_words, 0);
  result.x_mask.resize(num_words, ~uint64_t{0});
  result.z_mask.resize(num_words, 0);
  MaskTopWord(result.x_mask, bit_width);
  return result;
}

auto IntegralAdd(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral {
  if (!lhs.IsKnown() || !rhs.IsKnown()) {
    return MakeUnknownIntegral(width);
  }

  auto result = MakeKnownIntegral(width);
  size_t num_words = WordsNeeded(width);

  uint64_t carry = 0;
  for (size_t i = 0; i < num_words; ++i) {
    uint64_t a = (i < lhs.value.size()) ? lhs.value[i] : 0;
    uint64_t b = (i < rhs.value.size()) ? rhs.value[i] : 0;
    __uint128_t sum = static_cast<__uint128_t>(a) + b + carry;
    result.value[i] = static_cast<uint64_t>(sum);
    carry = static_cast<uint64_t>(sum >> 64);
  }

  MaskTopWord(result.value, width);
  return result;
}

auto IntegralSub(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral {
  if (!lhs.IsKnown() || !rhs.IsKnown()) {
    return MakeUnknownIntegral(width);
  }

  auto result = MakeKnownIntegral(width);
  size_t num_words = WordsNeeded(width);

  uint64_t borrow = 0;
  for (size_t i = 0; i < num_words; ++i) {
    uint64_t a = (i < lhs.value.size()) ? lhs.value[i] : 0;
    uint64_t b = (i < rhs.value.size()) ? rhs.value[i] : 0;
    __uint128_t diff =
        static_cast<__uint128_t>(a) - b - borrow + (uint64_t{1} << 63);
    diff += (uint64_t{1} << 63);  // Add back the bias
    result.value[i] = static_cast<uint64_t>(diff);
    borrow = (diff < (static_cast<__uint128_t>(1) << 64)) ? 1 : 0;
  }

  MaskTopWord(result.value, width);
  return result;
}

auto IntegralMul(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral {
  if (!lhs.IsKnown() || !rhs.IsKnown()) {
    return MakeUnknownIntegral(width);
  }

  auto result = MakeKnownIntegral(width);
  size_t num_words = WordsNeeded(width);

  // Simple O(n^2) multiplication
  for (size_t i = 0; i < lhs.value.size() && i < num_words; ++i) {
    uint64_t carry = 0;
    for (size_t j = 0; j < rhs.value.size() && (i + j) < num_words; ++j) {
      __uint128_t prod =
          (static_cast<__uint128_t>(lhs.value[i]) * rhs.value[j]) +
          result.value[i + j] + carry;
      result.value[i + j] = static_cast<uint64_t>(prod);
      carry = static_cast<uint64_t>(prod >> 64);
    }
    if ((i + rhs.value.size()) < num_words) {
      result.value[i + rhs.value.size()] += carry;
    }
  }

  MaskTopWord(result.value, width);
  return result;
}

auto IntegralDiv(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width,
    bool is_signed) -> RuntimeIntegral {
  assert(width > 0 && "IntegralDiv: width must be positive");

  if (!lhs.IsKnown() || !rhs.IsKnown() || rhs.IsZero()) {
    return MakeUnknownIntegral(width);
  }

  // Single-word division
  if (width <= 64) {
    uint64_t a = lhs.value.empty() ? 0 : lhs.value[0];
    uint64_t b = rhs.value.empty() ? 0 : rhs.value[0];
    // Note: b cannot be 0 here since rhs.IsZero() was already checked above.
    // This explicit check makes the invariant visible to static analyzers.
    if (b == 0) {
      return MakeUnknownIntegral(width);
    }
    if (is_signed) {
      // Sign-extend each operand based on its own bit width
      auto sa = static_cast<int64_t>(a);
      auto sb = static_cast<int64_t>(b);
      if (lhs.bit_width < 64) {
        uint64_t sign_bit = uint64_t{1} << (lhs.bit_width - 1);
        if ((a & sign_bit) != 0) {
          sa = static_cast<int64_t>(a | ~GetMask(lhs.bit_width));
        }
      }
      if (rhs.bit_width < 64) {
        uint64_t sign_bit = uint64_t{1} << (rhs.bit_width - 1);
        if ((b & sign_bit) != 0) {
          sb = static_cast<int64_t>(b | ~GetMask(rhs.bit_width));
        }
      }
      return std::get<RuntimeIntegral>(
          MakeIntegral(static_cast<uint64_t>(sa / sb), width));
    }
    return std::get<RuntimeIntegral>(MakeIntegral(a / b, width));
  }

  // Multi-word division
  auto lhs_norm = NormalizeToWidth(lhs, width);
  auto rhs_norm = NormalizeToWidth(rhs, width);
  DivModResult result;
  if (is_signed) {
    result = DivModMultiWordSigned(lhs_norm, rhs_norm, width);
  } else {
    result = DivModMultiWord(lhs_norm, rhs_norm, width);
  }
  return result.quotient;
}

auto IntegralMod(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width,
    bool is_signed) -> RuntimeIntegral {
  assert(width > 0 && "IntegralMod: width must be positive");

  if (!lhs.IsKnown() || !rhs.IsKnown() || rhs.IsZero()) {
    return MakeUnknownIntegral(width);
  }

  // Single-word modulo
  if (width <= 64) {
    uint64_t a = lhs.value.empty() ? 0 : lhs.value[0];
    uint64_t b = rhs.value.empty() ? 0 : rhs.value[0];
    // Note: b cannot be 0 here since rhs.IsZero() was already checked above.
    // This explicit check makes the invariant visible to static analyzers.
    if (b == 0) {
      return MakeUnknownIntegral(width);
    }
    if (is_signed) {
      // Sign-extend each operand based on its own bit width
      auto sa = static_cast<int64_t>(a);
      auto sb = static_cast<int64_t>(b);
      if (lhs.bit_width < 64) {
        uint64_t sign_bit = uint64_t{1} << (lhs.bit_width - 1);
        if ((a & sign_bit) != 0) {
          sa = static_cast<int64_t>(a | ~GetMask(lhs.bit_width));
        }
      }
      if (rhs.bit_width < 64) {
        uint64_t sign_bit = uint64_t{1} << (rhs.bit_width - 1);
        if ((b & sign_bit) != 0) {
          sb = static_cast<int64_t>(b | ~GetMask(rhs.bit_width));
        }
      }
      return std::get<RuntimeIntegral>(
          MakeIntegral(static_cast<uint64_t>(sa % sb), width));
    }
    return std::get<RuntimeIntegral>(MakeIntegral(a % b, width));
  }

  // Multi-word modulo
  auto lhs_norm = NormalizeToWidth(lhs, width);
  auto rhs_norm = NormalizeToWidth(rhs, width);
  DivModResult result;
  if (is_signed) {
    result = DivModMultiWordSigned(lhs_norm, rhs_norm, width);
  } else {
    result = DivModMultiWord(lhs_norm, rhs_norm, width);
  }
  return result.remainder;
}

auto IntegralAnd(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral {
  auto result = MakeKnownIntegral(width);
  size_t num_words = WordsNeeded(width);

  for (size_t i = 0; i < num_words; ++i) {
    uint64_t a = (i < lhs.value.size()) ? lhs.value[i] : 0;
    uint64_t b = (i < rhs.value.size()) ? rhs.value[i] : 0;
    uint64_t ax = (i < lhs.x_mask.size()) ? lhs.x_mask[i] : 0;
    uint64_t bx = (i < rhs.x_mask.size()) ? rhs.x_mask[i] : 0;
    uint64_t az = (i < lhs.z_mask.size()) ? lhs.z_mask[i] : 0;
    uint64_t bz = (i < rhs.z_mask.size()) ? rhs.z_mask[i] : 0;

    // AND with 0 produces 0 regardless of X/Z
    // AND with X/Z produces X unless the other operand is 0
    uint64_t unknown = (ax | az | bx | bz);
    uint64_t known_zero = (~a & ~(ax | az)) | (~b & ~(bx | bz));
    result.value[i] = (a & b) & ~unknown;
    result.x_mask[i] = unknown & ~known_zero;
  }

  MaskTopWord(result.value, width);
  MaskTopWord(result.x_mask, width);
  return result;
}

auto IntegralOr(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral {
  auto result = MakeKnownIntegral(width);
  size_t num_words = WordsNeeded(width);

  for (size_t i = 0; i < num_words; ++i) {
    uint64_t a = (i < lhs.value.size()) ? lhs.value[i] : 0;
    uint64_t b = (i < rhs.value.size()) ? rhs.value[i] : 0;
    uint64_t ax = (i < lhs.x_mask.size()) ? lhs.x_mask[i] : 0;
    uint64_t bx = (i < rhs.x_mask.size()) ? rhs.x_mask[i] : 0;
    uint64_t az = (i < lhs.z_mask.size()) ? lhs.z_mask[i] : 0;
    uint64_t bz = (i < rhs.z_mask.size()) ? rhs.z_mask[i] : 0;

    // OR with 1 produces 1 regardless of X/Z
    uint64_t unknown = (ax | az | bx | bz);
    uint64_t known_one = (a & ~(ax | az)) | (b & ~(bx | bz));
    result.value[i] = (a | b) | known_one;
    result.x_mask[i] = unknown & ~known_one;
  }

  MaskTopWord(result.value, width);
  MaskTopWord(result.x_mask, width);
  return result;
}

auto IntegralXor(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral {
  auto result = MakeKnownIntegral(width);
  size_t num_words = WordsNeeded(width);

  for (size_t i = 0; i < num_words; ++i) {
    uint64_t a = (i < lhs.value.size()) ? lhs.value[i] : 0;
    uint64_t b = (i < rhs.value.size()) ? rhs.value[i] : 0;
    uint64_t ax = (i < lhs.x_mask.size()) ? lhs.x_mask[i] : 0;
    uint64_t bx = (i < rhs.x_mask.size()) ? rhs.x_mask[i] : 0;
    uint64_t az = (i < lhs.z_mask.size()) ? lhs.z_mask[i] : 0;
    uint64_t bz = (i < rhs.z_mask.size()) ? rhs.z_mask[i] : 0;

    uint64_t unknown = (ax | az | bx | bz);
    result.value[i] = (a ^ b) & ~unknown;
    result.x_mask[i] = unknown;
  }

  MaskTopWord(result.value, width);
  MaskTopWord(result.x_mask, width);
  return result;
}

auto IntegralNeg(const RuntimeIntegral& op, uint32_t width) -> RuntimeIntegral {
  if (!op.IsKnown()) {
    return MakeUnknownIntegral(width);
  }

  // Two's complement negation: ~x + 1
  auto result = MakeKnownIntegral(width);
  size_t num_words = WordsNeeded(width);

  uint64_t carry = 1;
  for (size_t i = 0; i < num_words; ++i) {
    uint64_t v = (i < op.value.size()) ? op.value[i] : 0;
    __uint128_t sum = static_cast<__uint128_t>(~v) + carry;
    result.value[i] = static_cast<uint64_t>(sum);
    carry = static_cast<uint64_t>(sum >> 64);
  }

  MaskTopWord(result.value, width);
  return result;
}

auto IntegralNot(const RuntimeIntegral& op, uint32_t width) -> RuntimeIntegral {
  auto result = MakeKnownIntegral(width);
  size_t num_words = WordsNeeded(width);

  for (size_t i = 0; i < num_words; ++i) {
    uint64_t v = (i < op.value.size()) ? op.value[i] : 0;
    uint64_t x = (i < op.x_mask.size()) ? op.x_mask[i] : 0;
    uint64_t z = (i < op.z_mask.size()) ? op.z_mask[i] : 0;

    result.value[i] = ~v & ~(x | z);
    result.x_mask[i] = x | z;
  }

  MaskTopWord(result.value, width);
  MaskTopWord(result.x_mask, width);
  return result;
}

namespace {

// Helper for 1-bit unknown result (used by comparisons and logical ops)
auto MakeUnknown1Bit() -> RuntimeIntegral {
  RuntimeIntegral result;
  result.bit_width = 1;
  result.value = {0};
  result.x_mask = {1};
  result.z_mask = {0};
  return result;
}

}  // namespace

auto IntegralEq(const RuntimeIntegral& lhs, const RuntimeIntegral& rhs)
    -> RuntimeIntegral {
  if (!lhs.IsKnown() || !rhs.IsKnown()) {
    return MakeUnknown1Bit();
  }

  size_t max_words = std::max(lhs.value.size(), rhs.value.size());
  bool equal = true;
  for (size_t i = 0; i < max_words; ++i) {
    uint64_t a = (i < lhs.value.size()) ? lhs.value[i] : 0;
    uint64_t b = (i < rhs.value.size()) ? rhs.value[i] : 0;
    if (a != b) {
      equal = false;
      break;
    }
  }

  return std::get<RuntimeIntegral>(MakeIntegral(equal ? 1 : 0, 1));
}

auto IntegralNe(const RuntimeIntegral& lhs, const RuntimeIntegral& rhs)
    -> RuntimeIntegral {
  auto eq = IntegralEq(lhs, rhs);
  if (eq.IsX()) {
    return eq;
  }
  return std::get<RuntimeIntegral>(MakeIntegral(eq.IsZero() ? 1 : 0, 1));
}

auto IntegralCaseXMatch(const RuntimeIntegral& lhs, const RuntimeIntegral& rhs)
    -> RuntimeIntegral {
  // casex: X and Z bits from BOTH operands are wildcards (don't-care).
  // Compare only the known bits that are not X or Z in either operand.

  // If both are fully known, use exact equality
  if (lhs.IsKnown() && rhs.IsKnown()) {
    auto eq = IntegralEq(lhs, rhs);
    // IntegralEq returns 0 or 1 for known operands, never X
    return eq;
  }

  size_t max_words = std::max(
      {lhs.value.size(), rhs.value.size(), lhs.x_mask.size(), rhs.x_mask.size(),
       lhs.z_mask.size(), rhs.z_mask.size()});

  for (size_t i = 0; i < max_words; ++i) {
    uint64_t lhs_val = (i < lhs.value.size()) ? lhs.value[i] : 0;
    uint64_t rhs_val = (i < rhs.value.size()) ? rhs.value[i] : 0;
    uint64_t lhs_x = (i < lhs.x_mask.size()) ? lhs.x_mask[i] : 0;
    uint64_t rhs_x = (i < rhs.x_mask.size()) ? rhs.x_mask[i] : 0;
    uint64_t lhs_z = (i < lhs.z_mask.size()) ? lhs.z_mask[i] : 0;
    uint64_t rhs_z = (i < rhs.z_mask.size()) ? rhs.z_mask[i] : 0;

    // Wildcard mask: bits that are X or Z in either operand
    uint64_t wildcard = lhs_x | lhs_z | rhs_x | rhs_z;
    // Compare mask: bits we actually need to compare
    uint64_t compare_mask = ~wildcard;

    if ((lhs_val & compare_mask) != (rhs_val & compare_mask)) {
      return std::get<RuntimeIntegral>(MakeIntegral(0, 1));  // No match
    }
  }

  return std::get<RuntimeIntegral>(MakeIntegral(1, 1));  // Match
}

auto IntegralCaseZMatch(const RuntimeIntegral& lhs, const RuntimeIntegral& rhs)
    -> RuntimeIntegral {
  // casez: Z bits from BOTH operands are wildcards (don't-care).
  // X bits are NOT wildcards: they are compared normally (X==X is true,
  // X==0 or X==1 is false). This differs from casex where X is also a wildcard.

  // If both are fully known, use exact equality
  if (lhs.IsKnown() && rhs.IsKnown()) {
    auto eq = IntegralEq(lhs, rhs);
    return eq;
  }

  size_t max_words = std::max(
      {lhs.value.size(), rhs.value.size(), lhs.x_mask.size(), rhs.x_mask.size(),
       lhs.z_mask.size(), rhs.z_mask.size()});

  for (size_t i = 0; i < max_words; ++i) {
    uint64_t lhs_val = (i < lhs.value.size()) ? lhs.value[i] : 0;
    uint64_t rhs_val = (i < rhs.value.size()) ? rhs.value[i] : 0;
    uint64_t lhs_x = (i < lhs.x_mask.size()) ? lhs.x_mask[i] : 0;
    uint64_t rhs_x = (i < rhs.x_mask.size()) ? rhs.x_mask[i] : 0;
    uint64_t lhs_z = (i < lhs.z_mask.size()) ? lhs.z_mask[i] : 0;
    uint64_t rhs_z = (i < rhs.z_mask.size()) ? rhs.z_mask[i] : 0;

    // Z wildcard mask: bits that are Z in either operand (wildcards)
    uint64_t z_wildcard = lhs_z | rhs_z;
    // Compare mask: bits we need to compare (not Z in either operand)
    uint64_t compare_mask = ~z_wildcard;

    // Value bits must match (for non-wildcard positions)
    if ((lhs_val & compare_mask) != (rhs_val & compare_mask)) {
      return std::get<RuntimeIntegral>(MakeIntegral(0, 1));  // No match
    }

    // X bits must match exactly (for non-wildcard positions)
    if ((lhs_x & compare_mask) != (rhs_x & compare_mask)) {
      return std::get<RuntimeIntegral>(MakeIntegral(0, 1));  // No match
    }
  }

  return std::get<RuntimeIntegral>(MakeIntegral(1, 1));  // Match
}

auto IntegralLt(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, bool is_signed)
    -> RuntimeIntegral {
  assert(
      lhs.bit_width > 0 && rhs.bit_width > 0 &&
      "IntegralLt: bit_width must be positive");

  if (!lhs.IsKnown() || !rhs.IsKnown()) {
    return MakeUnknown1Bit();
  }

  // Single-word comparison is fully supported
  uint32_t width = std::max(lhs.bit_width, rhs.bit_width);
  if (width <= 64) {
    uint64_t a = lhs.value.empty() ? 0 : lhs.value[0];
    uint64_t b = rhs.value.empty() ? 0 : rhs.value[0];
    bool cmp_result = false;
    if (is_signed) {
      auto sa = static_cast<int64_t>(a);
      auto sb = static_cast<int64_t>(b);
      // Sign-extend if needed
      if (lhs.bit_width < 64) {
        uint64_t sign_bit = uint64_t{1} << (lhs.bit_width - 1);
        if ((a & sign_bit) != 0) {
          sa = static_cast<int64_t>(a | ~GetMask(lhs.bit_width));
        }
      }
      if (rhs.bit_width < 64) {
        uint64_t sign_bit = uint64_t{1} << (rhs.bit_width - 1);
        if ((b & sign_bit) != 0) {
          sb = static_cast<int64_t>(b | ~GetMask(rhs.bit_width));
        }
      }
      cmp_result = sa < sb;
    } else {
      cmp_result = a < b;
    }
    return std::get<RuntimeIntegral>(MakeIntegral(cmp_result ? 1 : 0, 1));
  }

  // Multi-word comparison: normalize both operands to the same width
  auto lhs_norm = NormalizeToWidth(lhs, width);
  auto rhs_norm = NormalizeToWidth(rhs, width);

  int cmp = 0;
  if (is_signed) {
    cmp = CompareMultiWordSigned(lhs_norm, rhs_norm, width);
  } else {
    cmp = CompareMultiWordUnsigned(lhs_norm, rhs_norm, width);
  }
  return std::get<RuntimeIntegral>(MakeIntegral(cmp < 0 ? 1 : 0, 1));
}

auto IntegralLe(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, bool is_signed)
    -> RuntimeIntegral {
  auto lt = IntegralLt(lhs, rhs, is_signed);
  if (lt.IsX()) {
    return lt;
  }
  if (!lt.IsZero()) {
    return lt;
  }

  auto eq = IntegralEq(lhs, rhs);
  if (eq.IsX()) {
    return eq;
  }
  return eq;
}

auto IntegralGt(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, bool is_signed)
    -> RuntimeIntegral {
  // Intentionally swap arguments: a > b iff b < a
  // NOLINTNEXTLINE(readability-suspicious-call-argument)
  return IntegralLt(rhs, lhs, is_signed);
}

auto IntegralGe(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, bool is_signed)
    -> RuntimeIntegral {
  // Intentionally swap arguments: a >= b iff b <= a
  // NOLINTNEXTLINE(readability-suspicious-call-argument)
  return IntegralLe(rhs, lhs, is_signed);
}

auto IntegralLogicalAnd(const RuntimeIntegral& lhs, const RuntimeIntegral& rhs)
    -> RuntimeIntegral {
  // False && anything = false
  if (lhs.IsKnown() && lhs.IsZero()) {
    return std::get<RuntimeIntegral>(MakeIntegral(0, 1));
  }
  if (rhs.IsKnown() && rhs.IsZero()) {
    return std::get<RuntimeIntegral>(MakeIntegral(0, 1));
  }

  // Unknown && unknown or true && unknown
  if (!lhs.IsKnown() || !rhs.IsKnown()) {
    return MakeUnknown1Bit();
  }

  bool result = !lhs.IsZero() && !rhs.IsZero();
  return std::get<RuntimeIntegral>(MakeIntegral(result ? 1 : 0, 1));
}

auto IntegralLogicalOr(const RuntimeIntegral& lhs, const RuntimeIntegral& rhs)
    -> RuntimeIntegral {
  // True || anything = true
  if (lhs.IsKnown() && !lhs.IsZero()) {
    return std::get<RuntimeIntegral>(MakeIntegral(1, 1));
  }
  if (rhs.IsKnown() && !rhs.IsZero()) {
    return std::get<RuntimeIntegral>(MakeIntegral(1, 1));
  }

  // Unknown || unknown or false || unknown
  if (!lhs.IsKnown() || !rhs.IsKnown()) {
    return MakeUnknown1Bit();
  }

  bool result = !lhs.IsZero() || !rhs.IsZero();
  return std::get<RuntimeIntegral>(MakeIntegral(result ? 1 : 0, 1));
}

auto IntegralLogicalNot(const RuntimeIntegral& op) -> RuntimeIntegral {
  if (!op.IsKnown()) {
    return MakeUnknown1Bit();
  }

  return std::get<RuntimeIntegral>(MakeIntegral(op.IsZero() ? 1 : 0, 1));
}

auto IntegralShl(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral {
  if (!lhs.IsKnown() || !rhs.IsKnown()) {
    return MakeUnknownIntegral(width);
  }

  uint64_t shift = rhs.value.empty() ? 0 : rhs.value[0];
  if (shift >= width) {
    return std::get<RuntimeIntegral>(MakeIntegral(0, width));
  }

  auto result = MakeKnownIntegral(width);
  size_t num_words = WordsNeeded(width);

  size_t word_shift = shift / kBitsPerWord;
  size_t bit_shift = shift % kBitsPerWord;

  for (size_t i = word_shift; i < num_words; ++i) {
    size_t src = i - word_shift;
    uint64_t v = (src < lhs.value.size()) ? lhs.value[src] : 0;
    result.value[i] |= v << bit_shift;
    if (bit_shift != 0 && i + 1 < num_words && src < lhs.value.size()) {
      result.value[i + 1] |= v >> (kBitsPerWord - bit_shift);
    }
  }

  MaskTopWord(result.value, width);
  return result;
}

auto IntegralShr(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width,
    bool is_signed) -> RuntimeIntegral {
  assert(
      width > 0 && lhs.bit_width > 0 && "IntegralShr: width must be positive");

  if (!lhs.IsKnown() || !rhs.IsKnown()) {
    return MakeUnknownIntegral(width);
  }

  uint64_t shift = rhs.value.empty() ? 0 : rhs.value[0];
  if (shift >= width) {
    if (is_signed) {
      // Arithmetic shift: fill with sign bit
      uint64_t sign_bit = (lhs.value.empty() ? 0 : lhs.value.back()) >>
                          ((lhs.bit_width - 1) % kBitsPerWord);
      sign_bit &= 1;
      uint64_t fill = (sign_bit != 0) ? ~uint64_t{0} : 0;
      RuntimeIntegral result;
      result.bit_width = width;
      size_t num_words = WordsNeeded(width);
      result.value.resize(num_words, fill);
      result.x_mask.resize(num_words, 0);
      result.z_mask.resize(num_words, 0);
      MaskTopWord(result.value, width);
      return result;
    }
    return std::get<RuntimeIntegral>(MakeIntegral(0, width));
  }

  auto result = MakeKnownIntegral(width);
  size_t num_words = WordsNeeded(width);

  size_t word_shift = shift / kBitsPerWord;
  size_t bit_shift = shift % kBitsPerWord;

  for (size_t i = 0; i + word_shift < num_words; ++i) {
    size_t src = i + word_shift;
    uint64_t v = (src < lhs.value.size()) ? lhs.value[src] : 0;
    result.value[i] |= v >> bit_shift;
    if (bit_shift != 0 && src + 1 < lhs.value.size()) {
      result.value[i] |= lhs.value[src + 1] << (kBitsPerWord - bit_shift);
    }
  }

  if (is_signed) {
    // Fill upper bits with sign bit
    uint64_t sign_bit = (lhs.value.empty() ? 0 : lhs.value.back()) >>
                        ((lhs.bit_width - 1) % kBitsPerWord);
    sign_bit &= 1;
    if (sign_bit != 0) {
      uint32_t result_bits = width - static_cast<uint32_t>(shift);
      for (uint32_t i = result_bits; i < width; ++i) {
        size_t word = i / kBitsPerWord;
        size_t bit = i % kBitsPerWord;
        if (word < result.value.size()) {
          result.value[word] |= uint64_t{1} << bit;
        }
      }
    }
  }

  MaskTopWord(result.value, width);
  return result;
}

auto IntegralResize2State(
    const RuntimeIntegral& src, bool src_is_signed, uint32_t target_width)
    -> RuntimeIntegral {
  assert(
      src.bit_width > 0 &&
      "IntegralResize2State: src.bit_width must be positive");

  if (!src.IsKnown()) {
    return MakeUnknownIntegral(target_width);
  }

  auto result = MakeKnownIntegral(target_width);
  size_t source_words = src.value.size();
  size_t result_words = WordsNeeded(target_width);

  // Copy source words (up to target size)
  for (size_t i = 0; i < std::min(source_words, result_words); ++i) {
    result.value[i] = src.value[i];
  }

  // Sign extension: fill upper bits with sign bit using word-level ops
  if (target_width > src.bit_width && src_is_signed && src.bit_width > 0) {
    uint32_t sign_bit_pos = (src.bit_width - 1) % kBitsPerWord;
    size_t sign_word_idx = (src.bit_width - 1) / kBitsPerWord;
    bool sign_bit = false;
    if (sign_word_idx < src.value.size()) {
      sign_bit = ((src.value[sign_word_idx] >> sign_bit_pos) & 1) != 0;
    }

    if (sign_bit) {
      // Fill whole words above sign_word_idx with all 1s
      for (size_t i = sign_word_idx + 1; i < result_words; ++i) {
        result.value[i] = ~uint64_t{0};
      }
      // Set bits above sign_bit_pos in the sign word.
      // Special case: if sign_bit_pos == 63, there are no bits above it in
      // this word (shifting by 64 is UB), so upper_mask is 0.
      if (sign_word_idx < result_words && sign_bit_pos < 63) {
        uint64_t upper_mask = ~((uint64_t{1} << (sign_bit_pos + 1)) - 1);
        result.value[sign_word_idx] |= upper_mask;
      }
    }
  }

  MaskTopWord(result.value, target_width);
  return result;
}

}  // namespace lyra::mir::interp
