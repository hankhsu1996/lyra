#include "lyra/mir/interp/runtime_integral_ops.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/common/internal_error.hpp"
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

auto StringBytesToIntegral(std::string_view str, uint32_t bit_width)
    -> RuntimeIntegral {
  auto result = MakeKnownIntegral(bit_width);
  size_t num_bytes = (bit_width + 7) / 8;

  // Pack bytes from string into integral, MSB first.
  // First character goes to most significant byte.
  for (size_t i = 0; i < str.size() && i < num_bytes; ++i) {
    // Target bit offset for this byte (MSB-aligned)
    size_t byte_idx = num_bytes - 1 - i;
    size_t bit_offset = byte_idx * 8;
    size_t word_idx = bit_offset / 64;
    size_t bit_in_word = bit_offset % 64;

    if (word_idx < result.value.size()) {
      auto byte_val = static_cast<uint8_t>(str[i]);
      result.value[word_idx] |= static_cast<uint64_t>(byte_val) << bit_in_word;
    }
  }

  // Ensure normalization when bit_width isn't a multiple of 8
  MaskTopWord(result.value, bit_width);
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

auto IntegralExtractSlice(
    const RuntimeIntegral& src, uint32_t bit_offset, uint32_t width)
    -> RuntimeIntegral {
  // Handle edge cases
  if (width == 0) {
    RuntimeIntegral result;
    result.bit_width = 0;
    return result;
  }

  auto result = MakeKnownIntegral(width);

  // Out-of-range semantics: if offset is entirely beyond source, return zero.
  // This is intentional - out-of-range reads return zero, not unknown.
  // Partial out-of-range reads zero-extend the missing high bits.
  if (bit_offset >= src.bit_width) {
    return result;
  }

  // 2-state only - unknown source should not exist
  if (!src.IsKnown()) {
    throw common::InternalError(
        "IntegralExtractSlice", "unknown source in 2-state operation");
  }

  size_t result_words = WordsNeeded(width);
  size_t word_offset = bit_offset / kBitsPerWord;
  size_t bit_shift = bit_offset % kBitsPerWord;

  for (size_t i = 0; i < result_words; ++i) {
    size_t src_word_idx = word_offset + i;
    if (src_word_idx >= src.value.size()) {
      // Beyond source - leave as zero
      break;
    }

    uint64_t low_part = src.value[src_word_idx] >> bit_shift;
    uint64_t high_part = 0;

    // Get bits from next word if shift is non-zero and next word exists
    if (bit_shift != 0 && src_word_idx + 1 < src.value.size()) {
      high_part = src.value[src_word_idx + 1] << (kBitsPerWord - bit_shift);
    }

    result.value[i] = low_part | high_part;
  }

  MaskTopWord(result.value, width);
  return result;
}

auto IntegralInsertSlice(
    const RuntimeIntegral& dst, const RuntimeIntegral& src, uint32_t bit_offset,
    uint32_t width) -> RuntimeIntegral {
  // Handle edge cases
  if (width == 0 || bit_offset >= dst.bit_width) {
    return dst;  // Nothing to modify
  }

  // 2-state only - unknown source should not exist
  if (!src.IsKnown()) {
    throw common::InternalError(
        "IntegralInsertSlice", "unknown source in 2-state operation");
  }

  // Copy destination
  auto result = dst;

  // Ensure result vectors are properly sized
  size_t result_words = WordsNeeded(dst.bit_width);
  result.value.resize(result_words, 0);
  result.x_mask.resize(result_words, 0);
  result.z_mask.resize(result_words, 0);

  // Calculate effective width (don't write beyond dst bounds)
  uint32_t effective_width = width;
  if (bit_offset + width > dst.bit_width) {
    effective_width = dst.bit_width - bit_offset;
  }

  // Create a properly sized source value (truncate or zero-extend to width)
  auto src_normalized = IntegralResize2State(src, false, width);

  // Strategy: iterate over destination words that are affected by the slice.
  // The slice occupies dst bits [bit_offset, bit_offset + effective_width).
  // For each dst word, compute which source bits map there.
  //
  // This mirrors IntegralExtractSlice but in reverse: instead of assembling
  // result words from source words, we're writing source bits into dst words.

  size_t first_dst_word = bit_offset / kBitsPerWord;
  size_t last_dst_word = (bit_offset + effective_width - 1) / kBitsPerWord;

  for (size_t dst_idx = first_dst_word;
       dst_idx <= last_dst_word && dst_idx < result_words; ++dst_idx) {
    // Global bit range for this destination word
    uint32_t word_start_bit = static_cast<uint32_t>(dst_idx * kBitsPerWord);
    uint32_t word_end_bit = word_start_bit + kBitsPerWord;

    // Intersection with the slice range [bit_offset, bit_offset +
    // effective_width)
    uint32_t slice_start = std::max(word_start_bit, bit_offset);
    uint32_t slice_end = std::min(word_end_bit, bit_offset + effective_width);

    if (slice_start >= slice_end) {
      continue;  // No intersection
    }

    uint32_t bits_to_write = slice_end - slice_start;
    uint32_t dst_bit_pos =
        slice_start - word_start_bit;  // Position within dst word
    uint32_t src_bit_pos =
        slice_start - bit_offset;  // Position within source stream

    // Extract 'bits_to_write' bits from source starting at src_bit_pos
    size_t src_word_idx = src_bit_pos / kBitsPerWord;
    size_t src_bit_in_word = src_bit_pos % kBitsPerWord;

    uint64_t src_bits = 0;
    if (src_word_idx < src_normalized.value.size()) {
      src_bits = src_normalized.value[src_word_idx] >> src_bit_in_word;
    }
    // If shift is non-zero and we need bits from the next source word
    if (src_bit_in_word != 0 &&
        src_word_idx + 1 < src_normalized.value.size()) {
      src_bits |= src_normalized.value[src_word_idx + 1]
                  << (kBitsPerWord - src_bit_in_word);
    }

    // Mask to only the bits we want
    uint64_t value_mask = GetMask(bits_to_write);
    src_bits &= value_mask;

    // Write to destination at dst_bit_pos
    uint64_t dst_mask = value_mask << dst_bit_pos;
    result.value[dst_idx] &= ~dst_mask;
    result.value[dst_idx] |= src_bits << dst_bit_pos;
    // Clear x_mask and z_mask for written bits (2-state writes known bits)
    result.x_mask[dst_idx] &= ~dst_mask;
    result.z_mask[dst_idx] &= ~dst_mask;
  }

  MaskTopWord(result.value, dst.bit_width);
  MaskTopWord(result.x_mask, dst.bit_width);
  MaskTopWord(result.z_mask, dst.bit_width);
  return result;
}

auto IntegralInsertSlice4State(
    const RuntimeIntegral& dst, const RuntimeIntegral& src, uint32_t bit_offset,
    uint32_t width) -> RuntimeIntegral {
  // Handle edge cases
  if (width == 0 || bit_offset >= dst.bit_width) {
    return dst;  // Nothing to modify
  }

  // Copy destination
  auto result = dst;

  // Ensure result vectors are properly sized
  size_t result_words = WordsNeeded(dst.bit_width);
  result.value.resize(result_words, 0);
  result.x_mask.resize(result_words, 0);
  result.z_mask.resize(result_words, 0);

  // Calculate effective width (don't write beyond dst bounds)
  uint32_t effective_width = width;
  if (bit_offset + width > dst.bit_width) {
    effective_width = dst.bit_width - bit_offset;
  }

  // Ensure source has properly sized vectors for safe indexing
  size_t src_words = WordsNeeded(width);
  std::vector<uint64_t> src_value = src.value;
  std::vector<uint64_t> src_x_mask = src.x_mask;
  std::vector<uint64_t> src_z_mask = src.z_mask;
  src_value.resize(src_words, 0);
  src_x_mask.resize(src_words, 0);
  src_z_mask.resize(src_words, 0);

  // Strategy: iterate over destination words that are affected by the slice.
  // The slice occupies dst bits [bit_offset, bit_offset + effective_width).
  // For each dst word, compute which source bits map there.
  size_t first_dst_word = bit_offset / kBitsPerWord;
  size_t last_dst_word = (bit_offset + effective_width - 1) / kBitsPerWord;

  for (size_t dst_idx = first_dst_word;
       dst_idx <= last_dst_word && dst_idx < result_words; ++dst_idx) {
    // Global bit range for this destination word
    uint32_t word_start_bit = static_cast<uint32_t>(dst_idx * kBitsPerWord);
    uint32_t word_end_bit = word_start_bit + kBitsPerWord;

    // Intersection with the slice range [bit_offset, bit_offset +
    // effective_width)
    uint32_t slice_start = std::max(word_start_bit, bit_offset);
    uint32_t slice_end = std::min(word_end_bit, bit_offset + effective_width);

    if (slice_start >= slice_end) {
      continue;  // No intersection
    }

    uint32_t bits_to_write = slice_end - slice_start;
    uint32_t dst_bit_pos =
        slice_start - word_start_bit;  // Position within dst word
    uint32_t src_bit_pos =
        slice_start - bit_offset;  // Position within source stream

    // Extract 'bits_to_write' bits from source starting at src_bit_pos
    size_t src_word_idx = src_bit_pos / kBitsPerWord;
    size_t src_bit_in_word = src_bit_pos % kBitsPerWord;

    // Helper lambda to extract bits from a source vector
    auto extract_bits = [&](const std::vector<uint64_t>& vec) -> uint64_t {
      uint64_t bits = 0;
      if (src_word_idx < vec.size()) {
        bits = vec[src_word_idx] >> src_bit_in_word;
      }
      if (src_bit_in_word != 0 && src_word_idx + 1 < vec.size()) {
        bits |= vec[src_word_idx + 1] << (kBitsPerWord - src_bit_in_word);
      }
      return bits & GetMask(bits_to_write);
    };

    uint64_t value_bits = extract_bits(src_value);
    uint64_t x_bits = extract_bits(src_x_mask);
    uint64_t z_bits = extract_bits(src_z_mask);

    // Write to destination at dst_bit_pos
    uint64_t dst_mask = GetMask(bits_to_write) << dst_bit_pos;
    result.value[dst_idx] &= ~dst_mask;
    result.value[dst_idx] |= value_bits << dst_bit_pos;
    result.x_mask[dst_idx] &= ~dst_mask;
    result.x_mask[dst_idx] |= x_bits << dst_bit_pos;
    result.z_mask[dst_idx] &= ~dst_mask;
    result.z_mask[dst_idx] |= z_bits << dst_bit_pos;
  }

  MaskTopWord(result.value, dst.bit_width);
  MaskTopWord(result.x_mask, dst.bit_width);
  MaskTopWord(result.z_mask, dst.bit_width);
  return result;
}

}  // namespace lyra::mir::interp
