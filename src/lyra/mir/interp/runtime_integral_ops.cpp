#include "lyra/mir/interp/runtime_integral_ops.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

namespace {

constexpr uint32_t kBitsPerWord = 64;

auto WordsNeeded(uint32_t bit_width) -> size_t {
  return (bit_width + kBitsPerWord - 1) / kBitsPerWord;
}

auto GetMask(uint32_t bit_width) -> uint64_t {
  if (bit_width == 0 || bit_width >= kBitsPerWord) {
    return ~uint64_t{0};
  }
  return (uint64_t{1} << bit_width) - 1;
}

void MaskTopWord(std::vector<uint64_t>& words, uint32_t bit_width) {
  if (words.empty() || bit_width == 0) {
    return;
  }
  uint32_t top_bits = bit_width % kBitsPerWord;
  if (top_bits != 0) {
    words.back() &= GetMask(top_bits);
  }
}

// Create a known zero-initialized integral result.
auto MakeKnownIntegral(uint32_t bit_width) -> RuntimeIntegral {
  RuntimeIntegral result;
  result.bit_width = bit_width;
  size_t num_words = WordsNeeded(bit_width);
  result.value.resize(num_words, 0);
  result.x_mask.resize(num_words, 0);
  result.z_mask.resize(num_words, 0);
  return result;
}

}  // namespace

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
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral {
  if (!lhs.IsKnown() || !rhs.IsKnown() || rhs.IsZero()) {
    return MakeUnknownIntegral(width);
  }

  // Single-word division is fully supported
  if (width <= 64) {
    uint64_t a = lhs.value.empty() ? 0 : lhs.value[0];
    uint64_t b = rhs.value.empty() ? 0 : rhs.value[0];
    // Note: b cannot be 0 here since rhs.IsZero() was already checked above.
    // This explicit check makes the invariant visible to static analyzers.
    if (b == 0) {
      return MakeUnknownIntegral(width);
    }
    return std::get<RuntimeIntegral>(MakeIntegral(a / b, width));
  }

  // Multi-word division: return unknown (all-X).
  // This is semantically "unimplemented" rather than "propagated X/Z".
  // TODO(hankhsu): Implement multi-word division algorithm.
  return MakeUnknownIntegral(width);
}

auto IntegralMod(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, uint32_t width)
    -> RuntimeIntegral {
  if (!lhs.IsKnown() || !rhs.IsKnown() || rhs.IsZero()) {
    return MakeUnknownIntegral(width);
  }

  // Single-word modulo is fully supported
  if (width <= 64) {
    uint64_t a = lhs.value.empty() ? 0 : lhs.value[0];
    uint64_t b = rhs.value.empty() ? 0 : rhs.value[0];
    // Note: b cannot be 0 here since rhs.IsZero() was already checked above.
    // This explicit check makes the invariant visible to static analyzers.
    if (b == 0) {
      return MakeUnknownIntegral(width);
    }
    return std::get<RuntimeIntegral>(MakeIntegral(a % b, width));
  }

  // Multi-word modulo: return unknown (all-X).
  // This is semantically "unimplemented" rather than "propagated X/Z".
  // TODO(hankhsu): Implement multi-word modulo algorithm.
  return MakeUnknownIntegral(width);
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

auto IntegralLt(
    const RuntimeIntegral& lhs, const RuntimeIntegral& rhs, bool is_signed)
    -> RuntimeIntegral {
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

  // Multi-word comparison: return unknown (all-X).
  // This is semantically "unimplemented" rather than "propagated X/Z".
  // TODO(hankhsu): Implement multi-word signed comparison.
  return MakeUnknown1Bit();
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

}  // namespace lyra::mir::interp
