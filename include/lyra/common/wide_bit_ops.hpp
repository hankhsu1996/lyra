#pragma once

// Shared algorithms for multi-word bit vector operations.
// Used by both common::WideBit (interpreter, dynamic) and sdk::WideBit<N>
// (codegen, static).
//
// Design: Operations are templated on container type to work with both
// std::vector<uint64_t> and std::array<uint64_t, N>. All core algorithms
// are constexpr-compatible.
//
// Storage convention: little-endian word order (LSB in words[0]).

#include <cstddef>
#include <cstdint>
#include <string>

namespace lyra::common::wide_ops {

// ============================================================================
// Helper functions
// ============================================================================

// Calculate number of words needed for a given bit width
constexpr auto WordsForBits(size_t bit_width) -> size_t {
  return (bit_width + 63) / 64;
}

// Calculate which word contains a given bit index
constexpr auto WordIndex(size_t bit_index) -> size_t {
  return bit_index / 64;
}

// Calculate bit position within a word
constexpr auto BitInWord(size_t bit_index) -> size_t {
  return bit_index % 64;
}

// Mask for the final word given bit width
constexpr auto FinalWordMask(size_t bit_width) -> uint64_t {
  size_t bits_in_final = bit_width % 64;
  return (bits_in_final == 0) ? ~0ULL : (1ULL << bits_in_final) - 1;
}

// ============================================================================
// Masking
// ============================================================================

// Mask final word to specified bit width (modifies in place)
template <typename Container>
constexpr auto MaskToWidth(Container& words, size_t bit_width) -> void {
  if (words.size() == 0) {
    return;
  }
  size_t needed_words = WordsForBits(bit_width);
  // Clear extra words beyond what's needed
  for (size_t i = needed_words; i < words.size(); ++i) {
    words[i] = 0;
  }
  // Mask the final word
  if (needed_words > 0 && needed_words <= words.size()) {
    words[needed_words - 1] &= FinalWordMask(bit_width);
  }
}

// ============================================================================
// Bitwise operations
// ============================================================================

// Bitwise NOT: result[i] = ~src[i], then mask to width
template <typename SrcContainer, typename DstContainer>
constexpr auto BitwiseNot(
    const SrcContainer& src, DstContainer& result, size_t bit_width) -> void {
  for (size_t i = 0; i < src.size(); ++i) {
    result[i] = ~src[i];
  }
  MaskToWidth(result, bit_width);
}

// Bitwise AND: result[i] = lhs[i] & rhs[i]
template <typename Container1, typename Container2, typename DstContainer>
constexpr auto BitwiseAnd(
    const Container1& lhs, const Container2& rhs, DstContainer& result)
    -> void {
  for (size_t i = 0; i < lhs.size(); ++i) {
    result[i] = lhs[i] & rhs[i];
  }
}

// Bitwise OR: result[i] = lhs[i] | rhs[i]
template <typename Container1, typename Container2, typename DstContainer>
constexpr auto BitwiseOr(
    const Container1& lhs, const Container2& rhs, DstContainer& result)
    -> void {
  for (size_t i = 0; i < lhs.size(); ++i) {
    result[i] = lhs[i] | rhs[i];
  }
}

// Bitwise XOR: result[i] = lhs[i] ^ rhs[i]
template <typename Container1, typename Container2, typename DstContainer>
constexpr auto BitwiseXor(
    const Container1& lhs, const Container2& rhs, DstContainer& result)
    -> void {
  for (size_t i = 0; i < lhs.size(); ++i) {
    result[i] = lhs[i] ^ rhs[i];
  }
}

// ============================================================================
// Arithmetic operations
// ============================================================================

// Addition with carry propagation, result masked to bit_width
template <typename Container1, typename Container2, typename DstContainer>
constexpr auto Add(
    const Container1& lhs, const Container2& rhs, DstContainer& result,
    size_t bit_width) -> void {
  uint64_t carry = 0;
  for (size_t i = 0; i < lhs.size(); ++i) {
    uint64_t a = lhs[i];
    uint64_t b = rhs[i];
    uint64_t sum = a + b;
    uint64_t carry1 = (sum < a) ? 1 : 0;
    uint64_t sum2 = sum + carry;
    uint64_t carry2 = (sum2 < sum) ? 1 : 0;
    result[i] = sum2;
    carry = carry1 | carry2;
  }
  MaskToWidth(result, bit_width);
}

// ============================================================================
// Shift operations
// ============================================================================

// Left shift: result = src << amount, masked to bit_width
template <typename SrcContainer, typename DstContainer>
constexpr auto ShiftLeft(
    const SrcContainer& src, DstContainer& result, size_t amount,
    size_t bit_width) -> void {
  // Zero-initialize result
  for (size_t i = 0; i < result.size(); ++i) {
    result[i] = 0;
  }

  if (amount == 0) {
    for (size_t i = 0; i < src.size(); ++i) {
      result[i] = src[i];
    }
    return;
  }
  if (amount >= bit_width) {
    return;  // All zeros
  }

  size_t word_shift = amount / 64;
  size_t bit_shift = amount % 64;

  if (bit_shift == 0) {
    for (size_t i = word_shift; i < src.size(); ++i) {
      result[i] = src[i - word_shift];
    }
  } else {
    for (size_t i = word_shift; i < src.size(); ++i) {
      result[i] = src[i - word_shift] << bit_shift;
      if (i > word_shift) {
        result[i] |= src[i - word_shift - 1] >> (64 - bit_shift);
      }
    }
  }
  MaskToWidth(result, bit_width);
}

// Logical right shift: result = src >> amount (zero fill)
template <typename SrcContainer, typename DstContainer>
constexpr auto ShiftRightLogical(
    const SrcContainer& src, DstContainer& result, size_t amount) -> void {
  // Zero-initialize result
  for (size_t i = 0; i < result.size(); ++i) {
    result[i] = 0;
  }

  if (amount == 0) {
    for (size_t i = 0; i < src.size(); ++i) {
      result[i] = src[i];
    }
    return;
  }

  size_t word_shift = amount / 64;
  size_t bit_shift = amount % 64;

  if (word_shift >= src.size()) {
    return;  // All zeros
  }

  if (bit_shift == 0) {
    for (size_t i = 0; i + word_shift < src.size(); ++i) {
      result[i] = src[i + word_shift];
    }
  } else {
    for (size_t i = 0; i + word_shift < src.size(); ++i) {
      result[i] = src[i + word_shift] >> bit_shift;
      if (i + word_shift + 1 < src.size()) {
        result[i] |= src[i + word_shift + 1] << (64 - bit_shift);
      }
    }
  }
}

// Arithmetic right shift: result = src >> amount with sign extension
template <typename SrcContainer, typename DstContainer>
constexpr auto ShiftRightArithmetic(
    const SrcContainer& src, DstContainer& result, size_t amount,
    size_t bit_width) -> void {
  // Get sign bit before shifting
  size_t sign_word = (bit_width - 1) / 64;
  size_t sign_bit_pos = (bit_width - 1) % 64;
  bool sign_bit =
      (sign_word < src.size()) && ((src[sign_word] >> sign_bit_pos) & 1);

  // Do logical shift first
  ShiftRightLogical(src, result, amount);

  // Fill in sign bits if negative
  if (sign_bit) {
    size_t fill_start = (amount >= bit_width) ? 0 : bit_width - amount;
    for (size_t i = fill_start; i < bit_width; ++i) {
      size_t word_idx = i / 64;
      size_t bit_idx = i % 64;
      if (word_idx < result.size()) {
        result[word_idx] |= (1ULL << bit_idx);
      }
    }
    MaskToWidth(result, bit_width);
  }
}

// ============================================================================
// Comparison and utility
// ============================================================================

// Check if two word arrays are equal
template <typename Container1, typename Container2>
constexpr auto Equal(const Container1& lhs, const Container2& rhs) -> bool {
  if (lhs.size() != rhs.size()) {
    return false;
  }
  for (size_t i = 0; i < lhs.size(); ++i) {
    if (lhs[i] != rhs[i]) {
      return false;
    }
  }
  return true;
}

// Check if all words are zero
template <typename Container>
constexpr auto IsZero(const Container& words) -> bool {
  for (size_t i = 0; i < words.size(); ++i) {
    if (words[i] != 0) {
      return false;
    }
  }
  return true;
}

// Get a single bit (returns 0 or 1)
template <typename Container>
constexpr auto GetBit(const Container& words, size_t index) -> uint64_t {
  size_t word_idx = WordIndex(index);
  size_t bit_idx = BitInWord(index);
  if (word_idx >= words.size()) {
    return 0;
  }
  return (words[word_idx] >> bit_idx) & 1;
}

// Set a single bit
template <typename Container>
constexpr auto SetBit(Container& words, size_t index, bool value) -> void {
  size_t word_idx = WordIndex(index);
  size_t bit_idx = BitInWord(index);
  if (word_idx >= words.size()) {
    return;
  }
  if (value) {
    words[word_idx] |= (1ULL << bit_idx);
  } else {
    words[word_idx] &= ~(1ULL << bit_idx);
  }
}

// ============================================================================
// String formatting (non-constexpr, uses runtime formatting)
// ============================================================================

// Convert to hex string (MSB first), e.g. "0x1234abcd"
// FormatFn should be a callable that formats a uint64_t to hex string
template <typename Container, typename FormatFn>
auto ToHexStringImpl(const Container& words, FormatFn format_fn)
    -> std::string {
  if (words.size() == 0) {
    return "0x0";
  }

  std::string result = "0x";
  bool leading = true;

  // Print words from MSB to LSB
  for (size_t i = words.size(); i > 0; --i) {
    uint64_t word = words[i - 1];
    if (leading && word == 0 && i > 1) {
      continue;  // Skip leading zeros
    }
    if (leading) {
      result += format_fn(word, false);  // Not padded
      leading = false;
    } else {
      result += format_fn(word, true);  // Padded to 16 chars
    }
  }

  if (leading) {
    result += "0";
  }
  return result;
}

// Convert to binary string (MSB first), e.g. "0b1010"
template <typename Container>
auto ToBinaryString(const Container& words) -> std::string {
  if (words.size() == 0) {
    return "0b0";
  }

  std::string result = "0b";
  bool leading = true;

  for (size_t i = words.size(); i > 0; --i) {
    uint64_t word = words[i - 1];
    if (leading && word == 0 && i > 1) {
      continue;  // Skip leading zero words
    }
    if (leading) {
      // Find first set bit
      int first_bit = 63;
      while (first_bit >= 0 && ((word >> first_bit) & 1) == 0) {
        --first_bit;
      }
      first_bit = (first_bit < 0) ? 0 : first_bit;
      for (int bit = first_bit; bit >= 0; --bit) {
        result += (((word >> bit) & 1) != 0) ? '1' : '0';
      }
      leading = false;
    } else {
      for (int bit = 63; bit >= 0; --bit) {
        result += (((word >> bit) & 1) != 0) ? '1' : '0';
      }
    }
  }

  if (leading) {
    result += "0";
  }
  return result;
}

// Convert to octal string (MSB first), e.g. "0o777"
// FormatFn should be a callable that formats a uint64_t to octal string
template <typename Container, typename FormatFn>
auto ToOctalStringImpl(const Container& words, FormatFn format_fn)
    -> std::string {
  if (words.size() == 0) {
    return "0o0";
  }

  std::string result = "0o";
  bool leading = true;

  for (size_t i = words.size(); i > 0; --i) {
    uint64_t word = words[i - 1];
    if (leading && word == 0 && i > 1) {
      continue;  // Skip leading zeros
    }
    if (leading) {
      result += format_fn(word, false);  // Not padded
      leading = false;
    } else {
      // 64 bits = 21 octal digits + 1 bit (3 bits per octal digit)
      result += format_fn(word, true);  // Padded to 22 chars
    }
  }

  if (leading) {
    result += "0";
  }
  return result;
}

}  // namespace lyra::common::wide_ops
