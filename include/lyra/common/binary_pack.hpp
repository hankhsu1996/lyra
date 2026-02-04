#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <vector>

namespace lyra::common {

// Pack big-endian bytes into a little-endian 64-bit word array.
//
// Input bytes are in big-endian order: bytes[0] is MSB.
// Output words are in little-endian order suitable for MakeIntegralWide()
// or direct memcpy into x86 backing store.
//
// byte_count:    number of valid bytes in src (may be < bytes_per_elem)
// bytes_per_elem: logical element width in bytes (ceil(element_bits / 8))
// element_bits:  actual element width in bits; the top word is masked to this
//                width so callers like MakeIntegralWide get clean padding.
//                If 0, no masking is applied (backwards compat for LLVM runtime
//                which writes raw bytes to backing store).
inline auto PackBigEndianToWords(
    std::span<const uint8_t> src, size_t bytes_per_elem,
    size_t element_bits = 0) -> std::vector<uint64_t> {
  size_t num_words = (bytes_per_elem + 7) / 8;
  std::vector<uint64_t> words(num_words, 0);
  for (size_t i = 0; i < src.size(); ++i) {
    size_t bit_pos = (bytes_per_elem - 1 - i) * 8;
    size_t word_idx = bit_pos / 64;
    size_t bit_in_word = bit_pos % 64;
    if (word_idx < words.size()) {
      words[word_idx] |= static_cast<uint64_t>(src[i]) << bit_in_word;
    }
  }
  // Mask the top word to element_bits to clear padding above the actual width.
  // Required when element_bits is not a multiple of 8 (e.g., 9-bit elements
  // use 2 bytes but only 9 bits are valid).
  if (element_bits > 0 && !words.empty()) {
    size_t top_word_idx = (element_bits - 1) / 64;
    size_t bits_in_top = element_bits % 64;
    if (bits_in_top != 0 && top_word_idx < words.size()) {
      uint64_t mask = (static_cast<uint64_t>(1) << bits_in_top) - 1;
      words[top_word_idx] &= mask;
    }
  }
  return words;
}

}  // namespace lyra::common
