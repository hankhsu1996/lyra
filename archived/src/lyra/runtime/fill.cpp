#include "lyra/runtime/fill.hpp"

#include <algorithm>
#include <cstdint>
#include <cstring>

namespace {

constexpr uint32_t kBitsPerWord = 64;

auto WordsNeeded(uint32_t bit_width) -> uint32_t {
  return (bit_width + kBitsPerWord - 1) / kBitsPerWord;
}

auto GetMask(uint32_t bits) -> uint64_t {
  if (bits >= 64) {
    return ~uint64_t{0};
  }
  return (uint64_t{1} << bits) - 1;
}

// Read a 64-bit word from potentially unaligned memory using memcpy.
auto ReadWord(const void* ptr, uint32_t word_idx) -> uint64_t {
  uint64_t result = 0;
  auto byte_offset = static_cast<size_t>(word_idx) * sizeof(uint64_t);
  // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
  std::memcpy(
      &result, static_cast<const uint8_t*>(ptr) + byte_offset,
      sizeof(uint64_t));
  return result;
}

// Write a 64-bit word to potentially unaligned memory using memcpy.
void WriteWord(void* ptr, uint32_t word_idx, uint64_t value) {
  auto byte_offset = static_cast<size_t>(word_idx) * sizeof(uint64_t);
  // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
  std::memcpy(
      static_cast<uint8_t*>(ptr) + byte_offset, &value, sizeof(uint64_t));
}

// Insert bits from src into dst at bit_offset.
// All access via memcpy to avoid alignment UB.
void InsertBitsInPlace(
    void* dst, uint32_t dst_bits, const void* src, uint32_t bit_offset,
    uint32_t width) {
  if (width == 0 || bit_offset >= dst_bits) {
    return;
  }

  uint32_t effective_width = width;
  if (bit_offset + width > dst_bits) {
    effective_width = dst_bits - bit_offset;
  }

  uint32_t src_words = WordsNeeded(width);
  uint32_t first_dst_word = bit_offset / kBitsPerWord;
  uint32_t last_dst_word = (bit_offset + effective_width - 1) / kBitsPerWord;
  uint32_t dst_words = WordsNeeded(dst_bits);

  for (uint32_t dst_idx = first_dst_word;
       dst_idx <= last_dst_word && dst_idx < dst_words; ++dst_idx) {
    uint32_t word_start_bit = dst_idx * kBitsPerWord;
    uint32_t word_end_bit = word_start_bit + kBitsPerWord;

    uint32_t slice_start = std::max(word_start_bit, bit_offset);
    uint32_t slice_end = std::min(word_end_bit, bit_offset + effective_width);

    if (slice_start >= slice_end) {
      continue;
    }

    uint32_t bits_to_write = slice_end - slice_start;
    uint32_t dst_bit_pos = slice_start - word_start_bit;
    uint32_t src_bit_pos = slice_start - bit_offset;

    uint32_t src_word_idx = src_bit_pos / kBitsPerWord;
    uint32_t src_bit_in_word = src_bit_pos % kBitsPerWord;

    uint64_t src_val = 0;
    if (src_word_idx < src_words) {
      src_val = ReadWord(src, src_word_idx) >> src_bit_in_word;
    }
    if (src_bit_in_word != 0 && src_word_idx + 1 < src_words) {
      src_val |= ReadWord(src, src_word_idx + 1)
                 << (kBitsPerWord - src_bit_in_word);
    }
    src_val &= GetMask(bits_to_write);

    uint64_t dst_word = ReadWord(dst, dst_idx);
    uint64_t dst_mask = GetMask(bits_to_write) << dst_bit_pos;
    dst_word &= ~dst_mask;
    dst_word |= src_val << dst_bit_pos;
    WriteWord(dst, dst_idx, dst_word);
  }
}

void MaskTopWord(void* words, uint32_t bit_width) {
  if (bit_width == 0) {
    return;
  }
  uint32_t num_words = WordsNeeded(bit_width);
  uint32_t top_bits = bit_width % kBitsPerWord;
  if (top_bits != 0) {
    uint64_t top_word = ReadWord(words, num_words - 1);
    top_word &= GetMask(top_bits);
    WriteWord(words, num_words - 1, top_word);
  }
}

}  // namespace

extern "C" {

void LyraFillPackedElements(
    void* dst_val_plane, void* dst_unk_plane, uint32_t total_bits,
    const uint64_t* src_val_words, const uint64_t* src_unk_words,
    uint32_t elem_bits, uint32_t elem_count) {
  // Zero-initialize destination planes
  uint32_t dst_bytes = WordsNeeded(total_bits) * sizeof(uint64_t);
  std::memset(dst_val_plane, 0, dst_bytes);
  if (dst_unk_plane != nullptr) {
    std::memset(dst_unk_plane, 0, dst_bytes);
  }

  // Fill each element position
  for (uint32_t i = 0; i < elem_count; ++i) {
    uint32_t bit_offset = i * elem_bits;
    InsertBitsInPlace(
        dst_val_plane, total_bits, src_val_words, bit_offset, elem_bits);
    if (dst_unk_plane != nullptr && src_unk_words != nullptr) {
      InsertBitsInPlace(
          dst_unk_plane, total_bits, src_unk_words, bit_offset, elem_bits);
    }
  }

  // Mask top bits
  MaskTopWord(dst_val_plane, total_bits);
  if (dst_unk_plane != nullptr) {
    MaskTopWord(dst_unk_plane, total_bits);
  }
}
}
