#include "lyra/mir/interp/runtime_integral_words.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <vector>

#include "lyra/mir/interp/runtime_integral_ops.hpp"
#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

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

void AssertNormalized(const RuntimeIntegral& val, uint32_t bit_width) {
  size_t expected_words = WordsNeeded(bit_width);
  assert(
      val.value.size() == expected_words &&
      "value.size() != WordsNeeded(bit_width)");
  if (bit_width > 0) {
    uint32_t top_bits = bit_width % kBitsPerWord;
    if (top_bits != 0) {
      uint64_t mask = GetMask(top_bits);
      assert((val.value.back() & ~mask) == 0 && "garbage bits in top word");
    }
  }
}

auto GetBit(const RuntimeIntegral& val, uint32_t bit_pos, uint32_t bit_width)
    -> bool {
  if (bit_pos >= bit_width) {
    return false;
  }
  size_t word_idx = bit_pos / kBitsPerWord;
  size_t bit_in_word = bit_pos % kBitsPerWord;
  assert(word_idx < val.value.size() && "GetBit: word_idx out of range");
  return ((val.value[word_idx] >> bit_in_word) & 1) != 0;
}

void SetBit(RuntimeIntegral& val, uint32_t bit_pos) {
  size_t word_idx = bit_pos / kBitsPerWord;
  size_t bit_in_word = bit_pos % kBitsPerWord;
  assert(word_idx < val.value.size() && "SetBit: word index out of range");
  val.value[word_idx] |= (uint64_t{1} << bit_in_word);
}

auto GetSignBit(const RuntimeIntegral& val, uint32_t bit_width) -> bool {
  assert(bit_width > 0 && "GetSignBit: bit_width must be positive");
  return GetBit(val, bit_width - 1, bit_width);
}

auto NormalizeToWidth(const RuntimeIntegral& val, uint32_t bit_width)
    -> RuntimeIntegral {
  auto result = MakeKnownIntegral(bit_width);
  size_t copy_words = std::min(val.value.size(), result.value.size());
  for (size_t i = 0; i < copy_words; ++i) {
    result.value[i] = val.value[i];
  }
  MaskTopWord(result.value, bit_width);
  return result;
}

}  // namespace lyra::mir::interp
