#include "lyra/runtime/byte_codec.hpp"

#include <algorithm>
#include <cstdint>
#include <span>
#include <vector>

#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

auto BytesToPackedArray(
    std::span<const char> bytes, std::uint64_t width, bool is_signed,
    bool is_four_state) -> value::PackedArray {
  const auto word_count = static_cast<std::size_t>((width + 63U) / 64U);
  std::vector<std::uint64_t> val_words(word_count, 0U);
  const auto total_input_bits = static_cast<std::uint64_t>(bytes.size()) * 8U;
  const auto bits_to_use = std::min<std::uint64_t>(total_input_bits, width);
  // Walk input bits MSB-first. The i-th written bit lands at destination
  // bit position (width - 1 - i); the source bit is byte (i/8)'s (7 - i%8)
  // position. Excess input bits (when bytes carries more than width bits)
  // never enter the loop; shortfalls leave the trailing val_words bits at
  // zero from construction.
  for (std::uint64_t i = 0; i < bits_to_use; ++i) {
    const auto byte =
        static_cast<std::uint64_t>(static_cast<unsigned char>(bytes[i / 8U]));
    const auto src_shift = 7U - (i % 8U);
    if (((byte >> src_shift) & 1U) != 0U) {
      const auto bit_pos = width - 1U - i;
      const auto word_ix = static_cast<std::size_t>(bit_pos / 64U);
      const auto bit_ix = static_cast<std::size_t>(bit_pos % 64U);
      val_words[word_ix] |= std::uint64_t{1} << bit_ix;
    }
  }
  return value::PackedArray::FromWords(
      std::span<const std::uint64_t>{val_words},
      std::span<const std::uint64_t>{}, width, is_signed, is_four_state);
}

}  // namespace lyra::runtime
