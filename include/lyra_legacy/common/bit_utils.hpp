#pragma once

#include <cstdint>

namespace lyra::common {

// Make bit mask with safety for 64-bit width
constexpr auto MakeBitMask(uint32_t bit_width) -> uint64_t {
  return (bit_width >= 64) ? ~0ULL : (1ULL << bit_width) - 1;
}

// Sign-extend a truncated unsigned integer to a full int64_t
constexpr auto SignExtend(uint64_t value, std::size_t bit_width) -> int64_t {
  if (bit_width == 0 || bit_width >= 64) {
    return static_cast<int64_t>(value);  // no-op
  }
  uint64_t sign_bit = 1ULL << (bit_width - 1);
  return static_cast<int64_t>((value ^ sign_bit) - sign_bit);
}

}  // namespace lyra::common
