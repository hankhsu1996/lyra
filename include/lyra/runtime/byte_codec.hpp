#pragma once

#include <cstdint>
#include <span>

#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// LRM 21.3.4.4 "first byte read fills the MSBs". Assembles `bytes` into a
// PackedArray of `width` bits, MSB-first: `bytes[0]` occupies the
// destination's top byte. Missing bytes (`bytes.size() * 8 < width`) zero-
// fill the LSBs ("as much as available" semantics); excess bytes are
// silently truncated. `is_four_state` matches the destination's declared
// state-axis; the unknown plane is left zero (LRM "2-value data" for
// $fread / $fread-style reads).
//
// The byte span is `char` rather than `uint8_t` so callers can pass
// std::istream::read buffers straight through; bit operations inside
// reinterpret each char's eight bits as unsigned.
[[nodiscard]] auto BytesToPackedArray(
    std::span<const char> bytes, std::uint64_t width, bool is_signed,
    bool is_four_state) -> value::PackedArray;

}  // namespace lyra::runtime
