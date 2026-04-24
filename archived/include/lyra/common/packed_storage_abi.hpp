#pragma once

#include <cstdint>

namespace lyra {

// Canonical storage byte size for a packed integer lane.
//
// This is the single source of truth for how many bytes a packed value of
// a given bit width occupies in canonical storage. Both the LLVM backend
// and the runtime must agree on this.
//
// Rule: power-of-2 rounding up to 64 bits, then byte-aligned ceil(width/8).
inline auto PackedStorageByteSize(uint32_t bit_width) -> uint32_t {
  if (bit_width <= 8) return 1;
  if (bit_width <= 16) return 2;
  if (bit_width <= 32) return 4;
  if (bit_width <= 64) return 8;
  return (bit_width + 7) / 8;
}

// Byte offset of the unknown lane within 4-state packed storage.
// Known lane at offset 0, unknown lane at offset = PackedStorageByteSize.
inline auto FourStateUnknownByteOffset(uint32_t bit_width) -> uint32_t {
  return PackedStorageByteSize(bit_width);
}

}  // namespace lyra
