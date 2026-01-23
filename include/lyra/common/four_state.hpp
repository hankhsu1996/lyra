#pragma once

#include <cstdint>

#include "llvm/ADT/APInt.h"

namespace lyra {

// 4-state encoding: (a, b) pair where:
//   b=0, a=0 -> 0
//   b=0, a=1 -> 1
//   b=1, a=0 -> X
//   b=1, a=1 -> Z
struct FourStatePair {
  llvm::APInt a;
  llvm::APInt b;
};

// Convert MIR (value, x_mask, z_mask) -> LLVM (a, b)
//   b = x_mask | z_mask
//   a = (value & ~b) | z_mask
inline auto ToAB(
    const llvm::APInt& value, const llvm::APInt& x_mask,
    const llvm::APInt& z_mask) -> FourStatePair {
  llvm::APInt b = x_mask | z_mask;
  llvm::APInt a = (value & ~b) | z_mask;
  return {.a = std::move(a), .b = std::move(b)};
}

// Semantic mask: clear bits above semantic_width in both a and b
inline void MaskFourState(FourStatePair& pair, uint32_t semantic_width) {
  uint32_t storage_width = pair.a.getBitWidth();
  if (semantic_width < storage_width) {
    llvm::APInt mask =
        llvm::APInt::getLowBitsSet(storage_width, semantic_width);
    pair.a &= mask;
    pair.b &= mask;
  }
}

}  // namespace lyra
