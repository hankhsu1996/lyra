#pragma once

#include <cstdint>

#include "llvm/ADT/APInt.h"

namespace lyra {

// 4-state encoding: (value, unknown) pair where:
//   unknown=0, value=0 -> 0
//   unknown=0, value=1 -> 1
//   unknown=1, value=0 -> X
//   unknown=1, value=1 -> Z
struct FourStatePair {
  llvm::APInt value;
  llvm::APInt unknown;
};

// Semantic mask: clear bits above semantic_width in both planes
inline void MaskFourState(FourStatePair& pair, uint32_t semantic_width) {
  uint32_t storage_width = pair.value.getBitWidth();
  if (semantic_width < storage_width) {
    llvm::APInt mask =
        llvm::APInt::getLowBitsSet(storage_width, semantic_width);
    pair.value &= mask;
    pair.unknown &= mask;
  }
}

}  // namespace lyra
