#pragma once

#include <cstddef>

#include "lyra/common/type.hpp"

namespace lyra::compiler::codegen {

// Returns true if bit width requires WideBit (> 64 bits)
constexpr auto IsWideWidth(size_t width) -> bool {
  return width > 64;
}

// Get element bit width after applying N indices to a multi-dimensional type
inline auto GetElementWidthAfterIndices(
    const common::Type& base_type, size_t num_indices) -> size_t {
  const common::Type* current = &base_type;
  for (size_t i = 0; i < num_indices; ++i) {
    current = &current->GetElementType();
  }
  return current->GetBitWidth();
}

}  // namespace lyra::compiler::codegen
