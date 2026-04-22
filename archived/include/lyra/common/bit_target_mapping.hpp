#pragma once

#include <cstdint>

namespace lyra::runtime {

// Normalized affine mapping: sv_index -> storage_bit.
//   logical_offset = (sv_index - index_base) * index_step
//   valid when 0 <= logical_offset < total_bits
// Computed once at lowering time. Used identically by codegen and runtime.
struct BitTargetMapping {
  int32_t index_base = 0;
  int32_t index_step = 1;
  uint32_t total_bits = 0;
};

}  // namespace lyra::runtime
