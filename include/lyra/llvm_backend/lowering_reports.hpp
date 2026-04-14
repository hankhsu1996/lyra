#pragma once

#include <cstdint>

namespace lyra::lowering::mir_to_llvm {

struct LoweringReport {
  uint32_t relay_slots_eliminated = 0;
};

}  // namespace lyra::lowering::mir_to_llvm
