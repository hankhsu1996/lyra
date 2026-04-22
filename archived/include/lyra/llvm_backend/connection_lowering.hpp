#pragma once

#include <vector>

#include "lyra/llvm_backend/kernel_types.hpp"

namespace lyra::lowering::mir_to_llvm {

struct LoweringInput;

struct LoweredConnectionArtifacts {
  std::vector<ConnectionKernelEntry> kernel_entries;
};

// Lower connection artifacts from bound connections.
// Kernel bindings produce flat-slot entries for slot-to-slot connections.
auto LowerConnectionArtifacts(const LoweringInput& input)
    -> LoweredConnectionArtifacts;

}  // namespace lyra::lowering::mir_to_llvm
