#pragma once

#include <vector>

#include "lyra/llvm_backend/kernel_types.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

struct LoweringInput;

struct LoweredConnectionArtifacts {
  std::vector<ConnectionKernelEntry> kernel_entries;
  std::vector<mir::ProcessId> non_kernelized_processes;
};

// Lower connection artifacts from bound connections and compiled
// expression connections. Kernel bindings produce flat-slot entries.
// Expression bindings produce synthetic design-global MIR processes
// (cloned from body-local functions with kModuleSlot -> kDesignGlobal
// remapping).
auto LowerConnectionArtifacts(const LoweringInput& input)
    -> LoweredConnectionArtifacts;

}  // namespace lyra::lowering::mir_to_llvm
