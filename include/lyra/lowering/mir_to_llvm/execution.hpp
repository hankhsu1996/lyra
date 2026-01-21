#pragma once

#include "lyra/lowering/mir_to_llvm/lower.hpp"

namespace lyra::lowering::mir_to_llvm {

// JIT compile and execute the LLVM module
// Returns the exit code from main (0 = success)
auto ExecuteJit(LoweringResult& result) -> int;

}  // namespace lyra::lowering::mir_to_llvm
