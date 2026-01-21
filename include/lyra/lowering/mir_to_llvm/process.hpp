#pragma once

#include "lyra/lowering/mir_to_llvm/context.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower a single MIR process to LLVM IR
void LowerProcess(Context& context, const mir::Process& process);

}  // namespace lyra::lowering::mir_to_llvm
