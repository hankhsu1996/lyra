#pragma once

#include "llvm/IR/BasicBlock.h"
#include "lyra/lowering/mir_to_llvm/context.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower a single MIR process to LLVM IR.
// The exit_block is the target for Return terminators.
void LowerProcess(
    Context& context, const mir::Process& process,
    llvm::BasicBlock* exit_block);

}  // namespace lyra::lowering::mir_to_llvm
