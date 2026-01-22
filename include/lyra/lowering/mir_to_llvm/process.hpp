#pragma once

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "lyra/lowering/mir_to_llvm/context.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower a single MIR process to LLVM IR.
// The exit_block is the target for Return terminators.
void LowerProcess(
    Context& context, const mir::Process& process,
    llvm::BasicBlock* exit_block);

// Generate a complete process function with resume dispatch.
// The function signature is: void process_N(void* state, uint32_t resume_block)
// Returns the generated function.
auto GenerateProcessFunction(
    Context& context, const mir::Process& process, const std::string& name)
    -> llvm::Function*;

}  // namespace lyra::lowering::mir_to_llvm
