#pragma once

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "lyra/llvm_backend/context.hpp"
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

// Declare a user function without generating its body.
// Used for two-pass generation to enable mutual recursion.
auto DeclareUserFunction(
    Context& context, mir::FunctionId func_id, const std::string& name)
    -> llvm::Function*;

// Generate the body for a user function.
// The function must have been declared first with DeclareUserFunction.
void DefineUserFunction(
    Context& context, mir::FunctionId func_id, llvm::Function* func);

}  // namespace lyra::lowering::mir_to_llvm
