#pragma once

#include "lyra/lowering/mir_to_llvm/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower a single MIR instruction to LLVM IR
void LowerInstruction(Context& context, const mir::Instruction& instruction);

}  // namespace lyra::lowering::mir_to_llvm
