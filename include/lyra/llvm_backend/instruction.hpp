#pragma once

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower a single MIR instruction to LLVM IR
auto LowerInstruction(Context& context, const mir::Instruction& instruction)
    -> Result<void>;

}  // namespace lyra::lowering::mir_to_llvm
