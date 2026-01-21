#pragma once

#include "llvm/IR/Value.h"
#include "lyra/lowering/mir_to_llvm/context.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower a MIR operand to an LLVM Value
auto LowerOperand(Context& context, const mir::Operand& operand)
    -> llvm::Value*;

// Lower a MIR constant to an LLVM Value
auto LowerConstant(Context& context, const Constant& constant) -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
