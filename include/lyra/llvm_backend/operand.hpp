#pragma once

#include "llvm/IR/Value.h"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower a MIR operand to an LLVM Value (coerces 4-state to 2-state integer)
auto LowerOperand(Context& context, const mir::Operand& operand)
    -> llvm::Value*;

// Lower a MIR operand without 4-state coercion (returns struct for 4-state)
auto LowerOperandRaw(Context& context, const mir::Operand& operand)
    -> llvm::Value*;

// Lower a MIR constant to an LLVM Value
auto LowerConstant(Context& context, const Constant& constant) -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
