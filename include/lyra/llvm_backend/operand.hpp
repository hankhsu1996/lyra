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

// Lower a MIR operand as a storage representation matching target_type exactly.
// Used for aggregate element insertion where LLVM demands type-exact values.
//
// Allowed conversions (all unsigned/zero-extended — storage is typeless bits):
//   - 2-state iN → 2-state iM  (ZExtOrTrunc for storage width rounding)
//   - 2-state iN → 4-state {iM, iM}  (wrap as {zext(val), 0})
//   - 4-state {iN, iN} → 4-state {iM, iM}  (ZExtOrTrunc both planes)
// Any other combination throws InternalError.
auto LowerOperandAsStorage(
    Context& context, const mir::Operand& operand, llvm::Type* target_type)
    -> llvm::Value*;

// Lower a MIR constant to an LLVM Value
auto LowerConstant(Context& context, const Constant& constant) -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
