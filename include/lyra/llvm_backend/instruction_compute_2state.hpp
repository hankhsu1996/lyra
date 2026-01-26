#pragma once

#include <cstdint>
#include <vector>

#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

// Lower 2-state compute instruction.
// Dispatches to appropriate rvalue lowering based on rvalue kind.
void LowerCompute2State(
    Context& context, const mir::Compute& compute, uint32_t bit_width);

// Lower binary rvalue (arithmetic, comparison, shift, etc).
auto LowerBinaryRvalue(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type,
    uint32_t semantic_width) -> llvm::Value*;

// Lower unary rvalue (negation, reduction, etc).
auto LowerUnaryRvalue(
    Context& context, const mir::UnaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value*;

// Lower packed concatenation rvalue.
auto LowerConcatRvalue(
    Context& context, const mir::ConcatRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value*;

// Lower index validity check (bounds checking).
auto LowerIndexValidity(
    Context& context, const mir::IndexValidityRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value*;

// Lower guarded use (bounds-checked array read).
auto LowerGuardedUse(
    Context& context, const mir::GuardedUseRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
