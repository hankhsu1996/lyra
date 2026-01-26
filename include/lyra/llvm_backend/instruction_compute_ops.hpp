#pragma once

#include <cstdint>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"

namespace lyra::lowering::mir_to_llvm {

auto ReturnsI1(mir::BinaryOp op) -> bool;
auto IsComparisonOp(mir::BinaryOp op) -> bool;
auto IsSignedComparisonOp(mir::BinaryOp op) -> bool;
auto IsShiftOp(mir::BinaryOp op) -> bool;
auto IsLogicalOp(mir::BinaryOp op) -> bool;
auto IsCaseMatchOp(mir::BinaryOp op) -> bool;
auto IsReductionOp(mir::UnaryOp op) -> bool;

auto GetSemanticMask(llvm::Type* ty, uint32_t semantic_width) -> llvm::Value*;

auto ApplyWidthMask(
    Context& context, llvm::Value* value, uint32_t semantic_width)
    -> llvm::Value*;

auto SignExtendToStorage(
    llvm::IRBuilderBase& builder, llvm::Value* val, uint32_t semantic_width)
    -> llvm::Value*;

auto GetOperandPackedWidth(Context& context, const mir::Operand& operand)
    -> uint32_t;

auto LowerBinaryArith(
    Context& context, mir::BinaryOp op, llvm::Value* lhs, llvm::Value* rhs)
    -> Result<llvm::Value*>;

auto LowerBinaryComparison(
    Context& context, mir::BinaryOp op, llvm::Value* lhs, llvm::Value* rhs)
    -> Result<llvm::Value*>;

auto LowerShiftOp(
    Context& context, mir::BinaryOp op, llvm::Value* value,
    llvm::Value* shift_amount, uint32_t semantic_width) -> llvm::Value*;

auto LowerShiftOpUnknown(
    Context& context, mir::BinaryOp op, llvm::Value* unk,
    llvm::Value* shift_amount, uint32_t semantic_width) -> llvm::Value*;

auto LowerUnaryOp(
    Context& context, mir::UnaryOp op, llvm::Value* operand,
    llvm::Type* storage_type, uint32_t operand_bit_width)
    -> Result<llvm::Value*>;

}  // namespace lyra::lowering::mir_to_llvm
