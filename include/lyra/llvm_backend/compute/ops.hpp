#pragma once

#include <cstdint>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"

namespace lyra::lowering::mir_to_llvm {

auto ReturnsI1(mir::BinaryOp op) -> bool;
auto IsComparisonOp(mir::BinaryOp op) -> bool;
auto IsSignedComparisonOp(mir::BinaryOp op) -> bool;
auto IsWildcardComparisonOp(mir::BinaryOp op) -> bool;
auto IsShiftOp(mir::BinaryOp op) -> bool;
auto IsLogicalOp(mir::BinaryOp op) -> bool;
auto IsCaseMatchOp(mir::BinaryOp op) -> bool;
auto IsCaseEqualityOp(mir::BinaryOp op) -> bool;
auto IsReductionOp(mir::UnaryOp op) -> bool;

auto GetSemanticMask(llvm::Type* ty, uint32_t semantic_width) -> llvm::Value*;

auto ApplyWidthMask(
    llvm::IRBuilder<>& builder, llvm::Value* value, uint32_t semantic_width)
    -> llvm::Value*;

auto SignExtendToStorage(
    llvm::IRBuilderBase& builder, llvm::Value* val, uint32_t semantic_width)
    -> llvm::Value*;

auto GetOperandPackedWidth(
    const CuFacts& facts, Context& context, const mir::Operand& operand)
    -> uint32_t;

auto LowerBinaryArith(
    llvm::IRBuilder<>& builder, mir::BinaryOp op, llvm::Value* lhs,
    llvm::Value* rhs) -> llvm::Value*;

auto LowerBinaryComparison(
    llvm::IRBuilder<>& builder, mir::BinaryOp op, llvm::Value* lhs,
    llvm::Value* rhs) -> llvm::Value*;

// Lowers a comparison operation to i1, handling operand width coercion.
//
// Contract:
// - lhs/rhs are LLVM integer values (any width)
// - lhs_semantic_width/rhs_semantic_width are the MIR semantic widths
// - For signed comparisons, each operand is sign-extended from its semantic
//   width to the comparison width (max of both semantic widths)
// - Returns i1 (caller is responsible for extending to storage type)
auto LowerCompareToI1(
    llvm::IRBuilder<>& builder, mir::BinaryOp op, llvm::Value* lhs,
    llvm::Value* rhs, uint32_t lhs_semantic_width, uint32_t rhs_semantic_width)
    -> llvm::Value*;

auto LowerShiftOp(
    llvm::IRBuilder<>& builder, mir::BinaryOp op, llvm::Value* value,
    llvm::Value* shift_amount, uint32_t semantic_width) -> llvm::Value*;

auto LowerShiftOpUnknown(
    llvm::IRBuilder<>& builder, mir::BinaryOp op, llvm::Value* unk,
    llvm::Value* shift_amount, uint32_t semantic_width) -> llvm::Value*;

auto LowerUnaryOp(
    llvm::IRBuilder<>& builder, mir::UnaryOp op, llvm::Value* operand,
    llvm::Type* storage_type, uint32_t operand_bit_width) -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
