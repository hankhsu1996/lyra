#include "lyra/llvm_backend/compute/two_state.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <variant>
#include <vector>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/ErrorHandling.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/runtime_query_kind.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/compute/four_state.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/compute/ops.hpp"
#include "lyra/llvm_backend/compute/result.hpp"
#include "lyra/llvm_backend/compute/rvalue.hpp"
#include "lyra/llvm_backend/compute/string.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto GetOperandTypeId(Context& context, const mir::Operand& operand) -> TypeId {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  return std::visit(
      common::Overloaded{
          [&](const Constant& c) -> TypeId { return c.type; },
          [&](mir::PlaceId place_id) -> TypeId {
            const auto& place = arena[place_id];
            return mir::TypeOfPlace(types, place);
          },
      },
      operand.payload);
}

auto IsStringOperand(Context& context, const mir::Operand& operand) -> bool {
  TypeId type_id = GetOperandTypeId(context, operand);
  return context.GetTypeArena()[type_id].Kind() == TypeKind::kString;
}

}  // namespace

auto LowerBinaryRvalue2State(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  llvm::Type* storage_type = packed_context.storage_type;
  uint32_t semantic_width = packed_context.bit_width;

  if (IsStringOperand(context, operands[0])) {
    auto result = LowerStringBinaryOp(context, info, operands, storage_type);
    if (!result) return std::unexpected(result.error());
    return ComputeResult::TwoState(*result);
  }

  if (IsCaseMatchOp(info.op)) {
    auto result = LowerCaseMatchOp(context, info, operands, storage_type);
    if (!result) return std::unexpected(result.error());
    return ComputeResult::TwoState(*result);
  }

  if (IsCaseEqualityOp(info.op)) {
    auto result = LowerCaseEqualityOp(context, info, operands, storage_type);
    if (!result) return std::unexpected(result.error());
    return ComputeResult::TwoState(*result);
  }

  auto& builder = context.GetBuilder();

  auto lhs_or_err = LowerOperand(context, operands[0]);
  if (!lhs_or_err) return std::unexpected(lhs_or_err.error());
  auto rhs_or_err = LowerOperand(context, operands[1]);
  if (!rhs_or_err) return std::unexpected(rhs_or_err.error());
  llvm::Value* lhs = *lhs_or_err;
  llvm::Value* rhs = *rhs_or_err;

  if (IsComparisonOp(info.op)) {
    uint32_t lhs_width = GetOperandPackedWidth(context, operands[0]);
    uint32_t rhs_width = GetOperandPackedWidth(context, operands[1]);
    auto cmp_or_err =
        LowerCompareToI1(context, info.op, lhs, rhs, lhs_width, rhs_width);
    if (!cmp_or_err) return std::unexpected(cmp_or_err.error());
    auto* result = builder.CreateZExt(*cmp_or_err, storage_type, "cmp.ext");
    return ComputeResult::TwoState(result);
  }

  // For non-comparison ops, coerce to result storage type
  lhs = builder.CreateZExtOrTrunc(lhs, storage_type, "lhs.coerce");
  rhs = builder.CreateZExtOrTrunc(rhs, storage_type, "rhs.coerce");

  if (IsShiftOp(info.op)) {
    auto* result = LowerShiftOp(context, info.op, lhs, rhs, semantic_width);
    return ComputeResult::TwoState(result);
  }

  auto result_or_err = LowerBinaryArith(context, info.op, lhs, rhs);
  if (!result_or_err) return std::unexpected(result_or_err.error());

  if (ReturnsI1(info.op)) {
    auto* result = builder.CreateZExt(*result_or_err, storage_type, "bool.ext");
    return ComputeResult::TwoState(result);
  }

  return ComputeResult::TwoState(*result_or_err);
}

// Lower regular (non-reduction) unary ops at storage width.
//
// Contract:
// - Operates entirely at storage width
// - Handles: Plus, Minus, BitwiseNot, LogicalNot
auto LowerRegularUnary2State(
    Context& context, mir::UnaryOp op, llvm::Value* operand,
    const PackedComputeContext& packed_context) -> Result<llvm::Value*> {
  llvm::Type* storage_type = packed_context.storage_type;
  uint32_t semantic_width = packed_context.bit_width;

  auto& builder = context.GetBuilder();

  // Coerce operand to storage width
  auto* coerced =
      builder.CreateZExtOrTrunc(operand, storage_type, "reg2.coerce");

  return LowerUnaryOp(context, op, coerced, storage_type, semantic_width);
}

// Lower reduction unary ops at operand semantic width.
//
// Contract:
// - Operates at operand semantic width (NOT storage width)
// - Produces 1-bit semantic result, extended to storage width
// - Handles: ReductionAnd, ReductionNand, ReductionOr, ReductionNor,
//            ReductionXor, ReductionXnor
auto LowerReduction2State(
    Context& context, mir::UnaryOp op, llvm::Value* operand,
    uint32_t operand_semantic_width, const PackedComputeContext& packed_context)
    -> Result<llvm::Value*> {
  llvm::Type* storage_type = packed_context.storage_type;

  // Keep operand at its semantic width - LowerUnaryOp uses
  // operand_semantic_width for mask creation (e.g., all-ones mask for
  // ReductionAnd)
  return LowerUnaryOp(
      context, op, operand, storage_type, operand_semantic_width);
}

// Dispatcher for 2-state unary operations.
//
// Routes to:
// - LowerRegularUnary2State for shape-preserving ops (N-bit -> N-bit)
// - LowerReduction2State for reducing ops (N-bit -> 1-bit)
auto LowerUnaryRvalue2State(
    Context& context, const mir::UnaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  auto operand_or_err = LowerOperand(context, operands[0]);
  if (!operand_or_err) return std::unexpected(operand_or_err.error());
  llvm::Value* operand = *operand_or_err;

  Result<llvm::Value*> result;
  if (IsReductionOp(info.op)) {
    uint32_t operand_semantic_width =
        GetOperandPackedWidth(context, operands[0]);
    result = LowerReduction2State(
        context, info.op, operand, operand_semantic_width, packed_context);
  } else {
    result = LowerRegularUnary2State(context, info.op, operand, packed_context);
  }

  if (!result) return std::unexpected(result.error());
  return ComputeResult::TwoState(*result);
}

auto LowerConcatRvalue2State(
    Context& context, const mir::ConcatRvalueInfo& /*info*/,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  llvm::Type* storage_type = packed_context.storage_type;

  auto& builder = context.GetBuilder();

  if (operands.empty()) {
    throw common::InternalError(
        "LowerConcatRvalue2State", "concat must have at least one operand");
  }

  uint32_t first_width = GetOperandPackedWidth(context, operands[0]);
  auto first_or_err = LowerOperand(context, operands[0]);
  if (!first_or_err) return std::unexpected(first_or_err.error());
  llvm::Value* first = *first_or_err;
  auto* first_ty = llvm::Type::getIntNTy(builder.getContext(), first_width);
  first = builder.CreateZExtOrTrunc(first, first_ty, "concat.trunc");
  llvm::Value* acc = builder.CreateZExt(first, storage_type, "concat.ext");

  for (size_t i = 1; i < operands.size(); ++i) {
    uint32_t op_width = GetOperandPackedWidth(context, operands[i]);
    auto op_or_err = LowerOperand(context, operands[i]);
    if (!op_or_err) return std::unexpected(op_or_err.error());
    llvm::Value* op = *op_or_err;

    auto* op_ty = llvm::Type::getIntNTy(builder.getContext(), op_width);
    op = builder.CreateZExtOrTrunc(op, op_ty, "concat.trunc");
    op = builder.CreateZExt(op, storage_type, "concat.ext");

    auto* shift_amount = llvm::ConstantInt::get(storage_type, op_width);
    acc = builder.CreateShl(acc, shift_amount, "concat.shl");
    acc = builder.CreateOr(acc, op, "concat.or");
  }

  return ComputeResult::TwoState(acc);
}

auto LowerIndexValidity2State(
    Context& context, const mir::IndexValidityRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  llvm::Type* storage_type = packed_context.storage_type;

  auto& builder = context.GetBuilder();

  auto index_or_err = LowerOperand(context, operands[0]);
  if (!index_or_err) return std::unexpected(index_or_err.error());
  llvm::Value* index = *index_or_err;
  auto* idx_type = index->getType();

  auto* lower = llvm::ConstantInt::get(idx_type, info.lower_bound, true);
  auto* ge_lower = builder.CreateICmpSGE(index, lower, "idx.ge_lower");

  auto* upper = llvm::ConstantInt::get(idx_type, info.upper_bound, true);
  auto* le_upper = builder.CreateICmpSLE(index, upper, "idx.le_upper");

  llvm::Value* valid = builder.CreateAnd(ge_lower, le_upper, "idx.valid");

  if (info.check_known) {
    auto raw_or_err = LowerOperandRaw(context, operands[0]);
    if (!raw_or_err) return std::unexpected(raw_or_err.error());
    llvm::Value* raw = *raw_or_err;
    if (raw->getType()->isStructTy()) {
      auto* unk = builder.CreateExtractValue(raw, 1, "idx.unk");
      auto* zero = llvm::ConstantInt::get(unk->getType(), 0);
      auto* is_known = builder.CreateICmpEQ(unk, zero, "idx.is_known");
      valid = builder.CreateAnd(valid, is_known, "idx.valid_known");
    }
  }

  auto* result = builder.CreateZExt(valid, storage_type, "idx.ext");
  return ComputeResult::TwoState(result);
}

auto LowerGuardedUse2State(
    Context& context, const mir::GuardedUseRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  llvm::Type* storage_type = packed_context.storage_type;

  auto& builder = context.GetBuilder();

  auto valid_or_err = LowerOperand(context, operands[0]);
  if (!valid_or_err) return std::unexpected(valid_or_err.error());
  llvm::Value* valid = *valid_or_err;
  if (valid->getType()->getIntegerBitWidth() > 1) {
    auto* zero = llvm::ConstantInt::get(valid->getType(), 0);
    valid = builder.CreateICmpNE(valid, zero, "gu.tobool");
  }

  auto* func = builder.GetInsertBlock()->getParent();
  auto* do_read_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "gu.read", func);
  auto* oob_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "gu.oob", func);
  auto* merge_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "gu.merge", func);

  builder.CreateCondBr(valid, do_read_bb, oob_bb);

  builder.SetInsertPoint(do_read_bb);
  auto place_operand = mir::Operand::Use(info.place);
  auto read_val_or_err = LowerOperand(context, place_operand);
  if (!read_val_or_err) return std::unexpected(read_val_or_err.error());
  llvm::Value* read_val = *read_val_or_err;
  read_val = builder.CreateZExtOrTrunc(read_val, storage_type, "gu.fit");
  auto* do_read_end_bb = builder.GetInsertBlock();
  builder.CreateBr(merge_bb);

  builder.SetInsertPoint(oob_bb);
  llvm::Value* oob_val = llvm::ConstantInt::get(storage_type, 0);
  builder.CreateBr(merge_bb);

  builder.SetInsertPoint(merge_bb);
  auto* phi = builder.CreatePHI(storage_type, 2, "gu.result");
  phi->addIncoming(read_val, do_read_end_bb);
  phi->addIncoming(oob_val, oob_bb);
  return ComputeResult::TwoState(phi);
}

auto LowerRuntimeQuery2State(
    Context& context, const mir::RuntimeQueryRvalueInfo& info,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  llvm::Type* storage_type = packed_context.storage_type;

  auto& builder = context.GetBuilder();

  switch (info.kind) {
    case RuntimeQueryKind::kTimeRawTicks: {
      auto* raw = builder.CreateCall(
          context.GetLyraGetTime(), {context.GetEnginePointer()});
      llvm::Value* result = raw;
      if (raw->getType() != storage_type) {
        result = builder.CreateZExtOrTrunc(raw, storage_type, "time.fit");
      }
      return ComputeResult::TwoState(result);
    }
  }
  llvm_unreachable("unhandled RuntimeQueryKind");
}

}  // namespace lyra::lowering::mir_to_llvm
