#include "lyra/llvm_backend/instruction_compute_2state.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
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
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction_compute_4state.hpp"
#include "lyra/llvm_backend/instruction_compute_ops.hpp"
#include "lyra/llvm_backend/instruction_compute_string.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
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

auto LowerBinaryRvalue(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type,
    uint32_t semantic_width) -> Result<llvm::Value*> {
  if (IsStringOperand(context, operands[0])) {
    return LowerStringBinaryOp(context, info, operands, storage_type);
  }

  if (IsCaseMatchOp(info.op)) {
    return LowerCaseMatchOp(context, info, operands, storage_type);
  }

  if (IsCaseEqualityOp(info.op)) {
    return LowerCaseEqualityOp(context, info, operands, storage_type);
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
    return builder.CreateZExt(*cmp_or_err, storage_type, "cmp.ext");
  }

  // For non-comparison ops, coerce to result storage type
  lhs = builder.CreateZExtOrTrunc(lhs, storage_type, "lhs.coerce");
  rhs = builder.CreateZExtOrTrunc(rhs, storage_type, "rhs.coerce");

  if (IsShiftOp(info.op)) {
    return LowerShiftOp(context, info.op, lhs, rhs, semantic_width);
  }

  auto result_or_err = LowerBinaryArith(context, info.op, lhs, rhs);
  if (!result_or_err) return std::unexpected(result_or_err.error());

  if (ReturnsI1(info.op)) {
    return builder.CreateZExt(*result_or_err, storage_type, "bool.ext");
  }

  return *result_or_err;
}

auto LowerUnaryRvalue(
    Context& context, const mir::UnaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();

  uint32_t operand_bit_width = GetOperandPackedWidth(context, operands[0]);

  auto operand_or_err = LowerOperand(context, operands[0]);
  if (!operand_or_err) return std::unexpected(operand_or_err.error());
  llvm::Value* operand = *operand_or_err;

  if (!IsReductionOp(info.op)) {
    operand = builder.CreateZExtOrTrunc(operand, storage_type, "op.coerce");
  }

  return LowerUnaryOp(
      context, info.op, operand, storage_type, operand_bit_width);
}

auto LowerConcatRvalue(
    Context& context, const mir::ConcatRvalueInfo& /*info*/,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();

  if (operands.empty()) {
    throw common::InternalError(
        "LowerConcatRvalue", "concat must have at least one operand");
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

  return acc;
}

auto LowerIndexValidity(
    Context& context, const mir::IndexValidityRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> Result<llvm::Value*> {
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

  return builder.CreateZExt(valid, storage_type, "idx.ext");
}

auto LowerGuardedUse(
    Context& context, const mir::GuardedUseRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> Result<llvm::Value*> {
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
  return phi;
}

auto LowerCompute2State(
    Context& context, const mir::Compute& compute, uint32_t bit_width)
    -> Result<void> {
  auto& builder = context.GetBuilder();

  auto target_ptr_or_err = context.GetPlacePointer(compute.target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;
  auto storage_type_or_err = context.GetPlaceLlvmType(compute.target);
  if (!storage_type_or_err) return std::unexpected(storage_type_or_err.error());
  llvm::Type* storage_type = *storage_type_or_err;

  auto result_or_err = std::visit(
      common::Overloaded{
          [&](const mir::UnaryRvalueInfo& info) -> Result<llvm::Value*> {
            return LowerUnaryRvalue(
                context, info, compute.value.operands, storage_type);
          },
          [&](const mir::BinaryRvalueInfo& info) -> Result<llvm::Value*> {
            return LowerBinaryRvalue(
                context, info, compute.value.operands, storage_type, bit_width);
          },
          [&](const mir::ConcatRvalueInfo& info) -> Result<llvm::Value*> {
            return LowerConcatRvalue(
                context, info, compute.value.operands, storage_type);
          },
          [&](const mir::IndexValidityRvalueInfo& info)
              -> Result<llvm::Value*> {
            return LowerIndexValidity(
                context, info, compute.value.operands, storage_type);
          },
          [&](const mir::GuardedUseRvalueInfo& info) -> Result<llvm::Value*> {
            return LowerGuardedUse(
                context, info, compute.value.operands, storage_type);
          },
          [&](const mir::RuntimeQueryRvalueInfo& info) -> Result<llvm::Value*> {
            switch (info.kind) {
              case RuntimeQueryKind::kTimeRawTicks: {
                auto* raw = builder.CreateCall(
                    context.GetLyraGetTime(), {context.GetEnginePointer()});
                if (raw->getType() == storage_type) {
                  return raw;
                }
                return builder.CreateZExtOrTrunc(raw, storage_type, "time.fit");
              }
            }
            llvm_unreachable("unhandled RuntimeQueryKind");
          },
          [&](const mir::UserCallRvalueInfo& info) -> Result<llvm::Value*> {
            llvm::Function* callee = context.GetUserFunction(info.callee);

            std::vector<llvm::Value*> args;
            args.push_back(context.GetDesignPointer());
            args.push_back(context.GetEnginePointer());

            for (const auto& operand : compute.value.operands) {
              auto arg_or_err = LowerOperand(context, operand);
              if (!arg_or_err) return std::unexpected(arg_or_err.error());
              args.push_back(*arg_or_err);
            }

            llvm::Value* call_result =
                builder.CreateCall(callee, args, "user_call");

            if (call_result->getType() != storage_type &&
                call_result->getType()->isIntegerTy() &&
                storage_type->isIntegerTy()) {
              call_result = builder.CreateZExtOrTrunc(
                  call_result, storage_type, "call.fit");
            }

            return call_result;
          },
          [&](const auto& /*info*/) -> Result<llvm::Value*> {
            return std::unexpected(
                context.GetDiagnosticContext().MakeUnsupported(
                    context.GetCurrentOrigin(),
                    std::format(
                        "unsupported rvalue kind in 2-state path: {}",
                        mir::GetRvalueKind(compute.value.info)),
                    UnsupportedCategory::kFeature));
          },
      },
      compute.value.info);

  if (!result_or_err) return std::unexpected(result_or_err.error());

  llvm::Value* result = ApplyWidthMask(context, *result_or_err, bit_width);

  builder.CreateStore(result, target_ptr);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
