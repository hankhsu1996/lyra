#include "lyra/llvm_backend/instruction_compute_2state.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <variant>
#include <vector>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/ErrorHandling.h"
#include "lyra/common/constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction_compute_4state.hpp"
#include "lyra/llvm_backend/instruction_compute_ops.hpp"
#include "lyra/llvm_backend/instruction_compute_string.hpp"
#include "lyra/llvm_backend/operand.hpp"
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
    uint32_t semantic_width) -> llvm::Value* {
  if (IsStringOperand(context, operands[0])) {
    return LowerStringBinaryOp(context, info, operands, storage_type);
  }

  if (IsCaseMatchOp(info.op)) {
    return LowerCaseMatchOp(context, info, operands, storage_type);
  }

  auto& builder = context.GetBuilder();

  llvm::Value* lhs = LowerOperand(context, operands[0]);
  llvm::Value* rhs = LowerOperand(context, operands[1]);

  lhs = builder.CreateZExtOrTrunc(lhs, storage_type, "lhs.coerce");
  rhs = builder.CreateZExtOrTrunc(rhs, storage_type, "rhs.coerce");

  if (IsComparisonOp(info.op)) {
    if (IsSignedComparisonOp(info.op)) {
      uint32_t op_width = GetOperandPackedWidth(context, operands[0]);
      lhs = SignExtendToStorage(builder, lhs, op_width);
      rhs = SignExtendToStorage(builder, rhs, op_width);
    }
    llvm::Value* cmp = LowerBinaryComparison(context, info.op, lhs, rhs);
    return builder.CreateZExt(cmp, storage_type, "cmp.ext");
  }

  if (IsShiftOp(info.op)) {
    return LowerShiftOp(context, info.op, lhs, rhs, semantic_width);
  }

  llvm::Value* result = LowerBinaryArith(context, info.op, lhs, rhs);

  if (ReturnsI1(info.op)) {
    return builder.CreateZExt(result, storage_type, "bool.ext");
  }

  return result;
}

auto LowerUnaryRvalue(
    Context& context, const mir::UnaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  uint32_t operand_bit_width = GetOperandPackedWidth(context, operands[0]);

  llvm::Value* operand = LowerOperand(context, operands[0]);

  if (!IsReductionOp(info.op)) {
    operand = builder.CreateZExtOrTrunc(operand, storage_type, "op.coerce");
  }

  return LowerUnaryOp(
      context, info.op, operand, storage_type, operand_bit_width);
}

auto LowerConcatRvalue(
    Context& context, const mir::ConcatRvalueInfo& /*info*/,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  if (operands.empty()) {
    throw common::InternalError(
        "LowerConcatRvalue", "concat must have at least one operand");
  }

  uint32_t first_width = GetOperandPackedWidth(context, operands[0]);
  llvm::Value* first = LowerOperand(context, operands[0]);
  auto* first_ty = llvm::Type::getIntNTy(builder.getContext(), first_width);
  first = builder.CreateZExtOrTrunc(first, first_ty, "concat.trunc");
  llvm::Value* acc = builder.CreateZExt(first, storage_type, "concat.ext");

  for (size_t i = 1; i < operands.size(); ++i) {
    uint32_t op_width = GetOperandPackedWidth(context, operands[i]);
    llvm::Value* op = LowerOperand(context, operands[i]);

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
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  llvm::Value* index = LowerOperand(context, operands[0]);
  auto* idx_type = index->getType();

  auto* lower = llvm::ConstantInt::get(idx_type, info.lower_bound, true);
  auto* ge_lower = builder.CreateICmpSGE(index, lower, "idx.ge_lower");

  auto* upper = llvm::ConstantInt::get(idx_type, info.upper_bound, true);
  auto* le_upper = builder.CreateICmpSLE(index, upper, "idx.le_upper");

  llvm::Value* valid = builder.CreateAnd(ge_lower, le_upper, "idx.valid");

  if (info.check_known) {
    llvm::Value* raw = LowerOperandRaw(context, operands[0]);
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
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  llvm::Value* valid = LowerOperand(context, operands[0]);
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
  llvm::Value* read_val = LowerOperand(context, place_operand);
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

void LowerCompute2State(
    Context& context, const mir::Compute& compute, uint32_t bit_width) {
  auto& builder = context.GetBuilder();

  llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
  llvm::Type* storage_type = context.GetPlaceLlvmType(compute.target);

  llvm::Value* result = std::visit(
      common::Overloaded{
          [&](const mir::UnaryRvalueInfo& info) {
            return LowerUnaryRvalue(
                context, info, compute.value.operands, storage_type);
          },
          [&](const mir::BinaryRvalueInfo& info) {
            return LowerBinaryRvalue(
                context, info, compute.value.operands, storage_type, bit_width);
          },
          [&](const mir::ConcatRvalueInfo& info) {
            return LowerConcatRvalue(
                context, info, compute.value.operands, storage_type);
          },
          [&](const mir::IndexValidityRvalueInfo& info) {
            return LowerIndexValidity(
                context, info, compute.value.operands, storage_type);
          },
          [&](const mir::GuardedUseRvalueInfo& info) {
            return LowerGuardedUse(
                context, info, compute.value.operands, storage_type);
          },
          [&](const mir::RuntimeQueryRvalueInfo& info) -> llvm::Value* {
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
          [&](const mir::UserCallRvalueInfo& info) -> llvm::Value* {
            llvm::Function* callee = context.GetUserFunction(info.callee);

            std::vector<llvm::Value*> args;
            args.push_back(context.GetDesignPointer());
            args.push_back(context.GetEnginePointer());

            for (const auto& operand : compute.value.operands) {
              args.push_back(LowerOperand(context, operand));
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
          [&](const auto& /*info*/) -> llvm::Value* {
            throw common::UnsupportedErrorException(
                common::UnsupportedLayer::kMirToLlvm,
                common::UnsupportedKind::kFeature, context.GetCurrentOrigin(),
                std::format(
                    "unsupported rvalue kind in 2-state path: {}",
                    mir::GetRvalueKind(compute.value.info)));
          },
      },
      compute.value.info);

  result = ApplyWidthMask(context, result, bit_width);

  builder.CreateStore(result, target_ptr);
}

}  // namespace lyra::lowering::mir_to_llvm
