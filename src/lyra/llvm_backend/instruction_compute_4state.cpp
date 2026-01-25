#include "lyra/llvm_backend/instruction_compute_4state.hpp"

#include <cstdint>
#include <format>
#include <variant>
#include <vector>

#include "llvm/ADT/APInt.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/runtime_query_kind.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction_compute_ops.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

struct FourStateValue {
  llvm::Value* value;
  llvm::Value* unknown;
};

auto ExtractFourState(llvm::IRBuilderBase& builder, llvm::Value* struct_val)
    -> FourStateValue {
  auto* val = builder.CreateExtractValue(struct_val, 0, "fs.val");
  auto* unk = builder.CreateExtractValue(struct_val, 1, "fs.unk");
  return {.value = val, .unknown = unk};
}

auto PackFourState(
    llvm::IRBuilderBase& builder, llvm::StructType* struct_type,
    llvm::Value* val, llvm::Value* unk) -> llvm::Value* {
  llvm::Value* result = llvm::UndefValue::get(struct_type);
  result = builder.CreateInsertValue(result, val, 0, "fs.pack.val");
  result = builder.CreateInsertValue(result, unk, 1, "fs.pack.unk");
  return result;
}

auto IsOperandFourState(Context& context, const mir::Operand& operand) -> bool {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  return std::visit(
      Overloaded{
          [&](const Constant& c) {
            const Type& type = types[c.type];
            return IsPacked(type) && IsPackedFourState(type, types);
          },
          [&](mir::PlaceId place_id) {
            const auto& place = arena[place_id];
            TypeId type_id = mir::TypeOfPlace(types, place);
            const Type& type = types[type_id];
            return IsPacked(type) && IsPackedFourState(type, types);
          },
      },
      operand.payload);
}

auto LowerOperandFourState(
    Context& context, const mir::Operand& operand, llvm::Type* elem_type)
    -> FourStateValue {
  auto& builder = context.GetBuilder();

  if (IsOperandFourState(context, operand)) {
    llvm::Value* loaded = LowerOperandRaw(context, operand);
    return ExtractFourState(builder, loaded);
  }

  llvm::Value* loaded_val = LowerOperand(context, operand);
  auto* val = builder.CreateZExtOrTrunc(loaded_val, elem_type, "fs2.val");
  auto* unk = llvm::ConstantInt::get(elem_type, 0);
  return {.value = val, .unknown = unk};
}

auto LowerConcatRvalue4State(
    Context& context, const mir::ConcatRvalueInfo& /*info*/,
    const std::vector<mir::Operand>& operands, llvm::Type* elem_type)
    -> FourStateValue {
  auto& builder = context.GetBuilder();

  if (operands.empty()) {
    throw common::InternalError(
        "LowerConcatRvalue4State", "concat must have at least one operand");
  }

  uint32_t first_width = GetOperandPackedWidth(context, operands[0]);
  auto first = LowerOperandFourState(context, operands[0], elem_type);

  auto* first_ty = llvm::Type::getIntNTy(builder.getContext(), first_width);
  auto* val_first =
      builder.CreateZExtOrTrunc(first.value, first_ty, "cat4.val.trunc");
  auto* unk_first =
      builder.CreateZExtOrTrunc(first.unknown, first_ty, "cat4.unk.trunc");
  auto* acc_val = builder.CreateZExt(val_first, elem_type, "cat4.val.ext");
  auto* acc_unk = builder.CreateZExt(unk_first, elem_type, "cat4.unk.ext");

  for (size_t i = 1; i < operands.size(); ++i) {
    uint32_t op_width = GetOperandPackedWidth(context, operands[i]);
    auto op = LowerOperandFourState(context, operands[i], elem_type);

    auto* op_ty = llvm::Type::getIntNTy(builder.getContext(), op_width);
    auto* val_op = builder.CreateZExtOrTrunc(op.value, op_ty, "cat4.val.trunc");
    auto* unk_op =
        builder.CreateZExtOrTrunc(op.unknown, op_ty, "cat4.unk.trunc");
    val_op = builder.CreateZExt(val_op, elem_type, "cat4.val.ext");
    unk_op = builder.CreateZExt(unk_op, elem_type, "cat4.unk.ext");

    auto* shift_amount = llvm::ConstantInt::get(elem_type, op_width);
    acc_val = builder.CreateShl(acc_val, shift_amount, "cat4.val.shl");
    acc_val = builder.CreateOr(acc_val, val_op, "cat4.val.or");
    acc_unk = builder.CreateShl(acc_unk, shift_amount, "cat4.unk.shl");
    acc_unk = builder.CreateOr(acc_unk, unk_op, "cat4.unk.or");
  }

  return {.value = acc_val, .unknown = acc_unk};
}

auto LowerUnaryRvalue4State(
    Context& context, const mir::UnaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* elem_type,
    uint32_t semantic_width) -> FourStateValue {
  auto& builder = context.GetBuilder();
  auto* zero = llvm::ConstantInt::get(elem_type, 0);

  auto src = LowerOperandFourState(context, operands[0], elem_type);
  src.value = builder.CreateZExtOrTrunc(src.value, elem_type, "un4.val");
  src.unknown = builder.CreateZExtOrTrunc(src.unknown, elem_type, "un4.unk");

  switch (info.op) {
    case mir::UnaryOp::kPlus:
      return src;

    case mir::UnaryOp::kMinus: {
      auto* any_unk = builder.CreateICmpNE(src.unknown, zero, "un4.anyunk");
      auto* sem_mask = GetSemanticMask(elem_type, semantic_width);
      return {
          .value = builder.CreateNeg(src.value, "un4.neg"),
          .unknown =
              builder.CreateSelect(any_unk, sem_mask, zero, "un4.neg.unk"),
      };
    }

    case mir::UnaryOp::kBitwiseNot:
      return {
          .value = builder.CreateNot(src.value, "un4.not"),
          .unknown = src.unknown,
      };

    case mir::UnaryOp::kLogicalNot: {
      auto* known = builder.CreateAnd(
          src.value, builder.CreateNot(src.unknown), "un4.known");
      auto* is_nonzero = builder.CreateICmpNE(known, zero, "un4.nz");
      auto* negated = builder.CreateNot(is_nonzero, "un4.lnot");
      auto* any_unk = builder.CreateICmpNE(src.unknown, zero, "un4.anyunk");
      return {
          .value = builder.CreateZExt(negated, elem_type, "un4.lnot.val"),
          .unknown = builder.CreateZExt(any_unk, elem_type, "un4.lnot.unk"),
      };
    }

    default: {
      auto* masked_val = ApplyWidthMask(context, src.value, semantic_width);
      auto* masked_unk = ApplyWidthMask(context, src.unknown, semantic_width);
      auto* known = builder.CreateAnd(
          masked_val, builder.CreateNot(masked_unk), "un4.red.known");

      auto* red_result =
          LowerUnaryOp(context, info.op, known, elem_type, semantic_width);

      auto* any_unk = builder.CreateICmpNE(masked_unk, zero, "un4.red.taint");
      return {
          .value = red_result,
          .unknown = builder.CreateZExt(any_unk, elem_type, "un4.red.unk"),
      };
    }
  }
}

auto LowerGuardedUse4State(
    Context& context, const mir::GuardedUseRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* elem_type,
    uint32_t semantic_width) -> FourStateValue {
  auto& builder = context.GetBuilder();

  llvm::Value* valid = LowerOperand(context, operands[0]);
  if (valid->getType()->getIntegerBitWidth() > 1) {
    auto* zero = llvm::ConstantInt::get(valid->getType(), 0);
    valid = builder.CreateICmpNE(valid, zero, "gu4.tobool");
  }

  auto* func = builder.GetInsertBlock()->getParent();
  auto* do_read_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "gu4.read", func);
  auto* oob_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "gu4.oob", func);
  auto* merge_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "gu4.merge", func);

  builder.CreateCondBr(valid, do_read_bb, oob_bb);

  builder.SetInsertPoint(do_read_bb);
  auto place_operand = mir::Operand::Use(info.place);
  auto read_fs = LowerOperandFourState(context, place_operand, elem_type);
  auto* do_read_end_bb = builder.GetInsertBlock();
  builder.CreateBr(merge_bb);

  builder.SetInsertPoint(oob_bb);
  auto* oob_val = llvm::ConstantInt::get(elem_type, 0);
  uint32_t storage_width = elem_type->getIntegerBitWidth();
  auto oob_unk_ap = llvm::APInt::getLowBitsSet(storage_width, semantic_width);
  auto* oob_unk = llvm::ConstantInt::get(elem_type, oob_unk_ap);
  builder.CreateBr(merge_bb);

  builder.SetInsertPoint(merge_bb);
  auto* phi_val = builder.CreatePHI(elem_type, 2, "gu4.val");
  phi_val->addIncoming(read_fs.value, do_read_end_bb);
  phi_val->addIncoming(oob_val, oob_bb);

  auto* phi_unk = builder.CreatePHI(elem_type, 2, "gu4.unk");
  phi_unk->addIncoming(read_fs.unknown, do_read_end_bb);
  phi_unk->addIncoming(oob_unk, oob_bb);

  return {.value = phi_val, .unknown = phi_unk};
}

auto LowerBinaryRvalue4State(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* elem_type,
    uint32_t semantic_width) -> FourStateValue {
  auto& builder = context.GetBuilder();
  auto* zero = llvm::ConstantInt::get(elem_type, 0);

  auto lhs = LowerOperandFourState(context, operands[0], elem_type);
  auto rhs = LowerOperandFourState(context, operands[1], elem_type);

  lhs.value = builder.CreateZExtOrTrunc(lhs.value, elem_type, "bin4.lhs.val");
  rhs.value = builder.CreateZExtOrTrunc(rhs.value, elem_type, "bin4.rhs.val");
  lhs.unknown =
      builder.CreateZExtOrTrunc(lhs.unknown, elem_type, "bin4.lhs.unk");
  rhs.unknown =
      builder.CreateZExtOrTrunc(rhs.unknown, elem_type, "bin4.rhs.unk");

  auto* combined_unk = builder.CreateOr(lhs.unknown, rhs.unknown, "bin4.unk");

  if (IsComparisonOp(info.op)) {
    auto* cmp_lhs = lhs.value;
    auto* cmp_rhs = rhs.value;
    if (IsSignedComparisonOp(info.op)) {
      uint32_t op_width = GetOperandPackedWidth(context, operands[0]);
      cmp_lhs = SignExtendToStorage(builder, cmp_lhs, op_width);
      cmp_rhs = SignExtendToStorage(builder, cmp_rhs, op_width);
    }
    auto* cmp = LowerBinaryComparison(context, info.op, cmp_lhs, cmp_rhs);
    auto* taint = builder.CreateICmpNE(combined_unk, zero, "bin4.taint");
    return {
        .value = builder.CreateZExt(cmp, elem_type, "bin4.cmp.val"),
        .unknown = builder.CreateZExt(taint, elem_type, "bin4.cmp.unk"),
    };
  }

  if (IsLogicalOp(info.op)) {
    auto* val = LowerBinaryArith(context, info.op, lhs.value, rhs.value);
    auto* taint = builder.CreateICmpNE(combined_unk, zero, "bin4.taint");
    return {
        .value = builder.CreateZExt(val, elem_type, "bin4.log.val"),
        .unknown = builder.CreateZExt(taint, elem_type, "bin4.log.unk"),
    };
  }

  if (IsShiftOp(info.op)) {
    auto* val =
        LowerShiftOp(context, info.op, lhs.value, rhs.value, semantic_width);
    auto* unk = LowerShiftOpUnknown(
        context, info.op, lhs.unknown, rhs.value, semantic_width);
    return {.value = val, .unknown = unk};
  }

  if (ReturnsI1(info.op)) {
    throw common::InternalError(
        "LowerBinaryRvalue4State",
        "i1-producing op must be handled as comparison or logical");
  }
  auto* val = LowerBinaryArith(context, info.op, lhs.value, rhs.value);
  return {.value = val, .unknown = combined_unk};
}

}  // namespace

auto LowerCaseMatchOp(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  uint32_t operand_width = GetOperandPackedWidth(context, operands[0]);
  auto* elem_type =
      llvm::Type::getIntNTy(context.GetLlvmContext(), operand_width);

  auto lhs = LowerOperandFourState(context, operands[0], elem_type);
  auto rhs = LowerOperandFourState(context, operands[1], elem_type);

  lhs.value = builder.CreateZExtOrTrunc(lhs.value, elem_type, "cm.lhs.val");
  lhs.unknown = builder.CreateZExtOrTrunc(lhs.unknown, elem_type, "cm.lhs.unk");
  rhs.value = builder.CreateZExtOrTrunc(rhs.value, elem_type, "cm.rhs.val");
  rhs.unknown = builder.CreateZExtOrTrunc(rhs.unknown, elem_type, "cm.rhs.unk");

  llvm::Value* wildcard = nullptr;
  if (info.op == mir::BinaryOp::kCaseXMatch) {
    wildcard = builder.CreateOr(lhs.unknown, rhs.unknown, "cx.wc");
  } else {
    auto* lhs_z = builder.CreateAnd(lhs.value, lhs.unknown, "cz.lhs.z");
    auto* rhs_z = builder.CreateAnd(rhs.value, rhs.unknown, "cz.rhs.z");
    wildcard = builder.CreateOr(lhs_z, rhs_z, "cz.wc");
  }

  auto* not_wildcard = builder.CreateNot(wildcard, "cm.not.wc");
  auto sem_mask = llvm::APInt::getLowBitsSet(
      elem_type->getIntegerBitWidth(), operand_width);
  auto* mask = builder.CreateAnd(
      not_wildcard, llvm::ConstantInt::get(elem_type, sem_mask), "cm.mask");
  auto* lhs_masked = builder.CreateAnd(lhs.value, mask, "cm.lv");
  auto* rhs_masked = builder.CreateAnd(rhs.value, mask, "cm.rv");
  llvm::Value* result = builder.CreateICmpEQ(lhs_masked, rhs_masked, "cm.veq");

  if (info.op == mir::BinaryOp::kCaseZMatch) {
    auto* lhs_unk_m = builder.CreateAnd(lhs.unknown, mask, "cz.lu");
    auto* rhs_unk_m = builder.CreateAnd(rhs.unknown, mask, "cz.ru");
    auto* unk_eq = builder.CreateICmpEQ(lhs_unk_m, rhs_unk_m, "cz.ueq");
    result = builder.CreateAnd(result, unk_eq, "cz.match");
  }

  return builder.CreateZExt(result, storage_type, "cm.ext");
}

void LowerCompute4State(
    Context& context, const mir::Compute& compute, uint32_t bit_width) {
  auto& builder = context.GetBuilder();

  llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
  llvm::Type* storage_type = context.GetPlaceLlvmType(compute.target);
  auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
  auto* elem_type = struct_type->getElementType(0);

  FourStateValue result = std::visit(
      Overloaded{
          [&](const mir::ConcatRvalueInfo& info) {
            return LowerConcatRvalue4State(
                context, info, compute.value.operands, elem_type);
          },
          [&](const mir::CastRvalueInfo&) -> FourStateValue {
            throw common::InternalError(
                "LowerCompute4State", "casts use LowerCastUnified");
          },
          [&](const mir::UnaryRvalueInfo& info) {
            return LowerUnaryRvalue4State(
                context, info, compute.value.operands, elem_type, bit_width);
          },
          [&](const mir::BinaryRvalueInfo& info) {
            return LowerBinaryRvalue4State(
                context, info, compute.value.operands, elem_type, bit_width);
          },
          [&](const mir::GuardedUseRvalueInfo& info) {
            return LowerGuardedUse4State(
                context, info, compute.value.operands, elem_type, bit_width);
          },
          [&](const mir::RuntimeQueryRvalueInfo& info) -> FourStateValue {
            switch (info.kind) {
              case RuntimeQueryKind::kTime: {
                auto& builder = context.GetBuilder();
                auto* raw = builder.CreateCall(
                    context.GetLyraGetTime(), {context.GetEnginePointer()});
                llvm::Value* val = raw;
                if (raw->getType() != elem_type) {
                  val = builder.CreateZExtOrTrunc(raw, elem_type, "time.fit");
                }
                auto* zero = llvm::ConstantInt::get(elem_type, 0);
                return {.value = val, .unknown = zero};
              }
            }
            llvm_unreachable("unhandled RuntimeQueryKind");
          },
          [&](const auto& /*info*/) -> FourStateValue {
            throw common::UnsupportedErrorException(
                common::UnsupportedLayer::kMirToLlvm,
                common::UnsupportedKind::kFeature, context.GetCurrentOrigin(),
                std::format(
                    "4-state rvalue kind not yet supported: {}",
                    mir::GetRvalueKind(compute.value.info)));
          },
      },
      compute.value.info);

  result.value = ApplyWidthMask(context, result.value, bit_width);
  result.unknown = ApplyWidthMask(context, result.unknown, bit_width);

  llvm::Value* packed =
      PackFourState(builder, struct_type, result.value, result.unknown);
  builder.CreateStore(packed, target_ptr);
}

}  // namespace lyra::lowering::mir_to_llvm
