#include "lyra/llvm_backend/compute/four_state.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <variant>
#include <vector>

#include <llvm/ADT/APInt.h>
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
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/compute/ops.hpp"
#include "lyra/llvm_backend/compute/result.hpp"
#include "lyra/llvm_backend/compute/rvalue.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto IsOperandFourState(Context& context, const mir::Operand& operand) -> bool {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  return std::visit(
      common::Overloaded{
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
    -> Result<FourStateValue> {
  auto& builder = context.GetBuilder();

  if (IsOperandFourState(context, operand)) {
    auto loaded_or_err = LowerOperandRaw(context, operand);
    if (!loaded_or_err) return std::unexpected(loaded_or_err.error());
    return ExtractFourState(builder, *loaded_or_err);
  }

  auto loaded_val_or_err = LowerOperand(context, operand);
  if (!loaded_val_or_err) return std::unexpected(loaded_val_or_err.error());
  auto* val =
      builder.CreateZExtOrTrunc(*loaded_val_or_err, elem_type, "fs2.val");
  auto* unk = llvm::ConstantInt::get(elem_type, 0);
  return FourStateValue{.value = val, .unknown = unk};
}

}  // namespace

auto LowerConcatRvalue4State(
    Context& context, const mir::ConcatRvalueInfo& /*info*/,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  llvm::Type* elem_type = packed_context.element_type;

  auto& builder = context.GetBuilder();

  if (operands.empty()) {
    throw common::InternalError(
        "LowerConcatRvalue4State", "concat must have at least one operand");
  }

  uint32_t first_width = GetOperandPackedWidth(context, operands[0]);
  auto first_or_err = LowerOperandFourState(context, operands[0], elem_type);
  if (!first_or_err) return std::unexpected(first_or_err.error());
  auto first = *first_or_err;

  auto* first_ty = llvm::Type::getIntNTy(builder.getContext(), first_width);
  auto* val_first =
      builder.CreateZExtOrTrunc(first.value, first_ty, "cat4.val.trunc");
  auto* unk_first =
      builder.CreateZExtOrTrunc(first.unknown, first_ty, "cat4.unk.trunc");
  auto* acc_val = builder.CreateZExt(val_first, elem_type, "cat4.val.ext");
  auto* acc_unk = builder.CreateZExt(unk_first, elem_type, "cat4.unk.ext");

  for (size_t i = 1; i < operands.size(); ++i) {
    uint32_t op_width = GetOperandPackedWidth(context, operands[i]);
    auto op_or_err = LowerOperandFourState(context, operands[i], elem_type);
    if (!op_or_err) return std::unexpected(op_or_err.error());
    auto op = *op_or_err;

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

  return ComputeResult::FourState(acc_val, acc_unk);
}

// Lower regular (non-reduction) unary ops at carrier width.
//
// Contract:
// - Operates entirely at carrier width (elem_type)
// - Returns planes at carrier width
// - Handles: Plus, Minus, BitwiseNot, LogicalNot
auto LowerRegularUnary4State(
    Context& context, mir::UnaryOp op, const FourStateValue& operand,
    const PackedComputeContext& packed_context) -> ComputeResult {
  llvm::Type* carrier_type = packed_context.element_type;
  uint32_t semantic_width = packed_context.bit_width;

  auto& builder = context.GetBuilder();
  auto* zero = llvm::ConstantInt::get(carrier_type, 0);

  // Coerce operand to carrier width
  auto src = FourStateValue{
      .value =
          builder.CreateZExtOrTrunc(operand.value, carrier_type, "reg4.val"),
      .unknown =
          builder.CreateZExtOrTrunc(operand.unknown, carrier_type, "reg4.unk"),
  };

  switch (op) {
    case mir::UnaryOp::kPlus:
      return ComputeResult::FourState(src.value, src.unknown);

    case mir::UnaryOp::kMinus: {
      auto* any_unk = builder.CreateICmpNE(src.unknown, zero, "reg4.anyunk");
      auto* sem_mask = GetSemanticMask(carrier_type, semantic_width);
      auto* neg_val = builder.CreateNeg(src.value, "reg4.neg");
      auto* neg_unk =
          builder.CreateSelect(any_unk, sem_mask, zero, "reg4.neg.unk");
      return ComputeResult::FourState(neg_val, neg_unk);
    }

    case mir::UnaryOp::kBitwiseNot: {
      auto result = FourStateNot(builder, src);
      return ComputeResult::FourState(result.value, result.unknown);
    }

    case mir::UnaryOp::kLogicalNot: {
      auto* known = builder.CreateAnd(
          src.value, builder.CreateNot(src.unknown), "reg4.known");
      auto* is_nonzero = builder.CreateICmpNE(known, zero, "reg4.nz");
      auto* negated = builder.CreateNot(is_nonzero, "reg4.lnot");
      auto* any_unk = builder.CreateICmpNE(src.unknown, zero, "reg4.anyunk");
      auto* val = builder.CreateZExt(negated, carrier_type, "reg4.lnot.val");
      auto* unk = builder.CreateZExt(any_unk, carrier_type, "reg4.lnot.unk");
      return ComputeResult::FourState(val, unk);
    }

    default:
      throw common::InternalError(
          "LowerRegularUnary4State",
          std::format("not a regular unary op: {}", mir::ToString(op)));
  }
}

// Lower reduction unary ops at operand semantic width.
//
// Contract:
// - Operates at operand semantic width (NOT carrier width)
// - Produces 1-bit semantic result
// - Packs to carrier width at the very end before returning
// - Handles: ReductionAnd, ReductionNand, ReductionOr, ReductionNor,
//            ReductionXor, ReductionXnor
auto LowerReduction4State(
    Context& context, mir::UnaryOp op, const FourStateValue& operand,
    uint32_t operand_semantic_width, const PackedComputeContext& packed_context)
    -> Result<ComputeResult> {
  llvm::Type* carrier_type = packed_context.element_type;

  auto& builder = context.GetBuilder();

  // Create working type at operand semantic width
  auto* working_type =
      llvm::Type::getIntNTy(builder.getContext(), operand_semantic_width);

  // Coerce operand planes to working width (operand semantic width)
  auto* working_val =
      builder.CreateZExtOrTrunc(operand.value, working_type, "red4.val");
  auto* working_unk =
      builder.CreateZExtOrTrunc(operand.unknown, working_type, "red4.unk");

  // Create mask at working width for semantic bits
  auto working_mask = llvm::APInt::getLowBitsSet(
      operand_semantic_width, operand_semantic_width);
  auto* mask = llvm::ConstantInt::get(working_type, working_mask);

  // Apply semantic mask at working width
  auto* masked_val = builder.CreateAnd(working_val, mask, "red4.val.masked");
  auto* masked_unk = builder.CreateAnd(working_unk, mask, "red4.unk.masked");

  // Compute known bits (value & ~unknown)
  auto* known = builder.CreateAnd(
      masked_val, builder.CreateNot(masked_unk), "red4.known");

  // Perform reduction at working width, producing 1-bit result
  auto red_result_or_err =
      LowerUnaryOp(context, op, known, carrier_type, operand_semantic_width);
  if (!red_result_or_err) return std::unexpected(red_result_or_err.error());

  // Check if any unknown bits exist (at working width)
  auto* working_zero = llvm::ConstantInt::get(working_type, 0);
  auto* any_unk = builder.CreateICmpNE(masked_unk, working_zero, "red4.taint");

  // Pack to carrier width: val is already at carrier (from LowerUnaryOp),
  // unk needs to be extended from i1
  auto* val = *red_result_or_err;
  auto* unk = builder.CreateZExt(any_unk, carrier_type, "red4.unk.ext");

  return ComputeResult::FourState(val, unk);
}

// Dispatcher for 4-state unary operations.
//
// Routes to:
// - LowerRegularUnary4State for shape-preserving ops (N-bit -> N-bit)
// - LowerReduction4State for reducing ops (N-bit -> 1-bit)
auto LowerUnaryRvalue4State(
    Context& context, const mir::UnaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  llvm::Type* carrier_type = packed_context.element_type;

  // Load operand at carrier width (standard contract for LowerOperandFourState)
  auto src_or_err = LowerOperandFourState(context, operands[0], carrier_type);
  if (!src_or_err) return std::unexpected(src_or_err.error());

  // LogicalNot is like reduction ops: it needs the operand at its original
  // width to compare against zero
  if (IsReductionOp(info.op) || info.op == mir::UnaryOp::kLogicalNot) {
    uint32_t operand_semantic_width =
        GetOperandPackedWidth(context, operands[0]);
    return LowerReduction4State(
        context, info.op, *src_or_err, operand_semantic_width, packed_context);
  }
  return LowerRegularUnary4State(context, info.op, *src_or_err, packed_context);
}

auto LowerGuardedUse4State(
    Context& context, const mir::GuardedUseRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  llvm::Type* elem_type = packed_context.element_type;
  uint32_t semantic_width = packed_context.bit_width;

  auto& builder = context.GetBuilder();

  auto valid_or_err = LowerOperand(context, operands[0]);
  if (!valid_or_err) return std::unexpected(valid_or_err.error());
  llvm::Value* valid = *valid_or_err;
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
  auto read_fs_or_err =
      LowerOperandFourState(context, place_operand, elem_type);
  if (!read_fs_or_err) return std::unexpected(read_fs_or_err.error());
  auto read_fs = *read_fs_or_err;
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

  return ComputeResult::FourState(phi_val, phi_unk);
}

auto LowerBinaryRvalue4State(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  llvm::Type* elem_type = packed_context.element_type;
  uint32_t semantic_width = packed_context.bit_width;

  auto& builder = context.GetBuilder();
  auto* zero = llvm::ConstantInt::get(elem_type, 0);

  auto lhs_or_err = LowerOperandFourState(context, operands[0], elem_type);
  if (!lhs_or_err) return std::unexpected(lhs_or_err.error());
  auto lhs = *lhs_or_err;

  auto rhs_or_err = LowerOperandFourState(context, operands[1], elem_type);
  if (!rhs_or_err) return std::unexpected(rhs_or_err.error());
  auto rhs = *rhs_or_err;

  // Handle case equality (===, !==) specially - always returns 0 or 1, never X
  // Compares both value AND unknown bits exactly
  if (IsCaseEqualityOp(info.op)) {
    uint32_t lhs_width = GetOperandPackedWidth(context, operands[0]);
    uint32_t rhs_width = GetOperandPackedWidth(context, operands[1]);
    uint32_t cmp_width = std::max(lhs_width, rhs_width);
    auto* cmp_type = llvm::Type::getIntNTy(context.GetLlvmContext(), cmp_width);

    // Coerce operands to comparison width
    auto* cmp_lhs_val =
        builder.CreateZExtOrTrunc(lhs.value, cmp_type, "ceq.lhs.val");
    auto* cmp_rhs_val =
        builder.CreateZExtOrTrunc(rhs.value, cmp_type, "ceq.rhs.val");
    auto* cmp_lhs_unk =
        builder.CreateZExtOrTrunc(lhs.unknown, cmp_type, "ceq.lhs.unk");
    auto* cmp_rhs_unk =
        builder.CreateZExtOrTrunc(rhs.unknown, cmp_type, "ceq.rhs.unk");

    // Both value AND unknown bits must match exactly
    auto* val_eq = builder.CreateICmpEQ(cmp_lhs_val, cmp_rhs_val, "ceq.val_eq");
    auto* unk_eq = builder.CreateICmpEQ(cmp_lhs_unk, cmp_rhs_unk, "ceq.unk_eq");
    llvm::Value* result = builder.CreateAnd(val_eq, unk_eq, "ceq.match");

    // For !==, invert the result
    if (info.op == mir::BinaryOp::kCaseNotEqual) {
      result = builder.CreateNot(result, "ceq.ne");
    }

    // Case equality always returns 2-state (0 or 1), never X
    auto* val = builder.CreateZExt(result, elem_type, "ceq.val");
    return ComputeResult::FourState(val, zero);
  }

  // Handle casez/casex matching (kCaseZMatch, kCaseXMatch)
  // These return 2-state result (always 0 or 1, never X)
  if (IsCaseMatchOp(info.op)) {
    uint32_t operand_width = GetOperandPackedWidth(context, operands[0]);
    auto* match_type =
        llvm::Type::getIntNTy(context.GetLlvmContext(), operand_width);

    auto lhs_4s = LowerOperandFourState(context, operands[0], match_type);
    if (!lhs_4s) return std::unexpected(lhs_4s.error());
    auto rhs_4s = LowerOperandFourState(context, operands[1], match_type);
    if (!rhs_4s) return std::unexpected(rhs_4s.error());

    lhs_4s->value =
        builder.CreateZExtOrTrunc(lhs_4s->value, match_type, "cm.lhs.val");
    lhs_4s->unknown =
        builder.CreateZExtOrTrunc(lhs_4s->unknown, match_type, "cm.lhs.unk");
    rhs_4s->value =
        builder.CreateZExtOrTrunc(rhs_4s->value, match_type, "cm.rhs.val");
    rhs_4s->unknown =
        builder.CreateZExtOrTrunc(rhs_4s->unknown, match_type, "cm.rhs.unk");

    llvm::Value* wildcard = nullptr;
    if (info.op == mir::BinaryOp::kCaseXMatch) {
      wildcard = builder.CreateOr(lhs_4s->unknown, rhs_4s->unknown, "cx.wc");
    } else {
      auto* lhs_z =
          builder.CreateAnd(lhs_4s->value, lhs_4s->unknown, "cz.lhs.z");
      auto* rhs_z =
          builder.CreateAnd(rhs_4s->value, rhs_4s->unknown, "cz.rhs.z");
      wildcard = builder.CreateOr(lhs_z, rhs_z, "cz.wc");
    }

    auto* not_wildcard = builder.CreateNot(wildcard, "cm.not.wc");
    auto sem_mask = llvm::APInt::getLowBitsSet(
        match_type->getIntegerBitWidth(), operand_width);
    auto* mask = builder.CreateAnd(
        not_wildcard, llvm::ConstantInt::get(match_type, sem_mask), "cm.mask");
    auto* lhs_masked = builder.CreateAnd(lhs_4s->value, mask, "cm.lv");
    auto* rhs_masked = builder.CreateAnd(rhs_4s->value, mask, "cm.rv");
    llvm::Value* result =
        builder.CreateICmpEQ(lhs_masked, rhs_masked, "cm.veq");

    if (info.op == mir::BinaryOp::kCaseZMatch) {
      auto* lhs_unk_m = builder.CreateAnd(lhs_4s->unknown, mask, "cz.lu");
      auto* rhs_unk_m = builder.CreateAnd(rhs_4s->unknown, mask, "cz.ru");
      auto* unk_eq = builder.CreateICmpEQ(lhs_unk_m, rhs_unk_m, "cz.ueq");
      result = builder.CreateAnd(result, unk_eq, "cz.match");
    }

    // Case match always returns 2-state (0 or 1), never X
    auto* val = builder.CreateZExt(result, elem_type, "cm.val");
    return ComputeResult::FourState(val, zero);
  }

  // Handle wildcard comparison (==?, !=?) specially - different taint rules
  if (IsWildcardComparisonOp(info.op)) {
    uint32_t lhs_width = GetOperandPackedWidth(context, operands[0]);
    uint32_t rhs_width = GetOperandPackedWidth(context, operands[1]);
    uint32_t cmp_width = std::max(lhs_width, rhs_width);
    auto* cmp_type = llvm::Type::getIntNTy(context.GetLlvmContext(), cmp_width);

    // Coerce operands to comparison width
    auto* cmp_lhs_val =
        builder.CreateZExtOrTrunc(lhs.value, cmp_type, "wc.lhs.val");
    auto* cmp_rhs_val =
        builder.CreateZExtOrTrunc(rhs.value, cmp_type, "wc.rhs.val");
    auto* cmp_lhs_unk =
        builder.CreateZExtOrTrunc(lhs.unknown, cmp_type, "wc.lhs.unk");
    auto* cmp_rhs_unk =
        builder.CreateZExtOrTrunc(rhs.unknown, cmp_type, "wc.rhs.unk");

    // RHS unknowns are wildcards - create mask for bits to compare
    auto* compare_mask = builder.CreateNot(cmp_rhs_unk, "wc.compare_mask");

    // Mask both values and compare
    auto* masked_lhs =
        builder.CreateAnd(cmp_lhs_val, compare_mask, "wc.masked_lhs");
    auto* masked_rhs =
        builder.CreateAnd(cmp_rhs_val, compare_mask, "wc.masked_rhs");
    auto* values_eq =
        builder.CreateICmpEQ(masked_lhs, masked_rhs, "wc.values_eq");

    // Taint: LHS has X/Z where RHS is definite (not wildcard)
    auto* taint_bits =
        builder.CreateAnd(cmp_lhs_unk, compare_mask, "wc.taint_bits");
    auto* taint = builder.CreateICmpNE(
        taint_bits, llvm::ConstantInt::get(cmp_type, 0), "wc.taint");

    // For !=?, invert the equality result
    llvm::Value* result = values_eq;
    if (info.op == mir::BinaryOp::kWildcardNotEqual) {
      result = builder.CreateNot(values_eq, "wc.ne");
    }

    auto* val = builder.CreateZExt(result, elem_type, "wc.cmp.val");
    auto* unk = builder.CreateZExt(taint, elem_type, "wc.cmp.unk");
    return ComputeResult::FourState(val, unk);
  }

  if (IsComparisonOp(info.op)) {
    uint32_t lhs_width = GetOperandPackedWidth(context, operands[0]);
    uint32_t rhs_width = GetOperandPackedWidth(context, operands[1]);

    // Use shared helper for value comparison
    auto cmp_or_err = LowerCompareToI1(
        context, info.op, lhs.value, rhs.value, lhs_width, rhs_width);
    if (!cmp_or_err) return std::unexpected(cmp_or_err.error());

    // Unknown bit handling: any X/Z in either operand taints the result
    uint32_t cmp_width = std::max(lhs_width, rhs_width);
    auto* cmp_type = llvm::Type::getIntNTy(context.GetLlvmContext(), cmp_width);
    auto* cmp_lhs_unk =
        builder.CreateZExtOrTrunc(lhs.unknown, cmp_type, "bin4.lhs.unk");
    auto* cmp_rhs_unk =
        builder.CreateZExtOrTrunc(rhs.unknown, cmp_type, "bin4.rhs.unk");
    auto* combined_unk = builder.CreateOr(cmp_lhs_unk, cmp_rhs_unk, "bin4.unk");
    auto* taint = builder.CreateICmpNE(
        combined_unk, llvm::ConstantInt::get(cmp_type, 0), "bin4.taint");

    // When tainted, result is X (val=0, unk=1), not (cmp_result, 1) which
    // could be Z if cmp_result=1
    auto* cmp_ext = builder.CreateZExt(*cmp_or_err, elem_type, "bin4.cmp");
    auto* val = builder.CreateSelect(taint, zero, cmp_ext, "bin4.cmp.val");
    auto* unk = builder.CreateZExt(taint, elem_type, "bin4.cmp.unk");
    return ComputeResult::FourState(val, unk);
  }

  lhs.value = builder.CreateZExtOrTrunc(lhs.value, elem_type, "bin4.lhs.val");
  rhs.value = builder.CreateZExtOrTrunc(rhs.value, elem_type, "bin4.rhs.val");
  lhs.unknown =
      builder.CreateZExtOrTrunc(lhs.unknown, elem_type, "bin4.lhs.unk");
  rhs.unknown =
      builder.CreateZExtOrTrunc(rhs.unknown, elem_type, "bin4.rhs.unk");

  // Bitwise operations with correct 4-state propagation (IEEE 1800 truth
  // tables)
  switch (info.op) {
    case mir::BinaryOp::kBitwiseAnd: {
      auto result = FourStateAnd(builder, lhs, rhs);
      return ComputeResult::FourState(result.value, result.unknown);
    }
    case mir::BinaryOp::kBitwiseOr: {
      auto result = FourStateOr(builder, lhs, rhs);
      return ComputeResult::FourState(result.value, result.unknown);
    }
    case mir::BinaryOp::kBitwiseXor: {
      auto result = FourStateXor(builder, lhs, rhs);
      return ComputeResult::FourState(result.value, result.unknown);
    }
    default:
      break;  // Fall through to existing paths
  }

  auto* combined_unk = builder.CreateOr(lhs.unknown, rhs.unknown, "bin4.unk");

  if (IsLogicalOp(info.op)) {
    auto val_or_err = LowerBinaryArith(context, info.op, lhs.value, rhs.value);
    if (!val_or_err) return std::unexpected(val_or_err.error());
    auto* taint = builder.CreateICmpNE(combined_unk, zero, "bin4.taint");
    auto* val = builder.CreateZExt(*val_or_err, elem_type, "bin4.log.val");
    auto* unk = builder.CreateZExt(taint, elem_type, "bin4.log.unk");
    return ComputeResult::FourState(val, unk);
  }

  if (IsShiftOp(info.op)) {
    auto* val =
        LowerShiftOp(context, info.op, lhs.value, rhs.value, semantic_width);
    auto* unk = LowerShiftOpUnknown(
        context, info.op, lhs.unknown, rhs.value, semantic_width);
    return ComputeResult::FourState(val, unk);
  }

  if (ReturnsI1(info.op)) {
    throw common::InternalError(
        "LowerBinaryRvalue4State",
        "i1-producing op must be handled as comparison or logical");
  }
  auto val_or_err = LowerBinaryArith(context, info.op, lhs.value, rhs.value);
  if (!val_or_err) return std::unexpected(val_or_err.error());
  return ComputeResult::FourState(*val_or_err, combined_unk);
}

auto LowerRuntimeQuery4State(
    Context& context, const mir::RuntimeQueryRvalueInfo& info,
    const PackedComputeContext& packed_context) -> Result<ComputeResult> {
  llvm::Type* elem_type = packed_context.element_type;

  auto& builder = context.GetBuilder();

  switch (info.kind) {
    case RuntimeQueryKind::kTimeRawTicks: {
      auto* raw = builder.CreateCall(
          context.GetLyraGetTime(), {context.GetEnginePointer()});
      llvm::Value* val = raw;
      if (raw->getType() != elem_type) {
        val = builder.CreateZExtOrTrunc(raw, elem_type, "time.fit");
      }
      auto* zero = llvm::ConstantInt::get(elem_type, 0);
      return ComputeResult::FourState(val, zero);
    }
  }
  llvm_unreachable("unhandled RuntimeQueryKind");
}

auto LowerCaseMatchOp(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();

  uint32_t operand_width = GetOperandPackedWidth(context, operands[0]);
  auto* elem_type =
      llvm::Type::getIntNTy(context.GetLlvmContext(), operand_width);

  auto lhs_or_err = LowerOperandFourState(context, operands[0], elem_type);
  if (!lhs_or_err) return std::unexpected(lhs_or_err.error());
  auto lhs = *lhs_or_err;

  auto rhs_or_err = LowerOperandFourState(context, operands[1], elem_type);
  if (!rhs_or_err) return std::unexpected(rhs_or_err.error());
  auto rhs = *rhs_or_err;

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

auto LowerCaseEqualityOp(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* storage_type)
    -> Result<llvm::Value*> {
  // === (case equality): Exact 4-state comparison.
  // Compares both value AND unknown bits exactly, always returning 0 or 1
  // (never X).
  auto& builder = context.GetBuilder();

  uint32_t lhs_width = GetOperandPackedWidth(context, operands[0]);
  uint32_t rhs_width = GetOperandPackedWidth(context, operands[1]);
  uint32_t cmp_width = std::max(lhs_width, rhs_width);
  auto* elem_type = llvm::Type::getIntNTy(context.GetLlvmContext(), cmp_width);

  auto lhs_or_err = LowerOperandFourState(context, operands[0], elem_type);
  if (!lhs_or_err) return std::unexpected(lhs_or_err.error());
  auto lhs = *lhs_or_err;

  auto rhs_or_err = LowerOperandFourState(context, operands[1], elem_type);
  if (!rhs_or_err) return std::unexpected(rhs_or_err.error());
  auto rhs = *rhs_or_err;

  // Coerce to comparison width
  lhs.value = builder.CreateZExtOrTrunc(lhs.value, elem_type, "ceq.lhs.val");
  lhs.unknown =
      builder.CreateZExtOrTrunc(lhs.unknown, elem_type, "ceq.lhs.unk");
  rhs.value = builder.CreateZExtOrTrunc(rhs.value, elem_type, "ceq.rhs.val");
  rhs.unknown =
      builder.CreateZExtOrTrunc(rhs.unknown, elem_type, "ceq.rhs.unk");

  // Both value AND unknown bits must match exactly
  auto* val_eq = builder.CreateICmpEQ(lhs.value, rhs.value, "ceq.val_eq");
  auto* unk_eq = builder.CreateICmpEQ(lhs.unknown, rhs.unknown, "ceq.unk_eq");
  llvm::Value* result = builder.CreateAnd(val_eq, unk_eq, "ceq.match");

  // For !==, invert the result
  if (info.op == mir::BinaryOp::kCaseNotEqual) {
    result = builder.CreateNot(result, "ceq.ne");
  }

  return builder.CreateZExt(result, storage_type, "ceq.ext");
}

}  // namespace lyra::lowering::mir_to_llvm
