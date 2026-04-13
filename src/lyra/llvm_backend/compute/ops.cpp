#include "lyra/llvm_backend/compute/ops.hpp"

#include <algorithm>
#include <cstdint>
#include <format>

#include <llvm/ADT/APInt.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/ErrorHandling.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"

namespace lyra::lowering::mir_to_llvm {

auto ReturnsI1(mir::BinaryOp op) -> bool {
  switch (op) {
    case mir::BinaryOp::kEqual:
    case mir::BinaryOp::kNotEqual:
    case mir::BinaryOp::kLessThan:
    case mir::BinaryOp::kLessThanEqual:
    case mir::BinaryOp::kGreaterThan:
    case mir::BinaryOp::kGreaterThanEqual:
    case mir::BinaryOp::kLessThanSigned:
    case mir::BinaryOp::kLessThanEqualSigned:
    case mir::BinaryOp::kGreaterThanSigned:
    case mir::BinaryOp::kGreaterThanEqualSigned:
    case mir::BinaryOp::kLogicalAnd:
    case mir::BinaryOp::kLogicalOr:
    case mir::BinaryOp::kLogicalImplication:
    case mir::BinaryOp::kLogicalEquivalence:
    case mir::BinaryOp::kCaseZMatch:
    case mir::BinaryOp::kCaseXMatch:
      return true;
    default:
      return false;
  }
}

auto IsComparisonOp(mir::BinaryOp op) -> bool {
  switch (op) {
    case mir::BinaryOp::kEqual:
    case mir::BinaryOp::kNotEqual:
    case mir::BinaryOp::kCaseEqual:
    case mir::BinaryOp::kCaseNotEqual:
    case mir::BinaryOp::kWildcardEqual:
    case mir::BinaryOp::kWildcardNotEqual:
    case mir::BinaryOp::kLessThan:
    case mir::BinaryOp::kLessThanEqual:
    case mir::BinaryOp::kGreaterThan:
    case mir::BinaryOp::kGreaterThanEqual:
    case mir::BinaryOp::kLessThanSigned:
    case mir::BinaryOp::kLessThanEqualSigned:
    case mir::BinaryOp::kGreaterThanSigned:
    case mir::BinaryOp::kGreaterThanEqualSigned:
      return true;
    default:
      return false;
  }
}

auto IsWildcardComparisonOp(mir::BinaryOp op) -> bool {
  switch (op) {
    case mir::BinaryOp::kWildcardEqual:
    case mir::BinaryOp::kWildcardNotEqual:
      return true;
    default:
      return false;
  }
}

auto IsSignedComparisonOp(mir::BinaryOp op) -> bool {
  switch (op) {
    case mir::BinaryOp::kLessThanSigned:
    case mir::BinaryOp::kLessThanEqualSigned:
    case mir::BinaryOp::kGreaterThanSigned:
    case mir::BinaryOp::kGreaterThanEqualSigned:
      return true;
    default:
      return false;
  }
}

auto IsShiftOp(mir::BinaryOp op) -> bool {
  switch (op) {
    case mir::BinaryOp::kLogicalShiftLeft:
    case mir::BinaryOp::kArithmeticShiftLeft:
    case mir::BinaryOp::kLogicalShiftRight:
    case mir::BinaryOp::kArithmeticShiftRight:
      return true;
    default:
      return false;
  }
}

auto IsLogicalOp(mir::BinaryOp op) -> bool {
  return op == mir::BinaryOp::kLogicalAnd || op == mir::BinaryOp::kLogicalOr ||
         op == mir::BinaryOp::kLogicalImplication ||
         op == mir::BinaryOp::kLogicalEquivalence;
}

auto IsCaseMatchOp(mir::BinaryOp op) -> bool {
  return op == mir::BinaryOp::kCaseZMatch || op == mir::BinaryOp::kCaseXMatch;
}

auto IsCaseEqualityOp(mir::BinaryOp op) -> bool {
  return op == mir::BinaryOp::kCaseEqual || op == mir::BinaryOp::kCaseNotEqual;
}

auto IsReductionOp(mir::UnaryOp op) -> bool {
  switch (op) {
    case mir::UnaryOp::kReductionAnd:
    case mir::UnaryOp::kReductionNand:
    case mir::UnaryOp::kReductionOr:
    case mir::UnaryOp::kReductionNor:
    case mir::UnaryOp::kReductionXor:
    case mir::UnaryOp::kReductionXnor:
      return true;
    default:
      return false;
  }
}

auto GetSemanticMask(llvm::Type* ty, uint32_t semantic_width) -> llvm::Value* {
  auto mask =
      llvm::APInt::getLowBitsSet(ty->getIntegerBitWidth(), semantic_width);
  return llvm::ConstantInt::get(ty, mask);
}

auto ApplyWidthMask(
    llvm::IRBuilder<>& builder, llvm::Value* value, uint32_t semantic_width)
    -> llvm::Value* {
  uint32_t storage_width = value->getType()->getIntegerBitWidth();

  if (semantic_width == 0) {
    throw common::InternalError("ApplyWidthMask", "semantic width cannot be 0");
  }
  if (semantic_width > storage_width) {
    throw common::InternalError(
        "ApplyWidthMask", std::format(
                              "semantic width ({}) exceeds storage width ({})",
                              semantic_width, storage_width));
  }

  if (semantic_width == storage_width) {
    return value;
  }

  return builder.CreateAnd(
      value, GetSemanticMask(value->getType(), semantic_width), "mask");
}

auto SignExtendToStorage(
    llvm::IRBuilderBase& builder, llvm::Value* val, uint32_t semantic_width)
    -> llvm::Value* {
  auto* storage_type = val->getType();
  uint32_t storage_width = storage_type->getIntegerBitWidth();
  if (semantic_width >= storage_width) {
    return val;
  }
  auto* sem_ty = llvm::Type::getIntNTy(builder.getContext(), semantic_width);
  auto* truncated = builder.CreateTrunc(val, sem_ty, "sext.trunc");
  return builder.CreateSExt(truncated, storage_type, "sext.ext");
}

auto GetOperandPackedWidth(
    const CuFacts& facts, Context& context, const mir::Operand& operand)
    -> uint32_t {
  const auto& types = *facts.types;
  TypeId type_id = GetOperandTypeId(facts, context, operand);
  return PackedBitWidth(types[type_id], types);
}

auto LowerBinaryArith(
    llvm::IRBuilder<>& builder, mir::BinaryOp op, llvm::Value* lhs,
    llvm::Value* rhs) -> llvm::Value* {
  switch (op) {
    case mir::BinaryOp::kAdd:
      return builder.CreateAdd(lhs, rhs, "add");
    case mir::BinaryOp::kSubtract:
      return builder.CreateSub(lhs, rhs, "sub");
    case mir::BinaryOp::kMultiply:
      return builder.CreateMul(lhs, rhs, "mul");
    case mir::BinaryOp::kDivide:
      return builder.CreateUDiv(lhs, rhs, "udiv");
    case mir::BinaryOp::kDivideSigned:
      return builder.CreateSDiv(lhs, rhs, "sdiv");
    case mir::BinaryOp::kMod:
      return builder.CreateURem(lhs, rhs, "urem");
    case mir::BinaryOp::kModSigned:
      return builder.CreateSRem(lhs, rhs, "srem");
    case mir::BinaryOp::kBitwiseAnd:
      return builder.CreateAnd(lhs, rhs, "and");
    case mir::BinaryOp::kBitwiseOr:
      return builder.CreateOr(lhs, rhs, "or");
    case mir::BinaryOp::kBitwiseXor:
      return builder.CreateXor(lhs, rhs, "xor");
    case mir::BinaryOp::kBitwiseXnor: {
      auto* xor_result = builder.CreateXor(lhs, rhs, "xor");
      return builder.CreateNot(xor_result, "xnor");
    }

    case mir::BinaryOp::kLogicalAnd: {
      auto* const_zero = llvm::ConstantInt::get(lhs->getType(), 0);
      auto* lhs_bool = builder.CreateICmpNE(lhs, const_zero, "lhs.bool");
      // NOLINTNEXTLINE(readability-suspicious-call-argument)
      auto* rhs_bool = builder.CreateICmpNE(rhs, const_zero, "rhs.bool");
      return builder.CreateAnd(lhs_bool, rhs_bool, "land");
    }
    case mir::BinaryOp::kLogicalOr: {
      auto* const_zero = llvm::ConstantInt::get(lhs->getType(), 0);
      auto* lhs_bool = builder.CreateICmpNE(lhs, const_zero, "lhs.bool");
      // NOLINTNEXTLINE(readability-suspicious-call-argument)
      auto* rhs_bool = builder.CreateICmpNE(rhs, const_zero, "rhs.bool");
      return builder.CreateOr(lhs_bool, rhs_bool, "lor");
    }
    case mir::BinaryOp::kLogicalImplication: {
      // a -> b  ===  !a || b
      auto* const_zero = llvm::ConstantInt::get(lhs->getType(), 0);
      auto* lhs_bool = builder.CreateICmpNE(lhs, const_zero, "lhs.bool");
      // NOLINTNEXTLINE(readability-suspicious-call-argument)
      auto* rhs_bool = builder.CreateICmpNE(rhs, const_zero, "rhs.bool");
      auto* not_lhs = builder.CreateNot(lhs_bool, "impl.not");
      return builder.CreateOr(not_lhs, rhs_bool, "impl");
    }
    case mir::BinaryOp::kLogicalEquivalence: {
      // a <-> b  ===  (a && b) || (!a && !b)  ===  !(a ^ b) at boolean level
      auto* const_zero = llvm::ConstantInt::get(lhs->getType(), 0);
      auto* lhs_bool = builder.CreateICmpNE(lhs, const_zero, "lhs.bool");
      // NOLINTNEXTLINE(readability-suspicious-call-argument)
      auto* rhs_bool = builder.CreateICmpNE(rhs, const_zero, "rhs.bool");
      auto* xor_result = builder.CreateXor(lhs_bool, rhs_bool, "equiv.xor");
      return builder.CreateNot(xor_result, "equiv");
    }

    case mir::BinaryOp::kPower:
    case mir::BinaryOp::kPowerSigned: {
      // Integer exponentiation by squaring.
      // kPowerSigned: negative exponent returns 0 (LRM 11.4.3).
      // kPower: exponent is unsigned, no negative guard needed.
      bool is_signed = (op == mir::BinaryOp::kPowerSigned);

      auto* ty = lhs->getType();
      auto* zero = llvm::ConstantInt::get(ty, 0);
      auto* one = llvm::ConstantInt::get(ty, 1);

      auto* func = builder.GetInsertBlock()->getParent();
      auto* entry_bb = builder.GetInsertBlock();
      auto& llvm_ctx = builder.getContext();
      auto* loop_bb = llvm::BasicBlock::Create(llvm_ctx, "pow.loop", func);
      auto* body_bb = llvm::BasicBlock::Create(llvm_ctx, "pow.body", func);
      auto* merge_bb = llvm::BasicBlock::Create(llvm_ctx, "pow.merge", func);

      if (is_signed) {
        auto* is_neg = builder.CreateICmpSLT(rhs, zero, "pow.neg");
        builder.CreateCondBr(is_neg, merge_bb, loop_bb);
      } else {
        builder.CreateBr(loop_bb);
      }

      // Loop header: check if exponent is zero
      builder.SetInsertPoint(loop_bb);
      auto* acc = builder.CreatePHI(ty, 2, "pow.acc");
      auto* base = builder.CreatePHI(ty, 2, "pow.base");
      auto* exp = builder.CreatePHI(ty, 2, "pow.exp");
      acc->addIncoming(one, entry_bb);
      base->addIncoming(lhs, entry_bb);
      exp->addIncoming(rhs, entry_bb);
      auto* done = builder.CreateICmpEQ(exp, zero, "pow.done");
      builder.CreateCondBr(done, merge_bb, body_bb);

      // Loop body: square-and-multiply
      builder.SetInsertPoint(body_bb);
      auto* is_odd = builder.CreateTrunc(
          builder.CreateAnd(exp, one, "pow.odd.bit"), builder.getInt1Ty(),
          "pow.odd");
      auto* acc_mul = builder.CreateMul(acc, base, "pow.mul");
      auto* acc_next =
          builder.CreateSelect(is_odd, acc_mul, acc, "pow.acc.next");
      auto* base_next = builder.CreateMul(base, base, "pow.sq");
      auto* exp_next = builder.CreateLShr(exp, one, "pow.exp.next");
      acc->addIncoming(acc_next, body_bb);
      base->addIncoming(base_next, body_bb);
      exp->addIncoming(exp_next, body_bb);
      builder.CreateBr(loop_bb);

      // Merge: result
      builder.SetInsertPoint(merge_bb);
      if (is_signed) {
        auto* result = builder.CreatePHI(ty, 2, "pow.result");
        result->addIncoming(zero, entry_bb);
        result->addIncoming(acc, loop_bb);
        return result;
      }
      return static_cast<llvm::Value*>(acc);
    }

    default:
      throw common::InternalError(
          "LowerBinaryArith",
          std::format(
              "unhandled binary op in arithmetic lowering: {}",
              mir::ToString(op)));
  }
}

auto LowerBinaryComparison(
    llvm::IRBuilder<>& builder, mir::BinaryOp op, llvm::Value* lhs,
    llvm::Value* rhs) -> llvm::Value* {
  switch (op) {
    case mir::BinaryOp::kEqual:
    case mir::BinaryOp::kCaseEqual:
    case mir::BinaryOp::kWildcardEqual:
      // In 2-state path, all equality ops degenerate to == (no X/Z possible)
      return builder.CreateICmpEQ(lhs, rhs, "eq");
    case mir::BinaryOp::kNotEqual:
    case mir::BinaryOp::kCaseNotEqual:
    case mir::BinaryOp::kWildcardNotEqual:
      // In 2-state path, all inequality ops degenerate to != (no X/Z possible)
      return builder.CreateICmpNE(lhs, rhs, "ne");
    case mir::BinaryOp::kLessThan:
      return builder.CreateICmpULT(lhs, rhs, "ult");
    case mir::BinaryOp::kLessThanEqual:
      return builder.CreateICmpULE(lhs, rhs, "ule");
    case mir::BinaryOp::kGreaterThan:
      return builder.CreateICmpUGT(lhs, rhs, "ugt");
    case mir::BinaryOp::kGreaterThanEqual:
      return builder.CreateICmpUGE(lhs, rhs, "uge");
    case mir::BinaryOp::kLessThanSigned:
      return builder.CreateICmpSLT(lhs, rhs, "slt");
    case mir::BinaryOp::kLessThanEqualSigned:
      return builder.CreateICmpSLE(lhs, rhs, "sle");
    case mir::BinaryOp::kGreaterThanSigned:
      return builder.CreateICmpSGT(lhs, rhs, "sgt");
    case mir::BinaryOp::kGreaterThanEqualSigned:
      return builder.CreateICmpSGE(lhs, rhs, "sge");
    default:
      throw common::InternalError(
          "LowerBinaryComparison",
          std::format(
              "non-comparison op {} reached comparison lowering",
              mir::ToString(op)));
  }
}

auto LowerCompareToI1(
    llvm::IRBuilder<>& builder, mir::BinaryOp op, llvm::Value* lhs,
    llvm::Value* rhs, uint32_t lhs_semantic_width, uint32_t rhs_semantic_width)
    -> llvm::Value* {
  // Compare at the max of operand widths (not result width, which is always 1)
  uint32_t cmp_width = std::max(lhs_semantic_width, rhs_semantic_width);
  auto* cmp_type = llvm::Type::getIntNTy(builder.getContext(), cmp_width);

  // Coerce operands to comparison width
  llvm::Value* cmp_lhs = builder.CreateZExtOrTrunc(lhs, cmp_type, "cmp.lhs");
  llvm::Value* cmp_rhs = builder.CreateZExtOrTrunc(rhs, cmp_type, "cmp.rhs");

  // Verify coercion succeeded (catch contract violations early)
  if (cmp_lhs->getType() != cmp_type || cmp_rhs->getType() != cmp_type) {
    throw common::InternalError(
        "LowerCompareToI1", "operand coercion produced unexpected type");
  }

  // For signed comparisons, sign-extend each operand from its semantic width
  if (IsSignedComparisonOp(op)) {
    cmp_lhs = SignExtendToStorage(builder, cmp_lhs, lhs_semantic_width);
    cmp_rhs = SignExtendToStorage(builder, cmp_rhs, rhs_semantic_width);
  }

  return LowerBinaryComparison(builder, op, cmp_lhs, cmp_rhs);
}

auto LowerShiftOp(
    llvm::IRBuilder<>& builder, mir::BinaryOp op, llvm::Value* value,
    llvm::Value* shift_amount, uint32_t semantic_width) -> llvm::Value* {
  llvm::Type* ty = value->getType();

  auto* width_const = llvm::ConstantInt::get(ty, semantic_width);
  auto* zero = llvm::ConstantInt::get(ty, 0);

  auto* in_range =
      builder.CreateICmpULT(shift_amount, width_const, "shift.inrange");

  auto* safe_amount =
      builder.CreateSelect(in_range, shift_amount, zero, "shift.safe");

  llvm::Value* shifted = nullptr;
  llvm::Value* fallback = zero;

  switch (op) {
    case mir::BinaryOp::kLogicalShiftLeft:
    case mir::BinaryOp::kArithmeticShiftLeft:
      shifted = builder.CreateShl(value, safe_amount, "shl");
      break;
    case mir::BinaryOp::kLogicalShiftRight:
      shifted = builder.CreateLShr(value, safe_amount, "lshr");
      break;
    case mir::BinaryOp::kArithmeticShiftRight: {
      shifted = builder.CreateAShr(value, safe_amount, "ashr");
      auto* width_m1 = llvm::ConstantInt::get(ty, semantic_width - 1);
      auto* sign_bit = builder.CreateLShr(value, width_m1, "signbit");
      auto* is_neg = builder.CreateTrunc(sign_bit, builder.getInt1Ty());
      auto* semantic_ones = GetSemanticMask(ty, semantic_width);
      fallback = builder.CreateSelect(is_neg, semantic_ones, zero, "signfill");
      break;
    }
    default:
      llvm_unreachable("not a shift op");
  }

  return builder.CreateSelect(in_range, shifted, fallback, "shift.result");
}

auto LowerShiftOpUnknown(
    llvm::IRBuilder<>& builder, mir::BinaryOp op, llvm::Value* unk,
    llvm::Value* shift_amount, uint32_t semantic_width) -> llvm::Value* {
  mir::BinaryOp unk_op = mir::BinaryOp::kLogicalShiftRight;
  switch (op) {
    case mir::BinaryOp::kLogicalShiftLeft:
    case mir::BinaryOp::kArithmeticShiftLeft:
      unk_op = mir::BinaryOp::kLogicalShiftLeft;
      break;
    default:
      break;
  }
  return LowerShiftOp(builder, unk_op, unk, shift_amount, semantic_width);
}

auto LowerUnaryOp(
    llvm::IRBuilder<>& builder, mir::UnaryOp op, llvm::Value* operand,
    llvm::Type* storage_type, uint32_t operand_bit_width) -> llvm::Value* {
  switch (op) {
    case mir::UnaryOp::kPlus:
      return operand;
    case mir::UnaryOp::kMinus:
      return builder.CreateNeg(operand, "neg");
    case mir::UnaryOp::kBitwiseNot:
      return builder.CreateNot(operand, "not");
    case mir::UnaryOp::kLogicalNot: {
      auto* zero = llvm::ConstantInt::get(operand->getType(), 0);
      auto* is_nonzero = builder.CreateICmpNE(operand, zero, "nonzero");
      auto* negated = builder.CreateNot(is_nonzero, "lnot");
      return builder.CreateZExt(negated, storage_type, "lnot.ext");
    }
    case mir::UnaryOp::kReductionAnd: {
      auto* all_ones = GetSemanticMask(operand->getType(), operand_bit_width);
      auto* is_all_ones = builder.CreateICmpEQ(operand, all_ones, "red.and");
      return builder.CreateZExt(is_all_ones, storage_type, "red.and.ext");
    }
    case mir::UnaryOp::kReductionNand: {
      auto* all_ones = GetSemanticMask(operand->getType(), operand_bit_width);
      auto* is_not_all_ones =
          builder.CreateICmpNE(operand, all_ones, "red.nand");
      return builder.CreateZExt(is_not_all_ones, storage_type, "red.nand.ext");
    }
    case mir::UnaryOp::kReductionOr: {
      auto* zero = llvm::ConstantInt::get(operand->getType(), 0);
      auto* is_nonzero = builder.CreateICmpNE(operand, zero, "red.or");
      return builder.CreateZExt(is_nonzero, storage_type, "red.or.ext");
    }
    case mir::UnaryOp::kReductionNor: {
      auto* zero = llvm::ConstantInt::get(operand->getType(), 0);
      auto* is_zero = builder.CreateICmpEQ(operand, zero, "red.nor");
      return builder.CreateZExt(is_zero, storage_type, "red.nor.ext");
    }
    case mir::UnaryOp::kReductionXor:
    case mir::UnaryOp::kReductionXnor: {
      auto* module = builder.GetInsertBlock()->getModule();
      auto* ctpop = llvm::Intrinsic::getDeclaration(
          module, llvm::Intrinsic::ctpop, {operand->getType()});
      auto* count = builder.CreateCall(ctpop, {operand}, "popcount");
      auto* one = llvm::ConstantInt::get(count->getType(), 1);
      auto* parity_n = builder.CreateAnd(count, one, "parity");
      auto* parity_i1 =
          builder.CreateTrunc(parity_n, builder.getInt1Ty(), "parity.i1");

      llvm::Value* result = parity_i1;
      if (op == mir::UnaryOp::kReductionXnor) {
        result = builder.CreateXor(result, builder.getTrue(), "parity.not");
      }

      const char* name =
          (op == mir::UnaryOp::kReductionXnor) ? "red.xnor.ext" : "red.xor.ext";
      return builder.CreateZExt(result, storage_type, name);
    }
    default:
      throw common::InternalError(
          "LowerUnaryOp", std::format(
                              "non-rvalue unary op {} reached backend lowering",
                              mir::ToString(op)));
  }
}

}  // namespace lyra::lowering::mir_to_llvm
