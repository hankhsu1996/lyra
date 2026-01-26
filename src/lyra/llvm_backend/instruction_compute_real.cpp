#include "lyra/llvm_backend/instruction_compute_real.hpp"

#include <expected>
#include <format>
#include <variant>
#include <vector>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/ErrorHandling.h"
#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto IsRealKind(TypeKind kind) -> bool {
  return kind == TypeKind::kReal || kind == TypeKind::kShortReal;
}

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

auto GetOperandFloatType(Context& context, const mir::Operand& operand)
    -> llvm::Type* {
  TypeId tid = GetOperandTypeId(context, operand);
  const auto& types = context.GetTypeArena();
  if (!IsRealKind(types[tid].Kind())) {
    throw common::InternalError(
        "GetOperandFloatType", "operand must be real or shortreal");
  }
  if (types[tid].Kind() == TypeKind::kShortReal) {
    return llvm::Type::getFloatTy(context.GetLlvmContext());
  }
  return llvm::Type::getDoubleTy(context.GetLlvmContext());
}

auto IsRealComparisonOp(mir::BinaryOp op) -> bool {
  switch (op) {
    case mir::BinaryOp::kEqual:
    case mir::BinaryOp::kNotEqual:
    case mir::BinaryOp::kLessThan:
    case mir::BinaryOp::kLessThanEqual:
    case mir::BinaryOp::kGreaterThan:
    case mir::BinaryOp::kGreaterThanEqual:
      return true;
    default:
      return false;
  }
}

auto MapToFcmpPredicate(mir::BinaryOp op) -> llvm::CmpInst::Predicate {
  switch (op) {
    case mir::BinaryOp::kEqual:
      return llvm::CmpInst::FCMP_OEQ;
    case mir::BinaryOp::kNotEqual:
      return llvm::CmpInst::FCMP_UNE;
    case mir::BinaryOp::kLessThan:
      return llvm::CmpInst::FCMP_OLT;
    case mir::BinaryOp::kLessThanEqual:
      return llvm::CmpInst::FCMP_OLE;
    case mir::BinaryOp::kGreaterThan:
      return llvm::CmpInst::FCMP_OGT;
    case mir::BinaryOp::kGreaterThanEqual:
      return llvm::CmpInst::FCMP_OGE;
    default:
      llvm_unreachable("not a real comparison op");
  }
}

auto LowerRealUnary(
    Context& context, const mir::UnaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, mir::PlaceId target)
    -> Result<void> {
  auto& builder = context.GetBuilder();

  llvm::Type* float_ty = GetOperandFloatType(context, operands[0]);
  auto operand_or_err = LowerOperand(context, operands[0]);
  if (!operand_or_err) return std::unexpected(operand_or_err.error());
  llvm::Value* operand = *operand_or_err;

  llvm::Value* result = nullptr;

  if (info.op == mir::UnaryOp::kPlus) {
    result = operand;
  } else if (info.op == mir::UnaryOp::kMinus) {
    result = builder.CreateFNeg(operand, "fneg");
  } else if (info.op == mir::UnaryOp::kLogicalNot) {
    auto* zero = llvm::ConstantFP::get(float_ty, 0.0);
    auto* nonzero = builder.CreateFCmpUNE(operand, zero, "nonzero");
    auto* not_val = builder.CreateNot(nonzero, "lnot");
    auto target_type_or_err = context.GetPlaceLlvmType(target);
    if (!target_type_or_err) return std::unexpected(target_type_or_err.error());
    llvm::Type* target_type = *target_type_or_err;
    result = builder.CreateZExtOrTrunc(not_val, target_type, "lnot.ext");
  } else {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        std::format("unsupported real unary op: {}", mir::ToString(info.op)),
        UnsupportedCategory::kOperation));
  }

  auto target_ptr_or_err = context.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;
  builder.CreateStore(result, target_ptr);
  return {};
}

auto LowerRealBinary(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, mir::PlaceId target)
    -> Result<void> {
  auto& builder = context.GetBuilder();

  llvm::Type* float_ty = GetOperandFloatType(context, operands[0]);
  auto lhs_or_err = LowerOperand(context, operands[0]);
  if (!lhs_or_err) return std::unexpected(lhs_or_err.error());
  llvm::Value* lhs = *lhs_or_err;
  auto rhs_or_err = LowerOperand(context, operands[1]);
  if (!rhs_or_err) return std::unexpected(rhs_or_err.error());
  llvm::Value* rhs = *rhs_or_err;

  if (!lhs->getType()->isFloatingPointTy() ||
      !rhs->getType()->isFloatingPointTy()) {
    throw common::InternalError(
        "LowerRealBinary", "real binary operands must be floating-point");
  }

  // Defensive: if mixed types (float vs double), promote to double
  if (lhs->getType() != rhs->getType()) {
    auto* double_ty = llvm::Type::getDoubleTy(context.GetLlvmContext());
    if (!lhs->getType()->isDoubleTy()) {
      lhs = builder.CreateFPExt(lhs, double_ty, "fpext.lhs");
    }
    if (!rhs->getType()->isDoubleTy()) {
      rhs = builder.CreateFPExt(rhs, double_ty, "fpext.rhs");
    }
    float_ty = double_ty;
  }

  llvm::Value* result = nullptr;
  auto target_ptr_or_err = context.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  if (IsRealComparisonOp(info.op)) {
    auto pred = MapToFcmpPredicate(info.op);
    auto* cmp = builder.CreateFCmp(pred, lhs, rhs, "fcmp");
    auto target_type_or_err = context.GetPlaceLlvmType(target);
    if (!target_type_or_err) return std::unexpected(target_type_or_err.error());
    llvm::Type* target_type = *target_type_or_err;
    result = builder.CreateZExtOrTrunc(cmp, target_type, "fcmp.ext");
    builder.CreateStore(result, target_ptr);
    return {};
  }

  if (info.op == mir::BinaryOp::kLogicalAnd ||
      info.op == mir::BinaryOp::kLogicalOr) {
    auto* zero = llvm::ConstantFP::get(float_ty, 0.0);
    auto* lhs_true = builder.CreateFCmpUNE(lhs, zero, "lhs.true");
    // NOLINTNEXTLINE(readability-suspicious-call-argument)
    auto* rhs_true = builder.CreateFCmpUNE(rhs, zero, "rhs.true");
    llvm::Value* logic_result = nullptr;
    if (info.op == mir::BinaryOp::kLogicalAnd) {
      logic_result = builder.CreateAnd(lhs_true, rhs_true, "fland");
    } else {
      logic_result = builder.CreateOr(lhs_true, rhs_true, "flor");
    }
    auto target_type_or_err = context.GetPlaceLlvmType(target);
    if (!target_type_or_err) return std::unexpected(target_type_or_err.error());
    llvm::Type* target_type = *target_type_or_err;
    result = builder.CreateZExtOrTrunc(logic_result, target_type, "flog.ext");
    builder.CreateStore(result, target_ptr);
    return {};
  }

  // Arithmetic operations
  switch (info.op) {
    case mir::BinaryOp::kAdd:
      result = builder.CreateFAdd(lhs, rhs, "fadd");
      break;
    case mir::BinaryOp::kSubtract:
      result = builder.CreateFSub(lhs, rhs, "fsub");
      break;
    case mir::BinaryOp::kMultiply:
      result = builder.CreateFMul(lhs, rhs, "fmul");
      break;
    case mir::BinaryOp::kDivide:
    case mir::BinaryOp::kDivideSigned:
      result = builder.CreateFDiv(lhs, rhs, "fdiv");
      break;
    case mir::BinaryOp::kPower: {
      auto& module = context.GetModule();
      auto* decl = llvm::Intrinsic::getDeclaration(
          &module, llvm::Intrinsic::pow, {float_ty});
      result = builder.CreateCall(decl, {lhs, rhs}, "pow");
      break;
    }
    default:
      return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
          context.GetCurrentOrigin(),
          std::format("unsupported real binary op: {}", mir::ToString(info.op)),
          UnsupportedCategory::kOperation));
  }

  builder.CreateStore(result, target_ptr);
  return {};
}

}  // namespace

auto IsRealMathCompute(Context& context, const mir::Compute& compute) -> bool {
  const auto& types = context.GetTypeArena();

  return std::visit(
      common::Overloaded{
          [&](const mir::UnaryRvalueInfo&) {
            TypeId tid = GetOperandTypeId(context, compute.value.operands[0]);
            return IsRealKind(types[tid].Kind());
          },
          [&](const mir::BinaryRvalueInfo&) {
            TypeId tid = GetOperandTypeId(context, compute.value.operands[0]);
            return IsRealKind(types[tid].Kind());
          },
          // Casts are handled by LowerCastUnified, not here
          [](const auto&) { return false; },
      },
      compute.value.info);
}

auto LowerRealCompute(Context& context, const mir::Compute& compute)
    -> Result<void> {
  return std::visit(
      common::Overloaded{
          [&](const mir::UnaryRvalueInfo& info) -> Result<void> {
            return LowerRealUnary(
                context, info, compute.value.operands, compute.target);
          },
          [&](const mir::BinaryRvalueInfo& info) -> Result<void> {
            return LowerRealBinary(
                context, info, compute.value.operands, compute.target);
          },
          [&](const mir::CastRvalueInfo&) -> Result<void> {
            throw common::InternalError(
                "LowerRealCompute", "casts use LowerCastUnified");
          },
          [&](const auto&) -> Result<void> {
            return std::unexpected(
                context.GetDiagnosticContext().MakeUnsupported(
                    context.GetCurrentOrigin(),
                    std::format(
                        "unsupported real rvalue kind: {}",
                        mir::GetRvalueKind(compute.value.info)),
                    UnsupportedCategory::kFeature));
          },
      },
      compute.value.info);
}

}  // namespace lyra::lowering::mir_to_llvm
