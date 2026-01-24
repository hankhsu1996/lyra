#include "lyra/llvm_backend/instruction_compute_real.hpp"

#include <format>
#include <variant>
#include <vector>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "lyra/common/constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
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
      Overloaded{
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

auto LowerRealMathUnary(
    Context& context, mir::UnaryOp op, llvm::Value* operand,
    llvm::Type* float_ty) -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& module = context.GetModule();

  // Intrinsic-backed ops
  llvm::Intrinsic::ID intrinsic_id = llvm::Intrinsic::not_intrinsic;
  switch (op) {
    case mir::UnaryOp::kLn:
      intrinsic_id = llvm::Intrinsic::log;
      break;
    case mir::UnaryOp::kLog10:
      intrinsic_id = llvm::Intrinsic::log10;
      break;
    case mir::UnaryOp::kExp:
      intrinsic_id = llvm::Intrinsic::exp;
      break;
    case mir::UnaryOp::kSqrt:
      intrinsic_id = llvm::Intrinsic::sqrt;
      break;
    case mir::UnaryOp::kFloor:
      intrinsic_id = llvm::Intrinsic::floor;
      break;
    case mir::UnaryOp::kCeil:
      intrinsic_id = llvm::Intrinsic::ceil;
      break;
    case mir::UnaryOp::kSin:
      intrinsic_id = llvm::Intrinsic::sin;
      break;
    case mir::UnaryOp::kCos:
      intrinsic_id = llvm::Intrinsic::cos;
      break;
    default:
      break;
  }

  if (intrinsic_id != llvm::Intrinsic::not_intrinsic) {
    auto* decl =
        llvm::Intrinsic::getDeclaration(&module, intrinsic_id, {float_ty});
    return builder.CreateCall(decl, {operand}, "math");
  }

  // Libc-backed ops
  const char* name = nullptr;
  bool is_double = float_ty->isDoubleTy();
  switch (op) {
    case mir::UnaryOp::kTan:
      name = is_double ? "tan" : "tanf";
      break;
    case mir::UnaryOp::kAsin:
      name = is_double ? "asin" : "asinf";
      break;
    case mir::UnaryOp::kAcos:
      name = is_double ? "acos" : "acosf";
      break;
    case mir::UnaryOp::kAtan:
      name = is_double ? "atan" : "atanf";
      break;
    case mir::UnaryOp::kSinh:
      name = is_double ? "sinh" : "sinhf";
      break;
    case mir::UnaryOp::kCosh:
      name = is_double ? "cosh" : "coshf";
      break;
    case mir::UnaryOp::kTanh:
      name = is_double ? "tanh" : "tanhf";
      break;
    case mir::UnaryOp::kAsinh:
      name = is_double ? "asinh" : "asinhf";
      break;
    case mir::UnaryOp::kAcosh:
      name = is_double ? "acosh" : "acoshf";
      break;
    case mir::UnaryOp::kAtanh:
      name = is_double ? "atanh" : "atanhf";
      break;
    default:
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm,
          common::UnsupportedKind::kOperation, context.GetCurrentOrigin(),
          std::format("unsupported real math unary op: {}", mir::ToString(op)));
  }

  auto* func_ty = llvm::FunctionType::get(float_ty, {float_ty}, false);
  auto func = module.getOrInsertFunction(name, func_ty);
  if (auto* f = llvm::dyn_cast<llvm::Function>(func.getCallee())) {
    f->setDoesNotThrow();
  }
  return builder.CreateCall(func, {operand}, "math");
}

auto LowerRealMathBinary(
    Context& context, mir::BinaryOp op, llvm::Value* lhs, llvm::Value* rhs,
    llvm::Type* float_ty) -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& module = context.GetModule();

  if (op == mir::BinaryOp::kPower) {
    auto* decl = llvm::Intrinsic::getDeclaration(
        &module, llvm::Intrinsic::pow, {float_ty});
    return builder.CreateCall(decl, {lhs, rhs}, "pow");
  }

  const char* name = nullptr;
  bool is_double = float_ty->isDoubleTy();
  switch (op) {
    case mir::BinaryOp::kAtan2:
      name = is_double ? "atan2" : "atan2f";
      break;
    case mir::BinaryOp::kHypot:
      name = is_double ? "hypot" : "hypotf";
      break;
    default:
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm,
          common::UnsupportedKind::kOperation, context.GetCurrentOrigin(),
          std::format(
              "unsupported real math binary op: {}", mir::ToString(op)));
  }

  auto* func_ty =
      llvm::FunctionType::get(float_ty, {float_ty, float_ty}, false);
  auto func = module.getOrInsertFunction(name, func_ty);
  if (auto* f = llvm::dyn_cast<llvm::Function>(func.getCallee())) {
    f->setDoesNotThrow();
  }
  return builder.CreateCall(func, {lhs, rhs}, "math");
}

auto IsMathUnaryOp(mir::UnaryOp op) -> bool {
  switch (op) {
    case mir::UnaryOp::kLn:
    case mir::UnaryOp::kLog10:
    case mir::UnaryOp::kExp:
    case mir::UnaryOp::kSqrt:
    case mir::UnaryOp::kFloor:
    case mir::UnaryOp::kCeil:
    case mir::UnaryOp::kSin:
    case mir::UnaryOp::kCos:
    case mir::UnaryOp::kTan:
    case mir::UnaryOp::kAsin:
    case mir::UnaryOp::kAcos:
    case mir::UnaryOp::kAtan:
    case mir::UnaryOp::kSinh:
    case mir::UnaryOp::kCosh:
    case mir::UnaryOp::kTanh:
    case mir::UnaryOp::kAsinh:
    case mir::UnaryOp::kAcosh:
    case mir::UnaryOp::kAtanh:
      return true;
    default:
      return false;
  }
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

void LowerRealUnary(
    Context& context, const mir::UnaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, mir::PlaceId target) {
  auto& builder = context.GetBuilder();

  llvm::Type* float_ty = GetOperandFloatType(context, operands[0]);
  llvm::Value* operand = LowerOperand(context, operands[0]);

  llvm::Value* result = nullptr;

  if (info.op == mir::UnaryOp::kPlus) {
    result = operand;
  } else if (info.op == mir::UnaryOp::kMinus) {
    result = builder.CreateFNeg(operand, "fneg");
  } else if (info.op == mir::UnaryOp::kLogicalNot) {
    auto* zero = llvm::ConstantFP::get(float_ty, 0.0);
    auto* nonzero = builder.CreateFCmpUNE(operand, zero, "nonzero");
    auto* not_val = builder.CreateNot(nonzero, "lnot");
    llvm::Type* target_type = context.GetPlaceLlvmType(target);
    result = builder.CreateZExtOrTrunc(not_val, target_type, "lnot.ext");
  } else if (IsMathUnaryOp(info.op)) {
    result = LowerRealMathUnary(context, info.op, operand, float_ty);
  } else {
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm,
        common::UnsupportedKind::kOperation, context.GetCurrentOrigin(),
        std::format("unsupported real unary op: {}", mir::ToString(info.op)));
  }

  llvm::Value* target_ptr = context.GetPlacePointer(target);
  builder.CreateStore(result, target_ptr);
}

void LowerRealBinary(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, mir::PlaceId target) {
  auto& builder = context.GetBuilder();

  llvm::Type* float_ty = GetOperandFloatType(context, operands[0]);
  llvm::Value* lhs = LowerOperand(context, operands[0]);
  llvm::Value* rhs = LowerOperand(context, operands[1]);

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
  llvm::Value* target_ptr = context.GetPlacePointer(target);

  if (IsRealComparisonOp(info.op)) {
    auto pred = MapToFcmpPredicate(info.op);
    auto* cmp = builder.CreateFCmp(pred, lhs, rhs, "fcmp");
    llvm::Type* target_type = context.GetPlaceLlvmType(target);
    result = builder.CreateZExtOrTrunc(cmp, target_type, "fcmp.ext");
    builder.CreateStore(result, target_ptr);
    return;
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
    llvm::Type* target_type = context.GetPlaceLlvmType(target);
    result = builder.CreateZExtOrTrunc(logic_result, target_type, "flog.ext");
    builder.CreateStore(result, target_ptr);
    return;
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
    case mir::BinaryOp::kPower:
    case mir::BinaryOp::kAtan2:
    case mir::BinaryOp::kHypot:
      result = LowerRealMathBinary(context, info.op, lhs, rhs, float_ty);
      break;
    default:
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm,
          common::UnsupportedKind::kOperation, context.GetCurrentOrigin(),
          std::format(
              "unsupported real binary op: {}", mir::ToString(info.op)));
  }

  builder.CreateStore(result, target_ptr);
}

void LowerRealCast(
    Context& context, const mir::CastRvalueInfo& info,
    const std::vector<mir::Operand>& operands, mir::PlaceId target) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  llvm::Value* source = LowerOperand(context, operands[0]);
  const Type& src_type = types[info.source_type];
  const Type& tgt_type = types[info.target_type];

  llvm::Value* result = nullptr;
  llvm::Value* target_ptr = context.GetPlacePointer(target);

  // integral → real/shortreal
  if (IsPacked(src_type) && IsRealKind(tgt_type.Kind())) {
    llvm::Type* float_ty =
        (tgt_type.Kind() == TypeKind::kReal)
            ? llvm::Type::getDoubleTy(context.GetLlvmContext())
            : llvm::Type::getFloatTy(context.GetLlvmContext());
    bool is_signed = IsPackedSigned(src_type, types);
    if (is_signed) {
      result = builder.CreateSIToFP(source, float_ty, "sitofp");
    } else {
      result = builder.CreateUIToFP(source, float_ty, "uitofp");
    }
    builder.CreateStore(result, target_ptr);
    return;
  }

  // real/shortreal → integral
  if (IsRealKind(src_type.Kind()) && IsPacked(tgt_type)) {
    llvm::Type* target_type = context.GetPlaceLlvmType(target);
    bool is_signed = IsPackedSigned(tgt_type, types);
    if (is_signed) {
      result = builder.CreateFPToSI(source, target_type, "fptosi");
    } else {
      result = builder.CreateFPToUI(source, target_type, "fptoui");
    }
    builder.CreateStore(result, target_ptr);
    return;
  }

  // real → shortreal (fptrunc)
  if (src_type.Kind() == TypeKind::kReal &&
      tgt_type.Kind() == TypeKind::kShortReal) {
    result = builder.CreateFPTrunc(
        source, llvm::Type::getFloatTy(context.GetLlvmContext()), "fptrunc");
    builder.CreateStore(result, target_ptr);
    return;
  }

  // shortreal → real (fpext)
  if (src_type.Kind() == TypeKind::kShortReal &&
      tgt_type.Kind() == TypeKind::kReal) {
    result = builder.CreateFPExt(
        source, llvm::Type::getDoubleTy(context.GetLlvmContext()), "fpext");
    builder.CreateStore(result, target_ptr);
    return;
  }

  throw common::UnsupportedErrorException(
      common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kType,
      context.GetCurrentOrigin(), "unsupported real cast combination");
}

}  // namespace

auto IsRealTypedRvalue(Context& context, const mir::Compute& compute) -> bool {
  const auto& types = context.GetTypeArena();

  return std::visit(
      Overloaded{
          [&](const mir::UnaryRvalueInfo&) {
            TypeId tid = GetOperandTypeId(context, compute.value.operands[0]);
            return IsRealKind(types[tid].Kind());
          },
          [&](const mir::BinaryRvalueInfo&) {
            TypeId tid = GetOperandTypeId(context, compute.value.operands[0]);
            return IsRealKind(types[tid].Kind());
          },
          [&](const mir::CastRvalueInfo& info) {
            return IsRealKind(types[info.source_type].Kind()) ||
                   IsRealKind(types[info.target_type].Kind());
          },
          [](const auto&) { return false; },
      },
      compute.value.info);
}

void LowerRealCompute(Context& context, const mir::Compute& compute) {
  std::visit(
      Overloaded{
          [&](const mir::UnaryRvalueInfo& info) {
            LowerRealUnary(
                context, info, compute.value.operands, compute.target);
          },
          [&](const mir::BinaryRvalueInfo& info) {
            LowerRealBinary(
                context, info, compute.value.operands, compute.target);
          },
          [&](const mir::CastRvalueInfo& info) {
            LowerRealCast(
                context, info, compute.value.operands, compute.target);
          },
          [&](const auto&) {
            throw common::UnsupportedErrorException(
                common::UnsupportedLayer::kMirToLlvm,
                common::UnsupportedKind::kFeature, context.GetCurrentOrigin(),
                std::format(
                    "unsupported real rvalue kind: {}",
                    mir::GetRvalueKind(compute.value.info)));
          },
      },
      compute.value.info);
}

}  // namespace lyra::lowering::mir_to_llvm
