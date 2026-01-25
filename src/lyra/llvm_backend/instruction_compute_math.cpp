#include "lyra/llvm_backend/instruction_compute_math.hpp"

#include <cstdint>
#include <format>
#include <variant>
#include <vector>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Value.h"
#include "lyra/common/constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/math_fn.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/operand.hpp"
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

void LowerMathIntegralClog2(
    Context& context, const std::vector<mir::Operand>& operands,
    mir::PlaceId target) {
  auto& builder = context.GetBuilder();
  auto& module = context.GetModule();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  // Get target type info
  const auto& place = arena[target];
  const Type& target_type = types[mir::TypeOfPlace(types, place)];
  uint32_t target_width = PackedBitWidth(target_type, types);
  bool target_is_four_state = IsPackedFourState(target_type, types);

  auto* result_type =
      llvm::Type::getIntNTy(context.GetLlvmContext(), target_width);
  auto* zero = llvm::ConstantInt::get(result_type, 0);
  auto* all_ones = llvm::ConstantInt::get(
      result_type, llvm::APInt::getAllOnes(target_width));

  // Check if operand is 4-state
  bool operand_is_four_state = IsOperandFourState(context, operands[0]);

  llvm::Value* operand_val = nullptr;
  llvm::Value* operand_unk = nullptr;

  if (operand_is_four_state) {
    // Extract {value, unknown} from 4-state operand
    llvm::Value* raw = LowerOperandRaw(context, operands[0]);
    operand_val = builder.CreateExtractValue(raw, 0, "clog2.op.val");
    operand_unk = builder.CreateExtractValue(raw, 1, "clog2.op.unk");
    // Coerce to result width
    operand_val =
        builder.CreateZExtOrTrunc(operand_val, result_type, "clog2.val.coerce");
    operand_unk =
        builder.CreateZExtOrTrunc(operand_unk, result_type, "clog2.unk.coerce");
  } else {
    // 2-state operand
    operand_val = LowerOperand(context, operands[0]);
    operand_val =
        builder.CreateZExtOrTrunc(operand_val, result_type, "clog2.coerce");
    operand_unk = zero;
  }

  // Check if operand has any unknown bits
  auto* has_unknown = builder.CreateICmpNE(operand_unk, zero, "clog2.hasunk");

  // Compute clog2 of the value (assumes known)
  auto* one = llvm::ConstantInt::get(result_type, 1);
  auto* width_const = llvm::ConstantInt::get(result_type, target_width);

  // n <= 1 means result is 0 (handles both n=0 and n=1)
  auto* is_le_one = builder.CreateICmpULE(operand_val, one, "clog2.le1");

  // n - 1 (safe: when n=0, n-1 wraps to all-ones, but we select 0 anyway)
  auto* n_minus_1 = builder.CreateSub(operand_val, one, "clog2.nm1");

  // ctlz(n-1, /*is_zero_undef=*/false)
  auto* ctlz_decl = llvm::Intrinsic::getDeclaration(
      &module, llvm::Intrinsic::ctlz, {result_type});
  auto* lz = builder.CreateCall(
      ctlz_decl, {n_minus_1, builder.getFalse()}, "clog2.lz");

  // result = width - ctlz(n-1)
  auto* raw_result = builder.CreateSub(width_const, lz, "clog2.raw");

  // select: if n <= 1 then 0 else result
  auto* known_result =
      builder.CreateSelect(is_le_one, zero, raw_result, "clog2.known");

  // Store result
  llvm::Value* target_ptr = context.GetPlacePointer(target);
  if (target_is_four_state) {
    // 4-state target: if operand has unknown bits, result is all-X
    // result_val = has_unknown ? 0 : known_result (value is 0 for X
    // representation) result_unk = has_unknown ? all_ones : 0
    auto* result_val =
        builder.CreateSelect(has_unknown, zero, known_result, "clog2.res.val");
    auto* result_unk =
        builder.CreateSelect(has_unknown, all_ones, zero, "clog2.res.unk");

    auto* struct_type = context.GetPlaceLlvmType4State(target_width);
    llvm::Value* val_struct = llvm::UndefValue::get(struct_type);
    val_struct = builder.CreateInsertValue(val_struct, result_val, 0, "4s.v");
    val_struct = builder.CreateInsertValue(val_struct, result_unk, 1, "4s.u");
    builder.CreateStore(val_struct, target_ptr);
  } else {
    // 2-state target: just store the result (unknown bits become 0 implicitly)
    builder.CreateStore(known_result, target_ptr);
  }
}

auto LowerRealMathFnUnary(
    Context& context, MathFn fn, llvm::Value* operand, llvm::Type* float_ty)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& module = context.GetModule();

  llvm::Intrinsic::ID intrinsic_id = llvm::Intrinsic::not_intrinsic;
  switch (fn) {
    case MathFn::kLn:
      intrinsic_id = llvm::Intrinsic::log;
      break;
    case MathFn::kLog10:
      intrinsic_id = llvm::Intrinsic::log10;
      break;
    case MathFn::kExp:
      intrinsic_id = llvm::Intrinsic::exp;
      break;
    case MathFn::kSqrt:
      intrinsic_id = llvm::Intrinsic::sqrt;
      break;
    case MathFn::kFloor:
      intrinsic_id = llvm::Intrinsic::floor;
      break;
    case MathFn::kCeil:
      intrinsic_id = llvm::Intrinsic::ceil;
      break;
    case MathFn::kSin:
      intrinsic_id = llvm::Intrinsic::sin;
      break;
    case MathFn::kCos:
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

  const char* name = nullptr;
  bool is_double = float_ty->isDoubleTy();
  switch (fn) {
    case MathFn::kTan:
      name = is_double ? "tan" : "tanf";
      break;
    case MathFn::kAsin:
      name = is_double ? "asin" : "asinf";
      break;
    case MathFn::kAcos:
      name = is_double ? "acos" : "acosf";
      break;
    case MathFn::kAtan:
      name = is_double ? "atan" : "atanf";
      break;
    case MathFn::kSinh:
      name = is_double ? "sinh" : "sinhf";
      break;
    case MathFn::kCosh:
      name = is_double ? "cosh" : "coshf";
      break;
    case MathFn::kTanh:
      name = is_double ? "tanh" : "tanhf";
      break;
    case MathFn::kAsinh:
      name = is_double ? "asinh" : "asinhf";
      break;
    case MathFn::kAcosh:
      name = is_double ? "acosh" : "acoshf";
      break;
    case MathFn::kAtanh:
      name = is_double ? "atanh" : "atanhf";
      break;
    default:
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm,
          common::UnsupportedKind::kOperation, context.GetCurrentOrigin(),
          std::format("unsupported real math function: {}", ToString(fn)));
  }

  auto* func_ty = llvm::FunctionType::get(float_ty, {float_ty}, false);
  auto func = module.getOrInsertFunction(name, func_ty);
  if (auto* f = llvm::dyn_cast<llvm::Function>(func.getCallee())) {
    f->setDoesNotThrow();
  }
  return builder.CreateCall(func, {operand}, "math");
}

auto LowerRealMathFnBinary(
    Context& context, MathFn fn, llvm::Value* lhs, llvm::Value* rhs,
    llvm::Type* float_ty) -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& module = context.GetModule();

  if (fn == MathFn::kPow) {
    auto* decl = llvm::Intrinsic::getDeclaration(
        &module, llvm::Intrinsic::pow, {float_ty});
    return builder.CreateCall(decl, {lhs, rhs}, "pow");
  }

  const char* name = nullptr;
  bool is_double = float_ty->isDoubleTy();
  switch (fn) {
    case MathFn::kAtan2:
      name = is_double ? "atan2" : "atan2f";
      break;
    case MathFn::kHypot:
      name = is_double ? "hypot" : "hypotf";
      break;
    default:
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm,
          common::UnsupportedKind::kOperation, context.GetCurrentOrigin(),
          std::format(
              "unsupported real math binary function: {}", ToString(fn)));
  }

  auto* func_ty =
      llvm::FunctionType::get(float_ty, {float_ty, float_ty}, false);
  auto func = module.getOrInsertFunction(name, func_ty);
  if (auto* f = llvm::dyn_cast<llvm::Function>(func.getCallee())) {
    f->setDoesNotThrow();
  }
  return builder.CreateCall(func, {lhs, rhs}, "math");
}

void LowerMathCall(
    Context& context, const mir::MathCallRvalueInfo& info,
    const std::vector<mir::Operand>& operands, mir::PlaceId target) {
  auto& builder = context.GetBuilder();

  int expected_arity = GetMathFnArity(info.fn);
  if (static_cast<int>(operands.size()) != expected_arity) {
    throw common::InternalError(
        "LowerMathCall",
        std::format(
            "arity mismatch for {}: expected {}, got {}", ToString(info.fn),
            expected_arity, operands.size()));
  }

  // Handle $clog2 specially (integral operand)
  if (info.fn == MathFn::kClog2) {
    LowerMathIntegralClog2(context, operands, target);
    return;
  }

  // All other math functions are real-typed
  llvm::Type* float_ty = GetOperandFloatType(context, operands[0]);
  llvm::Value* result = nullptr;

  if (expected_arity == 1) {
    llvm::Value* operand = LowerOperand(context, operands[0]);
    result = LowerRealMathFnUnary(context, info.fn, operand, float_ty);
  } else {
    llvm::Value* lhs = LowerOperand(context, operands[0]);
    llvm::Value* rhs = LowerOperand(context, operands[1]);
    result = LowerRealMathFnBinary(context, info.fn, lhs, rhs, float_ty);
  }

  llvm::Value* target_ptr = context.GetPlacePointer(target);
  builder.CreateStore(result, target_ptr);
}

}  // namespace

auto IsMathCompute(
    [[maybe_unused]] Context& context, const mir::Compute& compute) -> bool {
  return std::holds_alternative<mir::MathCallRvalueInfo>(compute.value.info);
}

void LowerMathCompute(Context& context, const mir::Compute& compute) {
  const auto* info = std::get_if<mir::MathCallRvalueInfo>(&compute.value.info);
  if (info == nullptr) {
    throw common::InternalError(
        "LowerMathCompute", "expected MathCallRvalueInfo");
  }
  LowerMathCall(context, *info, compute.value.operands, compute.target);
}

}  // namespace lyra::lowering::mir_to_llvm
