#include "lyra/llvm_backend/instruction_compute.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <variant>
#include <vector>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "lyra/common/constant.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/runtime_query_kind.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction_compute_4state.hpp"
#include "lyra/llvm_backend/instruction_compute_builtin.hpp"
#include "lyra/llvm_backend/instruction_compute_cast.hpp"
#include "lyra/llvm_backend/instruction_compute_math.hpp"
#include "lyra/llvm_backend/instruction_compute_ops.hpp"
#include "lyra/llvm_backend/instruction_compute_real.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

enum class PlaceKind {
  kIntegral,
  kString,
};

struct PlaceTypeInfo {
  PlaceKind kind;
  uint32_t bit_width;
  bool is_four_state;
};

auto ValidateAndGetTypeInfo(Context& context, mir::PlaceId place_id)
    -> PlaceTypeInfo {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  const auto& place = arena[place_id];
  const Type& type = types[mir::TypeOfPlace(types, place)];

  if (type.Kind() == TypeKind::kString) {
    return PlaceTypeInfo{
        .kind = PlaceKind::kString,
        .bit_width = 0,
        .is_four_state = false,
    };
  }

  if (!IsPacked(type)) {
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kType,
        context.GetCurrentOrigin(),
        std::format("non-packed type not supported: {}", ToString(type)));
  }
  return PlaceTypeInfo{
      .kind = PlaceKind::kIntegral,
      .bit_width = PackedBitWidth(type, types),
      .is_four_state = IsPackedFourState(type, types),
  };
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

auto IsStringOperand(Context& context, const mir::Operand& operand) -> bool {
  TypeId type_id = GetOperandTypeId(context, operand);
  return context.GetTypeArena()[type_id].Kind() == TypeKind::kString;
}

auto LowerStringBinaryOp(
    Context& context, const mir::BinaryRvalueInfo& info,
    const std::vector<mir::Operand>& operands, llvm::Type* result_type)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  if (!IsComparisonOp(info.op)) {
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm,
        common::UnsupportedKind::kOperation, context.GetCurrentOrigin(),
        std::format(
            "string operation not supported (only comparisons): {}",
            mir::ToString(info.op)));
  }

  llvm::Value* lhs = LowerOperand(context, operands[0]);
  llvm::Value* rhs = LowerOperand(context, operands[1]);

  if (std::holds_alternative<Constant>(operands[0].payload)) {
    context.RegisterOwnedTemp(lhs);
  }
  if (std::holds_alternative<Constant>(operands[1].payload)) {
    context.RegisterOwnedTemp(rhs);
  }

  llvm::Value* cmp_result =
      builder.CreateCall(context.GetLyraStringCmp(), {lhs, rhs}, "strcmp");

  auto* zero = llvm::ConstantInt::get(cmp_result->getType(), 0);
  llvm::Value* bool_result = nullptr;

  switch (info.op) {
    case mir::BinaryOp::kEqual:
      bool_result = builder.CreateICmpEQ(cmp_result, zero, "str.eq");
      break;
    case mir::BinaryOp::kNotEqual:
      bool_result = builder.CreateICmpNE(cmp_result, zero, "str.ne");
      break;
    case mir::BinaryOp::kLessThan:
    case mir::BinaryOp::kLessThanSigned:
      bool_result = builder.CreateICmpSLT(cmp_result, zero, "str.lt");
      break;
    case mir::BinaryOp::kLessThanEqual:
    case mir::BinaryOp::kLessThanEqualSigned:
      bool_result = builder.CreateICmpSLE(cmp_result, zero, "str.le");
      break;
    case mir::BinaryOp::kGreaterThan:
    case mir::BinaryOp::kGreaterThanSigned:
      bool_result = builder.CreateICmpSGT(cmp_result, zero, "str.gt");
      break;
    case mir::BinaryOp::kGreaterThanEqual:
    case mir::BinaryOp::kGreaterThanEqualSigned:
      bool_result = builder.CreateICmpSGE(cmp_result, zero, "str.ge");
      break;
    default:
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm,
          common::UnsupportedKind::kOperation, context.GetCurrentOrigin(),
          std::format(
              "unsupported string comparison: {}", mir::ToString(info.op)));
  }

  return builder.CreateZExt(bool_result, result_type, "str.cmp.ext");
}

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

void LowerStringConcat(
    Context& context, const mir::ConcatRvalueInfo& /*info*/,
    const std::vector<mir::Operand>& operands, mir::PlaceId target_place) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);

  auto count = static_cast<int64_t>(operands.size());

  // Lower each operand → string handle
  std::vector<llvm::Value*> handles;
  handles.reserve(operands.size());
  for (const auto& operand : operands) {
    llvm::Value* handle = LowerOperand(context, operand);
    handles.push_back(handle);

    // Register constant operands for release at statement end
    if (std::holds_alternative<Constant>(operand.payload)) {
      context.RegisterOwnedTemp(handle);
    }
  }

  // Build array on stack: alloca [N x ptr]
  auto* array_alloca =
      builder.CreateAlloca(ptr_ty, llvm::ConstantInt::get(i64_ty, count));
  for (size_t i = 0; i < handles.size(); ++i) {
    auto* slot = builder.CreateGEP(
        ptr_ty, array_alloca, {llvm::ConstantInt::get(i64_ty, i)});
    builder.CreateStore(handles[i], slot);
  }

  // Call LyraStringConcat
  llvm::Value* result = builder.CreateCall(
      context.GetLyraStringConcat(),
      {array_alloca, llvm::ConstantInt::get(i64_ty, count)}, "str.concat");

  // Store result into target place (NOT registered as owned temp)
  llvm::Value* target_ptr = context.GetPlacePointer(target_place);
  builder.CreateStore(result, target_ptr);
}

void LowerSFormatRvalue(
    Context& context, const mir::SFormatRvalueInfo& info,
    mir::PlaceId target_place) {
  // Check for unsupported paths
  if (info.has_runtime_format) {
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kFeature,
        context.GetCurrentOrigin(),
        "$sformat/$sformatf with runtime format string not supported in LLVM "
        "backend");
  }
  if (info.ops.empty()) {
    // Auto-format path: $swrite/$swriteh/$swriteb/$swriteo without format
    // string
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kFeature,
        context.GetCurrentOrigin(),
        "$swrite/$swriteh/$swriteb/$swriteo auto-format (no format string) not "
        "supported in LLVM backend");
  }

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* null_ptr = llvm::ConstantPointerNull::get(ptr_ty);

  // PHASE 1: Validate all ops BEFORE calling Start() (exception safety)
  for (const auto& op : info.ops) {
    if (op.kind == FormatKind::kLiteral) {
      continue;  // Always supported
    }
    if (op.kind == FormatKind::kString) {
      continue;  // String handle
    }
    // Value formatting - check constraints
    if (op.type) {
      const Type& ty = types[op.type];
      // Check supported type kinds
      if (ty.Kind() != TypeKind::kIntegral && ty.Kind() != TypeKind::kReal &&
          ty.Kind() != TypeKind::kShortReal && !IsPacked(ty)) {
        throw common::UnsupportedErrorException(
            common::UnsupportedLayer::kMirToLlvm,
            common::UnsupportedKind::kType, context.GetCurrentOrigin(),
            std::format("unsupported type kind in $sformat: {}", ToString(ty)));
      }
      // Check width limit (same as $display)
      int32_t width = 32;
      if (ty.Kind() == TypeKind::kIntegral) {
        width = static_cast<int32_t>(ty.AsIntegral().bit_width);
      } else if (IsPacked(ty)) {
        width = static_cast<int32_t>(PackedBitWidth(ty, types));
      } else if (ty.Kind() == TypeKind::kReal) {
        width = 64;
      } else if (ty.Kind() == TypeKind::kShortReal) {
        width = 32;
      }
      if (width > 64 && ty.Kind() != TypeKind::kReal &&
          ty.Kind() != TypeKind::kShortReal) {
        throw common::UnsupportedErrorException(
            common::UnsupportedLayer::kMirToLlvm,
            common::UnsupportedKind::kType, context.GetCurrentOrigin(),
            std::format(
                "$sformat with values wider than 64 bits not supported (got "
                "{} bits)",
                width));
      }
      // Check 4-state (would need x/z masks)
      if (ty.Kind() == TypeKind::kIntegral && ty.AsIntegral().is_four_state) {
        throw common::UnsupportedErrorException(
            common::UnsupportedLayer::kMirToLlvm,
            common::UnsupportedKind::kType, context.GetCurrentOrigin(),
            "$sformat with 4-state operand not supported in LLVM backend (use "
            "2-state)");
      }
      if (IsPacked(ty) && IsPackedFourState(ty, types)) {
        throw common::UnsupportedErrorException(
            common::UnsupportedLayer::kMirToLlvm,
            common::UnsupportedKind::kType, context.GetCurrentOrigin(),
            "$sformat with 4-state operand not supported in LLVM backend (use "
            "2-state)");
      }
    }
  }

  // PHASE 2: Emit code (no exceptions expected from here)
  llvm::Value* buf =
      builder.CreateCall(context.GetLyraStringFormatStart(), {}, "sformat.buf");

  for (const auto& op : info.ops) {
    if (op.kind == FormatKind::kLiteral) {
      // Append literal string
      auto* str_const = builder.CreateGlobalStringPtr(op.literal);
      auto* len = llvm::ConstantInt::get(i64_ty, op.literal.size());
      builder.CreateCall(
          context.GetLyraStringFormatLiteral(), {buf, str_const, len});
    } else if (op.kind == FormatKind::kString) {
      // Append string handle contents
      if (op.value.has_value()) {
        llvm::Value* handle = LowerOperand(context, *op.value);
        builder.CreateCall(context.GetLyraStringFormatString(), {buf, handle});
      }
    } else {
      // Value formatting - mirrors LowerDisplayEffect
      int32_t width = 32;
      bool is_signed = false;
      bool is_real = false;

      if (op.type) {
        const Type& ty = types[op.type];
        if (ty.Kind() == TypeKind::kIntegral) {
          width = static_cast<int32_t>(ty.AsIntegral().bit_width);
          is_signed = ty.AsIntegral().is_signed;
        } else if (ty.Kind() == TypeKind::kReal) {
          is_real = true;
          width = 64;
        } else if (ty.Kind() == TypeKind::kShortReal) {
          is_real = true;
          width = 32;
        } else if (IsPacked(ty)) {
          width = static_cast<int32_t>(PackedBitWidth(ty, types));
          is_signed = IsPackedSigned(ty, types);
        }
      }

      // Get pointer to the value
      llvm::Value* data_ptr = nullptr;
      if (op.value.has_value()) {
        llvm::Value* value = LowerOperand(context, *op.value);

        if (is_real) {
          // For real types, allocate matching float type and store
          auto* alloca = builder.CreateAlloca(value->getType());
          builder.CreateStore(value, alloca);
          data_ptr = alloca;
        } else {
          // For integral types, allocate storage sized to match width
          llvm::Type* storage_ty = nullptr;
          if (width <= 8) {
            storage_ty = llvm::Type::getInt8Ty(llvm_ctx);
          } else if (width <= 16) {
            storage_ty = llvm::Type::getInt16Ty(llvm_ctx);
          } else if (width <= 32) {
            storage_ty = llvm::Type::getInt32Ty(llvm_ctx);
          } else {
            storage_ty = llvm::Type::getInt64Ty(llvm_ctx);
          }

          auto* alloca = builder.CreateAlloca(storage_ty);
          llvm::Value* sized_value = value;
          if (value->getType() != storage_ty) {
            if (value->getType()->getIntegerBitWidth() >
                storage_ty->getIntegerBitWidth()) {
              sized_value = builder.CreateTrunc(value, storage_ty);
            } else if (is_signed) {
              sized_value = builder.CreateSExt(value, storage_ty);
            } else {
              sized_value = builder.CreateZExt(value, storage_ty);
            }
          }
          builder.CreateStore(sized_value, alloca);
          data_ptr = alloca;
        }
      } else {
        data_ptr = null_ptr;
      }

      // Call LyraStringFormatValue
      auto* format_val =
          llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(op.kind));
      auto* width_val = llvm::ConstantInt::get(i32_ty, width);
      auto* signed_val = llvm::ConstantInt::get(i1_ty, is_signed ? 1 : 0);
      auto* output_width_val =
          llvm::ConstantInt::get(i32_ty, op.mods.width.value_or(-1));
      auto* precision_val =
          llvm::ConstantInt::get(i32_ty, op.mods.precision.value_or(-1));
      auto* zero_pad_val =
          llvm::ConstantInt::get(i1_ty, op.mods.zero_pad ? 1 : 0);
      auto* left_align_val =
          llvm::ConstantInt::get(i1_ty, op.mods.left_align ? 1 : 0);

      builder.CreateCall(
          context.GetLyraStringFormatValue(),
          {buf, format_val, data_ptr, width_val, signed_val, output_width_val,
           precision_val, zero_pad_val, left_align_val, null_ptr, null_ptr});
    }
  }

  // Finish and store result
  llvm::Value* result = builder.CreateCall(
      context.GetLyraStringFormatFinish(), {buf}, "sformat.result");

  llvm::Value* target_ptr = context.GetPlacePointer(target_place);
  builder.CreateStore(result, target_ptr);
}

}  // namespace

void LowerCompute(Context& context, const mir::Compute& compute) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  // Void function calls: call for side effects only, no result to store
  if (const auto* user_call =
          std::get_if<mir::UserCallRvalueInfo>(&compute.value.info)) {
    llvm::Function* callee = context.GetUserFunction(user_call->callee);
    if (callee->getReturnType()->isVoidTy()) {
      // Build argument list: design pointer, engine pointer, then user args
      std::vector<llvm::Value*> args;
      args.push_back(context.GetDesignPointer());
      args.push_back(context.GetEnginePointer());

      // Add user arguments
      for (const auto& operand : compute.value.operands) {
        args.push_back(LowerOperand(context, operand));
      }

      // Call void function for side effects only
      builder.CreateCall(callee, args);
      return;
    }
  }

  // Dynamic array builtins: early-exit before ValidateAndGetTypeInfo
  if (const auto* builtin =
          std::get_if<mir::BuiltinCallRvalueInfo>(&compute.value.info)) {
    if (builtin->method == mir::BuiltinMethod::kNewArray ||
        builtin->method == mir::BuiltinMethod::kArraySize ||
        builtin->method == mir::BuiltinMethod::kArrayDelete) {
      LowerDynArrayBuiltin(context, compute, *builtin);
      return;
    }
    if (builtin->method == mir::BuiltinMethod::kQueueSize ||
        builtin->method == mir::BuiltinMethod::kQueueDelete ||
        builtin->method == mir::BuiltinMethod::kQueueDeleteAt ||
        builtin->method == mir::BuiltinMethod::kQueuePushBack ||
        builtin->method == mir::BuiltinMethod::kQueuePushFront ||
        builtin->method == mir::BuiltinMethod::kQueuePopBack ||
        builtin->method == mir::BuiltinMethod::kQueuePopFront ||
        builtin->method == mir::BuiltinMethod::kQueueInsert) {
      LowerQueueBuiltin(context, compute, *builtin);
      return;
    }
    if (builtin->method == mir::BuiltinMethod::kEnumNext ||
        builtin->method == mir::BuiltinMethod::kEnumPrev ||
        builtin->method == mir::BuiltinMethod::kEnumName) {
      LowerEnumBuiltin(context, compute, *builtin);
      return;
    }
  }

  // Unpacked array aggregate construction: early-exit before
  // ValidateAndGetTypeInfo
  if (std::holds_alternative<mir::AggregateRvalueInfo>(compute.value.info)) {
    const auto& arena = context.GetMirArena();
    const Type& target_type =
        types[mir::TypeOfPlace(types, arena[compute.target])];
    if (target_type.Kind() == TypeKind::kUnpackedArray) {
      llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
      llvm::Type* arr_type = context.GetPlaceLlvmType(compute.target);
      llvm::Type* elem_type = arr_type->getArrayElementType();

      llvm::Value* aggregate = llvm::UndefValue::get(arr_type);
      for (size_t i = 0; i < compute.value.operands.size(); ++i) {
        llvm::Value* elem = LowerOperandAsStorage(
            context, compute.value.operands[i], elem_type);
        aggregate = builder.CreateInsertValue(
            aggregate, elem, {static_cast<unsigned>(i)});
      }
      builder.CreateStore(aggregate, target_ptr);
      return;
    }
    if (target_type.Kind() == TypeKind::kUnpackedStruct) {
      const auto& struct_info = target_type.AsUnpackedStruct();

      // Safety check: operand count must match field count
      if (compute.value.operands.size() != struct_info.fields.size()) {
        throw common::InternalError(
            "LowerCompute",
            std::format(
                "struct aggregate operand count {} != field count {}",
                compute.value.operands.size(), struct_info.fields.size()));
      }

      llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
      llvm::Type* struct_type = context.GetPlaceLlvmType(compute.target);

      llvm::Value* aggregate = llvm::UndefValue::get(struct_type);
      for (size_t i = 0; i < compute.value.operands.size(); ++i) {
        llvm::Type* field_type = llvm::cast<llvm::StructType>(struct_type)
                                     ->getElementType(static_cast<unsigned>(i));
        llvm::Value* field_val = LowerOperandAsStorage(
            context, compute.value.operands[i], field_type);
        aggregate = builder.CreateInsertValue(
            aggregate, field_val, {static_cast<unsigned>(i)});
      }
      builder.CreateStore(aggregate, target_ptr);
      return;
    }
    if (target_type.Kind() == TypeKind::kQueue) {
      auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
      auto* i64_ty = llvm::Type::getInt64Ty(context.GetLlvmContext());
      auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());

      size_t n = compute.value.operands.size();
      uint32_t max_bound = target_type.AsQueue().max_bound;
      if (max_bound > 0 && n > static_cast<size_t>(max_bound) + 1) {
        n = static_cast<size_t>(max_bound) + 1;
      }

      TypeId elem_type_id = target_type.AsQueue().element_type;
      auto elem_ops = context.GetElemOpsForType(elem_type_id);

      llvm::Value* handle = builder.CreateCall(
          context.GetLyraDynArrayNew(),
          {llvm::ConstantInt::get(i64_ty, n),
           llvm::ConstantInt::get(i32_ty, elem_ops.elem_size),
           elem_ops.clone_fn, elem_ops.destroy_fn},
          "q.lit.new");

      for (size_t i = 0; i < n; ++i) {
        llvm::Value* elem_ptr = builder.CreateCall(
            context.GetLyraDynArrayElementPtr(),
            {handle, llvm::ConstantInt::get(i64_ty, i)}, "q.lit.ep");
        llvm::Value* val = LowerOperandAsStorage(
            context, compute.value.operands[i], elem_ops.elem_llvm_type);

        if (elem_ops.needs_clone) {
          auto* clone_fn = llvm::cast<llvm::Function>(elem_ops.clone_fn);
          auto* temp = builder.CreateAlloca(
              elem_ops.elem_llvm_type, nullptr, "q.lit.tmp");
          builder.CreateStore(val, temp);
          builder.CreateCall(clone_fn, {elem_ptr, temp});
        } else {
          builder.CreateStore(val, elem_ptr);
        }
      }

      llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
      auto* old = builder.CreateLoad(ptr_ty, target_ptr, "q.lit.old");
      builder.CreateCall(context.GetLyraDynArrayRelease(), {old});
      builder.CreateStore(handle, target_ptr);
      return;
    }
  }

  // String concatenation: early-exit before ValidateAndGetTypeInfo
  if (const auto* concat_info =
          std::get_if<mir::ConcatRvalueInfo>(&compute.value.info)) {
    const Type& result_type = types[concat_info->result_type];
    if (result_type.Kind() == TypeKind::kString) {
      LowerStringConcat(
          context, *concat_info, compute.value.operands, compute.target);
      return;
    }
  }

  // SFormat: early-exit before ValidateAndGetTypeInfo (result is string)
  if (const auto* sformat_info =
          std::get_if<mir::SFormatRvalueInfo>(&compute.value.info)) {
    LowerSFormatRvalue(context, *sformat_info, compute.target);
    return;
  }

  // Unified cast dispatch: all casts go through dedicated handlers
  if (std::holds_alternative<mir::CastRvalueInfo>(compute.value.info)) {
    LowerCastUnified(context, compute);
    return;
  }
  if (std::holds_alternative<mir::BitCastRvalueInfo>(compute.value.info)) {
    LowerBitCastUnified(context, compute);
    return;
  }

  // Math functions (IEEE 1800 §20.8): dispatch by semantic category, not type
  if (IsMathCompute(context, compute)) {
    LowerMathCompute(context, compute);
    return;
  }

  // Real/shortreal operations: dispatch based on operand type
  if (IsRealMathCompute(context, compute)) {
    LowerRealCompute(context, compute);
    return;
  }

  PlaceTypeInfo type_info = ValidateAndGetTypeInfo(context, compute.target);

  if (type_info.is_four_state) {
    LowerCompute4State(context, compute, type_info.bit_width);
    return;
  }

  // 2-state path
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
                context, info, compute.value.operands, storage_type,
                type_info.bit_width);
          },
          [&](const mir::CastRvalueInfo&) -> llvm::Value* {
            throw common::InternalError(
                "LowerCompute", "casts use LowerCastUnified");
          },
          [&](const mir::BitCastRvalueInfo&) -> llvm::Value* {
            throw common::InternalError(
                "LowerCompute", "bitcasts use LowerBitCastUnified");
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
              case RuntimeQueryKind::kTime: {
                auto& builder = context.GetBuilder();
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
            auto& builder = context.GetBuilder();

            // Get the LLVM function for this MIR function
            llvm::Function* callee = context.GetUserFunction(info.callee);

            // Build argument list: design pointer, engine pointer, then user
            // args
            std::vector<llvm::Value*> args;
            args.push_back(context.GetDesignPointer());
            args.push_back(context.GetEnginePointer());

            // Add user arguments
            for (const auto& operand : compute.value.operands) {
              args.push_back(LowerOperand(context, operand));
            }

            // Check if this is a void function call
            bool is_void = callee->getReturnType()->isVoidTy();
            if (is_void) {
              // Void function call - no result to store
              builder.CreateCall(callee, args);
              // Return poison value since we can't return void
              // The caller shouldn't use this value for void functions
              return llvm::PoisonValue::get(storage_type);
            }

            // Call the function
            llvm::Value* result = builder.CreateCall(callee, args, "user_call");

            // Coerce result to storage type if needed
            if (result->getType() != storage_type &&
                result->getType()->isIntegerTy() &&
                storage_type->isIntegerTy()) {
              result =
                  builder.CreateZExtOrTrunc(result, storage_type, "call.fit");
            }

            return result;
          },
          [&](const auto& /*info*/) -> llvm::Value* {
            throw common::UnsupportedErrorException(
                common::UnsupportedLayer::kMirToLlvm,
                common::UnsupportedKind::kFeature, context.GetCurrentOrigin(),
                std::format(
                    "unsupported rvalue kind: {}",
                    mir::GetRvalueKind(compute.value.info)));
          },
      },
      compute.value.info);

  result = ApplyWidthMask(context, result, type_info.bit_width);

  builder.CreateStore(result, target_ptr);
}

}  // namespace lyra::lowering::mir_to_llvm
