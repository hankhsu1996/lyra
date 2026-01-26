#include "lyra/llvm_backend/instruction_compute_string.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <variant>
#include <vector>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "lyra/common/constant.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction_compute_ops.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/operator.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Create an empty string handle via LyraStringFromLiteral("", 0).
// Returns a newly-owned handle (refcount=1).
auto CreateEmptyString(Context& context) -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto* i64_ty = llvm::Type::getInt64Ty(context.GetLlvmContext());
  auto* empty_data = builder.CreateGlobalStringPtr("");
  auto* empty_len = llvm::ConstantInt::get(i64_ty, 0);
  return builder.CreateCall(
      context.GetLyraStringFromLiteral(), {empty_data, empty_len}, "str.empty");
}

}  // namespace

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
    const std::vector<mir::Operand>& operands, mir::PlaceId target_place) {
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
    if (operands.empty()) {
      // No values to format - return empty string constant
      llvm::Value* result = CreateEmptyString(context);
      llvm::Value* target_ptr = context.GetPlacePointer(target_place);
      context.GetBuilder().CreateStore(result, target_ptr);
      return;
    }
    throw common::UnsupportedErrorException(
        common::UnsupportedLayer::kMirToLlvm, common::UnsupportedKind::kFeature,
        context.GetCurrentOrigin(),
        "$swrite/$swriteh/$swriteb/$swriteo auto-format with values not "
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

}  // namespace lyra::lowering::mir_to_llvm
