#include "lyra/llvm_backend/format_lowering.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <span>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/runtime/marshal.hpp"

namespace lyra::lowering::mir_to_llvm {

auto ValidateFormatOps(Context& context, std::span<const mir::FormatOp> ops)
    -> Result<void> {
  const auto& types = context.GetTypeArena();

  for (const auto& op : ops) {
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
        return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
            context.GetCurrentOrigin(),
            std::format("unsupported type kind in format op: {}", ToString(ty)),
            UnsupportedCategory::kType));
      }
      // Check width limit
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
        return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
            context.GetCurrentOrigin(),
            std::format(
                "format op with values wider than 64 bits not supported (got "
                "{} bits)",
                width),
            UnsupportedCategory::kType));
      }
      // 4-state types use the value plane directly for display formatting.
    }
  }
  return {};
}

auto LowerFormatOpToBuffer(
    Context& context, llvm::Value* buf, const mir::FormatOp& op)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* null_ptr = llvm::ConstantPointerNull::get(ptr_ty);

  if (op.kind == FormatKind::kLiteral) {
    // Append literal string
    auto* str_const = builder.CreateGlobalStringPtr(op.literal);
    auto* len = llvm::ConstantInt::get(i64_ty, op.literal.size());
    builder.CreateCall(
        context.GetLyraStringFormatLiteral(), {buf, str_const, len});
    return {};
  }

  if (op.kind == FormatKind::kString) {
    // Append string handle contents
    if (op.value.has_value()) {
      auto handle_or_err = LowerOperand(context, *op.value);
      if (!handle_or_err) return std::unexpected(handle_or_err.error());
      llvm::Value* handle = *handle_or_err;
      builder.CreateCall(context.GetLyraStringFormatString(), {buf, handle});
    }
    return {};
  }

  // Value formatting - mirrors LowerDisplayEffect
  int32_t width = 32;
  bool is_signed = false;
  auto value_kind = runtime::RuntimeValueKind::kIntegral;

  if (op.type) {
    const Type& ty = types[op.type];
    if (ty.Kind() == TypeKind::kIntegral) {
      width = static_cast<int32_t>(ty.AsIntegral().bit_width);
      is_signed = ty.AsIntegral().is_signed;
    } else if (ty.Kind() == TypeKind::kReal) {
      value_kind = runtime::RuntimeValueKind::kReal64;
      width = 64;
    } else if (ty.Kind() == TypeKind::kShortReal) {
      value_kind = runtime::RuntimeValueKind::kReal32;
      width = 32;
    } else if (IsPacked(ty)) {
      width = static_cast<int32_t>(PackedBitWidth(ty, types));
      is_signed = IsPackedSigned(ty, types);
    }
  }

  // Get pointer to the value
  llvm::Value* data_ptr = nullptr;
  if (op.value.has_value()) {
    auto value_or_err = LowerOperand(context, *op.value);
    if (!value_or_err) return std::unexpected(value_or_err.error());
    llvm::Value* value = *value_or_err;

    if (value_kind != runtime::RuntimeValueKind::kIntegral) {
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
  auto* value_kind_val =
      llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(value_kind));
  auto* width_val = llvm::ConstantInt::get(i32_ty, width);
  auto* signed_val = llvm::ConstantInt::get(i1_ty, is_signed ? 1 : 0);
  auto* output_width_val =
      llvm::ConstantInt::get(i32_ty, op.mods.width.value_or(-1));
  auto* precision_val =
      llvm::ConstantInt::get(i32_ty, op.mods.precision.value_or(-1));
  auto* zero_pad_val = llvm::ConstantInt::get(i1_ty, op.mods.zero_pad ? 1 : 0);
  auto* left_align_val =
      llvm::ConstantInt::get(i1_ty, op.mods.left_align ? 1 : 0);

  builder.CreateCall(
      context.GetLyraStringFormatValue(),
      {buf, format_val, value_kind_val, data_ptr, width_val, signed_val,
       output_width_val, precision_val, zero_pad_val, left_align_val, null_ptr,
       null_ptr});

  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
