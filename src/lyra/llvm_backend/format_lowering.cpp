#include "lyra/llvm_backend/format_lowering.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <span>
#include <utility>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/compute/cast.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/emit_string_conv.hpp"
#include "lyra/llvm_backend/slot_access.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/runtime/marshal.hpp"

namespace lyra::lowering::mir_to_llvm {

auto ValidateFormatOps(
    Context& context, const CuFacts& facts, std::span<const mir::FormatOp> ops)
    -> Result<void> {
  const auto& types = *facts.types;
  for (const auto& op : ops) {
    if (op.kind == FormatKind::kLiteral || op.kind == FormatKind::kString) {
      continue;
    }
    if (op.type) {
      const Type& ty = types[op.type];
      if (ty.Kind() != TypeKind::kIntegral && ty.Kind() != TypeKind::kReal &&
          ty.Kind() != TypeKind::kShortReal && !IsPacked(ty)) {
        return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
            context.GetCurrentOrigin(),
            std::format("unsupported type kind in format op: {}", ToString(ty)),
            UnsupportedCategory::kType));
      }
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
    }
  }
  return {};
}

auto LowerArgAsStringHandle(
    Context& context, const CuFacts& facts, const mir::Operand& operand,
    TypeId type_id) -> Result<std::pair<llvm::Value*, bool>> {
  CanonicalSlotAccess canonical(context, facts);
  return LowerArgAsStringHandle(context, facts, canonical, operand, type_id);
}

auto LowerFormatOpToBuffer(
    Context& context, const CuFacts& facts, llvm::Value* buffer_ptr,
    const mir::FormatOp& op) -> Result<void> {
  CanonicalSlotAccess canonical(context, facts);
  return LowerFormatOpToBuffer(context, facts, canonical, buffer_ptr, op);
}
auto LowerArgAsStringHandle(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::Operand& operand, TypeId type_id)
    -> Result<std::pair<llvm::Value*, bool>> {
  const auto& types = *facts.types;
  const Type& operand_type = types[type_id];

  if (operand_type.Kind() == TypeKind::kString) {
    auto handle_or_err = LowerOperand(context, facts, resolver, operand);
    if (!handle_or_err) return std::unexpected(handle_or_err.error());
    return std::pair{*handle_or_err, false};
  }

  if (IsPacked(operand_type)) {
    auto value_or_err = LowerOperand(context, facts, resolver, operand);
    if (!value_or_err) return std::unexpected(value_or_err.error());
    llvm::Value* packed_val = *value_or_err;

    if (!packed_val->getType()->isIntegerTy() &&
        !packed_val->getType()->isStructTy()) {
      throw common::InternalError(
          "LowerArgAsStringHandle",
          "packed operand should be iN or {iN,iN}, not pointer");
    }

    llvm::Value* handle =
        EmitPackedToString(context, facts, packed_val, operand_type);
    return std::pair{handle, true};
  }

  return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
      context.GetCurrentOrigin(),
      std::format(
          "unsupported type for %s format: got {}, expected string or packed",
          ToString(operand_type.Kind())),
      UnsupportedCategory::kType));
}

auto LowerFormatOpToBuffer(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    llvm::Value* buf, const mir::FormatOp& op) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = *facts.types;

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* null_ptr = llvm::ConstantPointerNull::get(ptr_ty);
  auto* engine_ptr = context.GetEnginePointer();
  auto* timeunit_val = llvm::ConstantInt::get(i8_ty, op.module_timeunit_power);

  if (op.kind == FormatKind::kLiteral) {
    auto* str_const = builder.CreateGlobalStringPtr(op.literal);
    auto* len = llvm::ConstantInt::get(i64_ty, op.literal.size());
    builder.CreateCall(
        context.GetLyraStringFormatLiteral(), {buf, str_const, len});
    return {};
  }

  if (op.kind == FormatKind::kString) {
    if (op.value.has_value()) {
      return WithStringHandle(
          context, facts, resolver, *op.value, op.type,
          [&](llvm::Value* h) -> Result<void> {
            builder.CreateCall(context.GetLyraStringFormatString(), {buf, h});
            return {};
          });
    }
    return {};
  }

  if (op.kind == FormatKind::kTime && op.value.has_value()) {
    auto value_or_err = LowerOperand(context, facts, resolver, *op.value);
    if (!value_or_err) return std::unexpected(value_or_err.error());

    llvm::Value* ticks = LowerTimeToTicks64(builder, *value_or_err);
    auto* alloca = builder.CreateAlloca(i64_ty);
    builder.CreateStore(ticks, alloca);

    builder.CreateCall(
        context.GetLyraStringFormatValue(),
        {buf, engine_ptr,
         llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(op.kind)),
         llvm::ConstantInt::get(
             i32_ty,
             static_cast<int32_t>(runtime::RuntimeValueKind::kIntegral)),
         alloca, llvm::ConstantInt::get(i32_ty, 64),
         llvm::ConstantInt::get(i1_ty, 0),
         llvm::ConstantInt::get(i32_ty, op.mods.width.value_or(-1)),
         llvm::ConstantInt::get(i32_ty, op.mods.precision.value_or(-1)),
         llvm::ConstantInt::get(i1_ty, op.mods.zero_pad ? 1 : 0),
         llvm::ConstantInt::get(i1_ty, op.mods.left_align ? 1 : 0), null_ptr,
         null_ptr, timeunit_val});
    return {};
  }

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

  llvm::Value* data_ptr = nullptr;
  if (op.value.has_value()) {
    auto value_or_err = LowerOperand(context, facts, resolver, *op.value);
    if (!value_or_err) return std::unexpected(value_or_err.error());
    llvm::Value* value = *value_or_err;

    if (value_kind != runtime::RuntimeValueKind::kIntegral) {
      auto* alloca = builder.CreateAlloca(value->getType());
      builder.CreateStore(value, alloca);
      data_ptr = alloca;
    } else {
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
      {buf, engine_ptr, format_val, value_kind_val, data_ptr, width_val,
       signed_val, output_width_val, precision_val, zero_pad_val,
       left_align_val, null_ptr, null_ptr, timeunit_val});

  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
