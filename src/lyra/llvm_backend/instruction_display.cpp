#include "lyra/llvm_backend/instruction_display.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <span>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/severity.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/runtime/format_spec_abi.hpp"
#include "lyra/runtime/marshal.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Lower a sequence of FormatOps to LLVM IR (shared by display and severity)
auto LowerFormatOps(Context& context, std::span<const mir::FormatOp> ops)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* null_ptr = llvm::ConstantPointerNull::get(ptr_ty);

  for (const auto& op : ops) {
    if (op.kind == FormatKind::kLiteral) {
      // Call LyraPrintLiteral(str)
      auto* str_const = builder.CreateGlobalStringPtr(op.literal);
      builder.CreateCall(context.GetLyraPrintLiteral(), {str_const});
    } else if (op.kind == FormatKind::kString) {
      // String: pass the handle to LyraPrintString with format spec pointer
      if (op.value.has_value()) {
        auto handle_or_err = LowerOperand(context, *op.value);
        if (!handle_or_err) return std::unexpected(handle_or_err.error());
        llvm::Value* handle = *handle_or_err;

        // Build LyraFormatSpec struct on stack
        uint8_t flags = 0;
        if (op.mods.zero_pad) {
          flags |= runtime::kFormatFlagZeroPad;
        }
        if (op.mods.left_align) {
          flags |= runtime::kFormatFlagLeftAlign;
        }
        auto* spec_ty = context.GetFormatSpecType();
        auto* spec_alloca = builder.CreateAlloca(spec_ty);
        auto* kind_ptr = builder.CreateStructGEP(spec_ty, spec_alloca, 0);
        builder.CreateStore(
            llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(op.kind)),
            kind_ptr);
        auto* width_ptr = builder.CreateStructGEP(spec_ty, spec_alloca, 1);
        builder.CreateStore(
            llvm::ConstantInt::get(i32_ty, op.mods.width.value_or(-1)),
            width_ptr);
        auto* precision_ptr = builder.CreateStructGEP(spec_ty, spec_alloca, 2);
        builder.CreateStore(
            llvm::ConstantInt::get(i32_ty, op.mods.precision.value_or(-1)),
            precision_ptr);
        auto* flags_ptr = builder.CreateStructGEP(spec_ty, spec_alloca, 3);
        builder.CreateStore(llvm::ConstantInt::get(i8_ty, flags), flags_ptr);
        // reserved[3] left uninitialized (don't care)

        builder.CreateCall(context.GetLyraPrintString(), {handle, spec_alloca});
      }
    } else if (op.kind == FormatKind::kTime) {
      // Time format: data is uint64_t time value, needs engine for formatting
      llvm::Value* data_ptr = nullptr;
      if (op.value.has_value()) {
        auto value_or_err = LowerOperand(context, *op.value);
        if (!value_or_err) return std::unexpected(value_or_err.error());
        llvm::Value* value = *value_or_err;
        auto* alloca = builder.CreateAlloca(i64_ty);
        // Extend to i64 if needed
        if (value->getType()->getIntegerBitWidth() < 64) {
          value = builder.CreateZExt(value, i64_ty);
        }
        builder.CreateStore(value, alloca);
        data_ptr = alloca;
      } else {
        data_ptr = null_ptr;
      }

      auto* engine_ptr = context.GetEnginePointer();
      auto* format_val =
          llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(op.kind));
      auto* value_kind_val = llvm::ConstantInt::get(
          i32_ty, static_cast<int32_t>(runtime::RuntimeValueKind::kIntegral));
      auto* width_val = llvm::ConstantInt::get(i32_ty, 64);
      auto* signed_val = llvm::ConstantInt::get(i1_ty, 0);
      auto* output_width_val =
          llvm::ConstantInt::get(i32_ty, op.mods.width.value_or(-1));
      auto* precision_val =
          llvm::ConstantInt::get(i32_ty, op.mods.precision.value_or(-1));
      auto* zero_pad_val =
          llvm::ConstantInt::get(i1_ty, op.mods.zero_pad ? 1 : 0);
      auto* left_align_val =
          llvm::ConstantInt::get(i1_ty, op.mods.left_align ? 1 : 0);
      auto* timeunit_val =
          llvm::ConstantInt::get(i8_ty, op.module_timeunit_power);

      builder.CreateCall(
          context.GetLyraPrintValue(),
          {engine_ptr, format_val, value_kind_val, data_ptr, width_val,
           signed_val, output_width_val, precision_val, zero_pad_val,
           left_align_val, null_ptr, null_ptr, timeunit_val});
    } else {
      // Get type info for width and signedness
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
          width = 64;  // double is 64 bits
        } else if (ty.Kind() == TypeKind::kShortReal) {
          value_kind = runtime::RuntimeValueKind::kReal32;
          width = 32;  // float is 32 bits
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
          // This ensures LyraPrintValue can read the correct number of bytes
          if (width > 64) {
            return std::unexpected(
                context.GetDiagnosticContext().MakeUnsupported(
                    context.GetCurrentOrigin(),
                    std::format(
                        "display of values wider than 64 bits not yet "
                        "supported (got {} bits)",
                        width),
                    UnsupportedCategory::kType));
          }

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
          // Truncate or extend value to match storage type
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

      // Call LyraPrintValue(engine, format, value_kind, data, width, is_signed,
      //                     field_width, precision, zero_pad, left_align,
      //                     x_mask, z_mask, module_timeunit_power)
      auto* format_val =
          llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(op.kind));
      auto* value_kind_val =
          llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(value_kind));
      auto* width_val = llvm::ConstantInt::get(i32_ty, width);
      auto* signed_val = llvm::ConstantInt::get(i1_ty, is_signed ? 1 : 0);

      // Pass format modifiers
      // output_width encodes FormatModifiers.width (optional<int>):
      // -1 = nullopt (auto-size), 0 = minimal, >0 = explicit width
      auto* output_width_val =
          llvm::ConstantInt::get(i32_ty, op.mods.width.value_or(-1));
      auto* precision_val =
          llvm::ConstantInt::get(i32_ty, op.mods.precision.value_or(-1));
      auto* zero_pad_val =
          llvm::ConstantInt::get(i1_ty, op.mods.zero_pad ? 1 : 0);
      auto* left_align_val =
          llvm::ConstantInt::get(i1_ty, op.mods.left_align ? 1 : 0);
      auto* timeunit_val =
          llvm::ConstantInt::get(i8_ty, op.module_timeunit_power);

      builder.CreateCall(
          context.GetLyraPrintValue(),
          {null_ptr, format_val, value_kind_val, data_ptr, width_val,
           signed_val, output_width_val, precision_val, zero_pad_val,
           left_align_val, null_ptr, null_ptr, timeunit_val});
    }
  }

  return {};
}

}  // namespace

auto LowerDisplayEffect(Context& context, const mir::DisplayEffect& display)
    -> Result<void> {
  if (display.descriptor) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        "$fdisplay/$fwrite not supported in LLVM backend",
        UnsupportedCategory::kFeature));
  }

  // Lower format ops (shared helper)
  auto result = LowerFormatOps(context, display.ops);
  if (!result) return result;

  // Call LyraPrintEnd(kind)
  auto& builder = context.GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
  auto* kind_val =
      llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(display.print_kind));
  builder.CreateCall(context.GetLyraPrintEnd(), {kind_val});
  return {};
}

auto LowerSeverityEffect(Context& context, const mir::SeverityEffect& severity)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());

  // 1. Print prefix using shared SeverityPrefixCStr (single source of truth)
  const char* prefix = SeverityPrefixCStr(severity.level);
  auto* prefix_ptr = builder.CreateGlobalStringPtr(prefix);
  builder.CreateCall(context.GetLyraPrintLiteral(), {prefix_ptr});

  // 2. Lower format ops (shared helper - same as display)
  auto result = LowerFormatOps(context, severity.ops);
  if (!result) return result;

  // 3. Newline (same as display)
  auto* kind_val =
      llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(PrintKind::kDisplay));
  builder.CreateCall(context.GetLyraPrintEnd(), {kind_val});

  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
