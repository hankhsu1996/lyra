#include "lyra/lowering/mir_to_llvm/instruction_display.hpp"

#include "lyra/common/format.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/lowering/mir_to_llvm/operand.hpp"

namespace lyra::lowering::mir_to_llvm {

void LowerDisplayEffect(Context& context, const mir::DisplayEffect& display) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* null_ptr = llvm::ConstantPointerNull::get(ptr_ty);

  for (const auto& op : display.ops) {
    if (op.kind == FormatKind::kLiteral) {
      // Call LyraPrintLiteral(str)
      auto* str_const = builder.CreateGlobalStringPtr(op.literal);
      builder.CreateCall(context.GetLyraPrintLiteral(), {str_const});
    } else {
      // Get type info for width and signedness
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
          width = 64;  // double is 64 bits
        } else if (IsPacked(ty)) {
          width = static_cast<int32_t>(PackedBitWidth(ty, types));
          is_signed = IsPackedSigned(ty, types);
        }
      }

      // Get pointer to the value
      llvm::Value* data_ptr = nullptr;
      if (op.value.has_value()) {
        llvm::Value* value = LowerOperand(context, *op.value);

        if (op.kind == FormatKind::kString) {
          // For strings, value is already a pointer (i8*) - pass directly
          data_ptr = value;
        } else if (is_real) {
          // For real types, allocate a double and store
          auto* double_ty = llvm::Type::getDoubleTy(llvm_ctx);
          auto* alloca = builder.CreateAlloca(double_ty);
          builder.CreateStore(value, alloca);
          data_ptr = alloca;
        } else {
          // For integral types, allocate storage sized to match width
          // This ensures LyraPrintValue can read the correct number of bytes
          if (width > 64) {
            throw common::InternalError(
                "LowerDisplayEffect",
                std::format(
                    "values wider than 64 bits not yet supported "
                    "(got {} bits)",
                    width));
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

      // Call LyraPrintValue(format, data, width, is_signed,
      //                     field_width, precision, zero_pad, left_align,
      //                     x_mask, z_mask)
      auto* format_val =
          llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(op.kind));
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

      builder.CreateCall(
          context.GetLyraPrintValue(),
          {format_val, data_ptr, width_val, signed_val, output_width_val,
           precision_val, zero_pad_val, left_align_val, null_ptr, null_ptr});
    }
  }

  // Call LyraPrintEnd(kind)
  auto* kind_val =
      llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(display.print_kind));
  builder.CreateCall(context.GetLyraPrintEnd(), {kind_val});
}

}  // namespace lyra::lowering::mir_to_llvm
