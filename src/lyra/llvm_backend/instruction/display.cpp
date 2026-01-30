#include "lyra/llvm_backend/instruction/display.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <span>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Alignment.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/format.hpp"
#include "lyra/common/severity.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/format_lowering.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/runtime/format_spec_abi.hpp"
#include "lyra/runtime/marshal.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Decompose wide iN value into [n x i64] array in canonical little-endian
// order. word[0] = bits 0-63, word[1] = bits 64-127, etc. Top word is masked to
// semantic width. Returns pointer to first element (uint64_t*), not array
// pointer.
//
// ABI contract: returned pointer valid only until runtime call returns.
// Runtime must NOT retain pointer beyond synchronous call.
auto EmitStoreWideToTemp(
    llvm::IRBuilder<>& builder, llvm::Value* wide_val, uint32_t bit_width,
    llvm::LLVMContext& ctx) -> llvm::Value* {
  size_t num_words = (static_cast<size_t>(bit_width) + 63) / 64;
  auto* i64_ty = llvm::Type::getInt64Ty(ctx);
  auto* array_ty = llvm::ArrayType::get(i64_ty, num_words);
  auto* alloca = builder.CreateAlloca(array_ty, nullptr, "wide.temp");
  alloca->setAlignment(llvm::Align(8));  // Guarantee 8-byte alignment

  // Always extend to padded width for predictable type in shift operations.
  // MUST use zext (not sext) to preserve bit pattern and clear padding bits.
  // Signedness is handled later via is_signed parameter to runtime.
  auto padded_width = static_cast<uint32_t>(num_words * 64);
  auto* padded_ty = llvm::IntegerType::get(ctx, padded_width);
  llvm::Value* extended = (wide_val->getType() == padded_ty)
                              ? wide_val
                              : builder.CreateZExt(wide_val, padded_ty);

  // Extract and store each 64-bit word
  for (size_t i = 0; i < num_words; ++i) {
    llvm::Value* word = nullptr;
    if (i == 0) {
      word = builder.CreateTrunc(extended, i64_ty);
    } else {
      // Use uint64_t for shift to handle very wide values (future-proof)
      // Shift amount type matches extended (padded_ty)
      uint64_t shift_bits = 64ULL * i;
      auto* shift_amt = llvm::ConstantInt::get(padded_ty, shift_bits);
      auto* shifted = builder.CreateLShr(extended, shift_amt);
      word = builder.CreateTrunc(shifted, i64_ty);
    }

    // Mask top word to semantic width (required by contract)
    // Guard avoids UB from (1 << 64) when top_bits == 0
    if (i == num_words - 1) {
      uint32_t top_bits = bit_width % 64;
      if (top_bits != 0) {
        uint64_t mask = (uint64_t{1} << top_bits) - 1;
        word = builder.CreateAnd(word, llvm::ConstantInt::get(i64_ty, mask));
      }
    }

    auto* gep = builder.CreateConstInBoundsGEP2_64(array_ty, alloca, 0, i);
    builder.CreateStore(word, gep);
  }

  // Return pointer to first element (uint64_t*), not array pointer
  // This matches runtime expectation: data is uint64_t*
  return builder.CreateConstInBoundsGEP2_64(array_ty, alloca, 0, 0);
}

// Store narrow integral (<=64 bits) to appropriately-sized temp storage.
// Returns pointer to the storage.
auto EmitStoreNarrowToTemp(
    llvm::IRBuilder<>& builder, llvm::Value* value, int32_t width,
    bool is_signed, llvm::LLVMContext& ctx) -> llvm::Value* {
  llvm::Type* storage_ty = nullptr;
  if (width <= 8) {
    storage_ty = llvm::Type::getInt8Ty(ctx);
  } else if (width <= 16) {
    storage_ty = llvm::Type::getInt16Ty(ctx);
  } else if (width <= 32) {
    storage_ty = llvm::Type::getInt32Ty(ctx);
  } else {
    storage_ty = llvm::Type::getInt64Ty(ctx);
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
  return alloca;
}

void LowerLiteralOp(Context& context, const mir::FormatOp& op) {
  auto& builder = context.GetBuilder();
  auto* str_const = builder.CreateGlobalStringPtr(op.literal);
  builder.CreateCall(context.GetLyraPrintLiteral(), {str_const});
}

auto LowerStringOp(Context& context, const mir::FormatOp& op) -> Result<void> {
  if (!op.value.has_value()) {
    return {};
  }

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  auto handle_or_err = LowerOperand(context, *op.value);
  if (!handle_or_err) return std::unexpected(handle_or_err.error());
  llvm::Value* handle = *handle_or_err;

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);

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
      llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(op.kind)), kind_ptr);

  auto* width_ptr = builder.CreateStructGEP(spec_ty, spec_alloca, 1);
  builder.CreateStore(
      llvm::ConstantInt::get(i32_ty, op.mods.width.value_or(-1)), width_ptr);

  auto* precision_ptr = builder.CreateStructGEP(spec_ty, spec_alloca, 2);
  builder.CreateStore(
      llvm::ConstantInt::get(i32_ty, op.mods.precision.value_or(-1)),
      precision_ptr);

  auto* flags_ptr = builder.CreateStructGEP(spec_ty, spec_alloca, 3);
  builder.CreateStore(llvm::ConstantInt::get(i8_ty, flags), flags_ptr);
  // reserved[3] left uninitialized (don't care)

  builder.CreateCall(context.GetLyraPrintString(), {handle, spec_alloca});
  return {};
}

auto LowerTimeOp(Context& context, const mir::FormatOp& op) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* null_ptr = llvm::ConstantPointerNull::get(ptr_ty);

  llvm::Value* data_ptr = nullptr;
  if (op.value.has_value()) {
    auto value_or_err = LowerOperand(context, *op.value);
    if (!value_or_err) return std::unexpected(value_or_err.error());
    llvm::Value* value = *value_or_err;
    auto* alloca = builder.CreateAlloca(i64_ty);
    if (value->getType()->getIntegerBitWidth() < 64) {
      value = builder.CreateZExt(value, i64_ty);
    }
    builder.CreateStore(value, alloca);
    data_ptr = alloca;
  } else {
    data_ptr = null_ptr;
  }

  auto* engine_ptr = context.GetEnginePointer();
  builder.CreateCall(
      context.GetLyraPrintValue(),
      {engine_ptr,
       llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(op.kind)),
       llvm::ConstantInt::get(
           i32_ty, static_cast<int32_t>(runtime::RuntimeValueKind::kIntegral)),
       data_ptr, llvm::ConstantInt::get(i32_ty, 64),
       llvm::ConstantInt::get(i1_ty, 0),
       llvm::ConstantInt::get(i32_ty, op.mods.width.value_or(-1)),
       llvm::ConstantInt::get(i32_ty, op.mods.precision.value_or(-1)),
       llvm::ConstantInt::get(i1_ty, op.mods.zero_pad ? 1 : 0),
       llvm::ConstantInt::get(i1_ty, op.mods.left_align ? 1 : 0), null_ptr,
       null_ptr, llvm::ConstantInt::get(i8_ty, op.module_timeunit_power)});

  return {};
}

auto LowerValueOp(Context& context, const mir::FormatOp& op) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* ptr_ty = llvm::PointerType::getUnqual(llvm_ctx);
  auto* null_ptr = llvm::ConstantPointerNull::get(ptr_ty);

  // Determine type info: width, signedness, value_kind, four_state
  int32_t width = 32;
  bool is_signed = false;
  bool is_four_state = false;
  auto value_kind = runtime::RuntimeValueKind::kIntegral;

  if (op.type) {
    const Type& ty = types[op.type];
    if (ty.Kind() == TypeKind::kIntegral) {
      width = static_cast<int32_t>(ty.AsIntegral().bit_width);
      is_signed = ty.AsIntegral().is_signed;
      is_four_state = ty.AsIntegral().is_four_state;
    } else if (ty.Kind() == TypeKind::kReal) {
      value_kind = runtime::RuntimeValueKind::kReal64;
      width = 64;
    } else if (ty.Kind() == TypeKind::kShortReal) {
      value_kind = runtime::RuntimeValueKind::kReal32;
      width = 32;
    } else if (IsPacked(ty)) {
      width = static_cast<int32_t>(PackedBitWidth(ty, types));
      is_signed = IsPackedSigned(ty, types);
      is_four_state = IsPackedFourState(ty, types);
    }
  }

  // Create storage for the value (and unknown plane for 4-state)
  llvm::Value* data_ptr = null_ptr;
  llvm::Value* unknown_ptr = null_ptr;

  if (op.value.has_value()) {
    if (value_kind != runtime::RuntimeValueKind::kIntegral) {
      // Real types: store directly (always 2-state)
      auto value_or_err = LowerOperand(context, *op.value);
      if (!value_or_err) return std::unexpected(value_or_err.error());
      auto* alloca = builder.CreateAlloca((*value_or_err)->getType());
      builder.CreateStore(*value_or_err, alloca);
      data_ptr = alloca;
    } else if (is_four_state) {
      // 4-state integral: use LowerOperandRaw to preserve struct
      auto raw_or_err = LowerOperandRaw(context, *op.value);
      if (!raw_or_err) return std::unexpected(raw_or_err.error());
      llvm::Value* raw = *raw_or_err;

      // Extract value and unknown planes from {iN, iN} struct
      llvm::Value* value_plane = builder.CreateExtractValue(raw, 0, "disp.val");
      llvm::Value* unknown_plane =
          builder.CreateExtractValue(raw, 1, "disp.unk");

      if (width > 64) {
        // Wide 4-state: decompose both planes to [n x i64] arrays
        value_kind = runtime::RuntimeValueKind::kWideIntegral;
        data_ptr = EmitStoreWideToTemp(
            builder, value_plane, static_cast<uint32_t>(width), llvm_ctx);
        unknown_ptr = EmitStoreWideToTemp(
            builder, unknown_plane, static_cast<uint32_t>(width), llvm_ctx);
      } else {
        // Narrow 4-state: store both planes to appropriately-sized temps
        data_ptr = EmitStoreNarrowToTemp(
            builder, value_plane, width, is_signed, llvm_ctx);
        unknown_ptr = EmitStoreNarrowToTemp(
            builder, unknown_plane, width, false, llvm_ctx);
      }
    } else {
      // 2-state integral
      auto value_or_err = LowerOperand(context, *op.value);
      if (!value_or_err) return std::unexpected(value_or_err.error());
      llvm::Value* value = *value_or_err;

      if (width > 64) {
        value_kind = runtime::RuntimeValueKind::kWideIntegral;
        data_ptr = EmitStoreWideToTemp(
            builder, value, static_cast<uint32_t>(width), llvm_ctx);
      } else {
        data_ptr =
            EmitStoreNarrowToTemp(builder, value, width, is_signed, llvm_ctx);
      }
    }
  }

  // Call LyraPrintValue (unknown_ptr passed as x_mask parameter)
  builder.CreateCall(
      context.GetLyraPrintValue(),
      {null_ptr, llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(op.kind)),
       llvm::ConstantInt::get(i32_ty, static_cast<int32_t>(value_kind)),
       data_ptr, llvm::ConstantInt::get(i32_ty, width),
       llvm::ConstantInt::get(i1_ty, is_signed ? 1 : 0),
       llvm::ConstantInt::get(i32_ty, op.mods.width.value_or(-1)),
       llvm::ConstantInt::get(i32_ty, op.mods.precision.value_or(-1)),
       llvm::ConstantInt::get(i1_ty, op.mods.zero_pad ? 1 : 0),
       llvm::ConstantInt::get(i1_ty, op.mods.left_align ? 1 : 0), unknown_ptr,
       null_ptr, llvm::ConstantInt::get(i8_ty, op.module_timeunit_power)});

  return {};
}

void LowerModulePathOp(Context& context) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  auto* engine_ptr = context.GetEnginePointer();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* instance_id =
      llvm::ConstantInt::get(i32_ty, context.GetCurrentInstanceId());

  builder.CreateCall(
      context.GetLyraPrintModulePath(), {engine_ptr, instance_id});
}

// Lower a sequence of FormatOps to LLVM IR (shared by display and severity)
auto LowerFormatOps(Context& context, std::span<const mir::FormatOp> ops)
    -> Result<void> {
  for (const auto& op : ops) {
    Result<void> result;
    switch (op.kind) {
      case FormatKind::kLiteral:
        LowerLiteralOp(context, op);
        break;
      case FormatKind::kString:
        result = LowerStringOp(context, op);
        break;
      case FormatKind::kTime:
        result = LowerTimeOp(context, op);
        break;
      case FormatKind::kModulePath:
        LowerModulePathOp(context);
        break;
      default:
        result = LowerValueOp(context, op);
        break;
    }
    if (!result) {
      return result;
    }
  }
  return {};
}

}  // namespace

auto LowerDisplayEffect(Context& context, const mir::DisplayEffect& display)
    -> Result<void> {
  if (display.descriptor) {
    // File-directed output: $fdisplay / $fwrite
    auto& builder = context.GetBuilder();

    // PHASE 1: Validate all ops BEFORE calling Start (ensures no early returns
    // after Start)
    auto validate_result = ValidateFormatOps(context, display.ops);
    if (!validate_result) return validate_result;

    // PHASE 2: Emit code (no failures expected - validation passed)
    auto* buf = builder.CreateCall(context.GetLyraStringFormatStart(), {});

    for (const auto& op : display.ops) {
      // LowerFormatOpToBuffer should not fail after validation
      auto result = LowerFormatOpToBuffer(context, buf, op);
      if (!result) return result;  // Defensive; validation should catch this
    }

    // Finish consumes buffer, returns +1 ref handle
    auto* message =
        builder.CreateCall(context.GetLyraStringFormatFinish(), {buf});

    // Lower descriptor - should be i32 from $fopen return type
    auto desc_or = LowerOperand(context, *display.descriptor);
    if (!desc_or) return std::unexpected(desc_or.error());

    // Call LyraFWrite
    auto* engine = context.GetEnginePointer();
    auto* add_newline = llvm::ConstantInt::get(
        llvm::Type::getInt1Ty(context.GetLlvmContext()),
        display.print_kind == PrintKind::kDisplay ? 1 : 0);
    builder.CreateCall(
        context.GetLyraFWrite(), {engine, *desc_or, message, add_newline});

    // Release temporary (Finish returns +1 ref; we consumed it)
    builder.CreateCall(context.GetLyraStringRelease(), {message});

    return {};
  }

  // Direct stdout output: $display / $write
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
