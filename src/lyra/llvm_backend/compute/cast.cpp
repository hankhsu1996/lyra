#include "lyra/llvm_backend/compute/cast.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <type_traits>
#include <variant>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/ErrorHandling.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/compute/ops.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/emit_string_conv.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/type_query.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto IsRealKind(TypeKind kind) -> bool {
  return kind == TypeKind::kReal || kind == TypeKind::kShortReal;
}

auto IsSignedIntegral(const TypeArena& types, TypeId type_id) -> bool {
  const Type& type = types[type_id];
  if (!IsPacked(type)) {
    return false;
  }
  return IsPackedSigned(type, types);
}

auto SignExtendFromSemanticWidth(
    llvm::IRBuilderBase& builder, llvm::Value* val, llvm::Type* dst_type,
    uint32_t semantic_bits) -> llvm::Value* {
  auto dst_bits = dst_type->getIntegerBitWidth();
  llvm::Value* extended = builder.CreateZExtOrTrunc(val, dst_type);
  if (semantic_bits == 0 || semantic_bits >= dst_bits) {
    return extended;
  }
  uint32_t shift_amount = dst_bits - semantic_bits;
  auto* shift_const = llvm::ConstantInt::get(dst_type, shift_amount);
  llvm::Value* shifted_left = builder.CreateShl(extended, shift_const);
  return builder.CreateAShr(shifted_left, shift_const);
}

auto ExtOrTrunc(
    llvm::IRBuilderBase& builder, llvm::Value* val, llvm::Type* dst_type,
    bool is_signed) -> llvm::Value* {
  if (is_signed) {
    return builder.CreateSExtOrTrunc(val, dst_type, "cast.sext");
  }
  return builder.CreateZExtOrTrunc(val, dst_type, "cast.zext");
}

auto ResizeFourStatePlanes(
    llvm::IRBuilderBase& builder, FourStateValue fs, llvm::Type* dst_type,
    bool is_signed, uint32_t source_semantic_bits) -> FourStateValue {
  llvm::Value* val =
      is_signed ? SignExtendFromSemanticWidth(
                      builder, fs.value, dst_type, source_semantic_bits)
                : builder.CreateZExtOrTrunc(fs.value, dst_type);
  llvm::Value* unk = builder.CreateZExtOrTrunc(fs.unknown, dst_type);
  return FourStateValue{.value = val, .unknown = unk};
}

auto GetSemanticBitWidth(const TypeArena& types, TypeId type_id) -> uint32_t {
  const Type& type = types[type_id];
  if (!IsPacked(type)) {
    return 0;
  }
  return PackedBitWidth(type, types);
}

auto GetTargetBitWidth(const TypeArena& types, TypeId destination_type)
    -> uint32_t {
  return GetSemanticBitWidth(types, destination_type);
}

auto LoadFourStateOperand(Context& context, const mir::Operand& operand)
    -> Result<FourStateValue> {
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  bool force_2s = context.IsForceTwoState();
  bool is_four_state = std::visit(
      common::Overloaded{
          [&](const Constant& c) -> bool {
            return IsTypeFourState(types, c.type, force_2s);
          },
          [&](mir::PlaceId place_id) -> bool {
            const auto& place = arena[place_id];
            return IsTypeFourState(
                types, mir::TypeOfPlace(types, place), force_2s);
          },
          [&](mir::TempId temp_id) -> bool {
            // Look up the MIR type (source of truth), not the LLVM type
            TypeId type = context.GetTempType(temp_id.value);
            return IsTypeFourState(types, type, force_2s);
          },
      },
      operand.payload);

  if (is_four_state) {
    auto loaded_or_err = LowerOperandRaw(context, operand);
    if (!loaded_or_err) return std::unexpected(loaded_or_err.error());
    return ExtractFourState(builder, *loaded_or_err);
  }

  auto loaded_val_or_err = LowerOperand(context, operand);
  if (!loaded_val_or_err) return std::unexpected(loaded_val_or_err.error());
  llvm::Value* val = *loaded_val_or_err;
  auto* unk = llvm::ConstantInt::get(val->getType(), 0);
  return FourStateValue{.value = val, .unknown = unk};
}

}  // namespace

auto LowerCastRvalue(
    Context& context, const mir::Rvalue& rvalue, TypeId destination_type)
    -> Result<RvalueValue> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const auto& info = std::get<mir::CastRvalueInfo>(rvalue.info);
  const mir::Operand& source_operand = rvalue.operands[0];

  const Type& src_type = types[info.source_type];
  const Type& tgt_type = types[destination_type];

  // Packed -> String conversion: call runtime to extract bytes
  if (IsPacked(src_type) && tgt_type.Kind() == TypeKind::kString) {
    auto source_or_err = LowerOperandRaw(context, source_operand);
    if (!source_or_err) return std::unexpected(source_or_err.error());

    llvm::Value* result = EmitPackedToString(context, *source_or_err, src_type);
    return RvalueValue::TwoState(result);
  }

  // String -> Packed conversion: pack string bytes into integral
  if (src_type.Kind() == TypeKind::kString && IsPacked(tgt_type)) {
    uint32_t bit_width = PackedBitWidth(tgt_type, types);
    llvm::Type* result_type =
        llvm::Type::getIntNTy(context.GetLlvmContext(), bit_width);
    bool tgt_is_4s = context.IsPackedFourState(tgt_type);

    // Try constant-folding for compile-time strings
    const auto* constant = std::get_if<Constant>(&source_operand.payload);
    if (constant != nullptr) {
      const auto* str_const = std::get_if<StringConstant>(&constant->value);
      if (str_const != nullptr) {
        size_t num_bytes = (bit_width + 7) / 8;

        // Pack bytes from string into integral, MSB first
        llvm::APInt packed_val(bit_width, 0);
        const std::string& str = str_const->value;
        for (size_t i = 0; i < str.size() && i < num_bytes; ++i) {
          size_t byte_idx = num_bytes - 1 - i;
          size_t bit_offset = byte_idx * 8;
          auto byte_val = static_cast<uint8_t>(str[i]);
          packed_val |= llvm::APInt(bit_width, byte_val) << bit_offset;
        }
        llvm::Value* result =
            llvm::ConstantInt::get(context.GetLlvmContext(), packed_val);
        if (tgt_is_4s) {
          auto* zero = llvm::ConstantInt::get(result_type, 0);
          return RvalueValue::FourState(result, zero);
        }
        return RvalueValue::TwoState(result);
      }
    }

    // Runtime string: call LyraPackedFromString(handle, out_data, bit_width)
    auto source_or_err = LowerOperand(context, source_operand);
    if (!source_or_err) return std::unexpected(source_or_err.error());
    llvm::Value* source = *source_or_err;

    // Allocate stack space for the packed result
    auto* alloca = builder.CreateAlloca(result_type);

    // Call LyraPackedFromString(handle, out_data, bit_width)
    auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
    builder.CreateCall(
        context.GetLyraPackedFromString(),
        {source, alloca, llvm::ConstantInt::get(i32_ty, bit_width)});

    // Load the result
    llvm::Value* result =
        builder.CreateLoad(result_type, alloca, "str.topacked");
    if (tgt_is_4s) {
      auto* zero = llvm::ConstantInt::get(result_type, 0);
      return RvalueValue::FourState(result, zero);
    }
    return RvalueValue::TwoState(result);
  }

  bool src_is_float = IsRealKind(src_type.Kind());
  bool tgt_is_float = IsRealKind(tgt_type.Kind());
  bool src_is_4s = IsPacked(src_type) && context.IsPackedFourState(src_type);
  bool tgt_is_4s = IsPacked(tgt_type) && context.IsPackedFourState(tgt_type);

  // Float -> Float
  if (src_is_float && tgt_is_float) {
    auto source_or_err = LowerOperand(context, source_operand);
    if (!source_or_err) return std::unexpected(source_or_err.error());
    llvm::Value* source = *source_or_err;

    llvm::Value* result = source;
    if (src_type.Kind() == TypeKind::kReal &&
        tgt_type.Kind() == TypeKind::kShortReal) {
      result = builder.CreateFPTrunc(
          source, llvm::Type::getFloatTy(context.GetLlvmContext()), "fptrunc");
    } else if (
        src_type.Kind() == TypeKind::kShortReal &&
        tgt_type.Kind() == TypeKind::kReal) {
      result = builder.CreateFPExt(
          source, llvm::Type::getDoubleTy(context.GetLlvmContext()), "fpext");
    }
    return RvalueValue::TwoState(result);
  }

  // Int -> Float
  if (!src_is_float && tgt_is_float) {
    auto source_or_err = LowerOperand(context, source_operand);
    if (!source_or_err) return std::unexpected(source_or_err.error());
    llvm::Value* source = *source_or_err;

    llvm::Type* float_ty =
        (tgt_type.Kind() == TypeKind::kReal)
            ? llvm::Type::getDoubleTy(context.GetLlvmContext())
            : llvm::Type::getFloatTy(context.GetLlvmContext());

    bool is_signed = IsSignedIntegral(types, info.source_type);
    llvm::Value* result = nullptr;
    if (is_signed) {
      result = builder.CreateSIToFP(source, float_ty, "sitofp");
    } else {
      result = builder.CreateUIToFP(source, float_ty, "uitofp");
    }
    return RvalueValue::TwoState(result);
  }

  // Float -> Int
  if (src_is_float && !tgt_is_float) {
    auto source_or_err = LowerOperand(context, source_operand);
    if (!source_or_err) return std::unexpected(source_or_err.error());
    llvm::Value* source = *source_or_err;

    uint32_t bit_width = GetTargetBitWidth(types, destination_type);
    llvm::Type* elem_type =
        GetLlvmStorageType(context.GetLlvmContext(), bit_width);

    bool is_signed = IsSignedIntegral(types, destination_type);
    llvm::Value* result = nullptr;
    if (is_signed) {
      result = builder.CreateFPToSI(source, elem_type, "fptosi");
    } else {
      result = builder.CreateFPToUI(source, elem_type, "fptoui");
    }

    result = ApplyWidthMask(context, result, bit_width);

    if (tgt_is_4s) {
      // Float -> 4s: unknown plane is all zeros (value is fully known)
      auto* zero = llvm::ConstantInt::get(elem_type, 0);
      return RvalueValue::FourState(result, zero);
    }
    return RvalueValue::TwoState(result);
  }

  // Int -> Int
  uint32_t bit_width = GetTargetBitWidth(types, destination_type);
  llvm::Type* elem_type =
      GetLlvmStorageType(context.GetLlvmContext(), bit_width);

  bool is_signed = IsSignedIntegral(types, info.source_type);
  uint32_t source_bits = GetSemanticBitWidth(types, info.source_type);

  if (src_is_4s) {
    auto fs_or_err = LoadFourStateOperand(context, source_operand);
    if (!fs_or_err) return std::unexpected(fs_or_err.error());
    auto fs = *fs_or_err;

    fs = ResizeFourStatePlanes(builder, fs, elem_type, is_signed, source_bits);
    fs.value = ApplyWidthMask(context, fs.value, bit_width);
    fs.unknown = ApplyWidthMask(context, fs.unknown, bit_width);

    if (tgt_is_4s) {
      return RvalueValue::FourState(fs.value, fs.unknown);
    }
    return RvalueValue::TwoState(fs.value);
  }

  // 2s source
  auto source_or_err = LowerOperand(context, source_operand);
  if (!source_or_err) return std::unexpected(source_or_err.error());
  llvm::Value* source = *source_or_err;

  llvm::Value* result = ExtOrTrunc(builder, source, elem_type, is_signed);
  result = ApplyWidthMask(context, result, bit_width);

  if (tgt_is_4s) {
    // 2s -> 4s: unknown plane is all zeros (value is fully known)
    auto* zero = llvm::ConstantInt::get(elem_type, 0);
    return RvalueValue::FourState(result, zero);
  }
  return RvalueValue::TwoState(result);
}

auto LowerBitCastRvalue(
    Context& context, const mir::Rvalue& rvalue, TypeId destination_type)
    -> Result<RvalueValue> {
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();
  const auto& info = std::get<mir::BitCastRvalueInfo>(rvalue.info);

  const Type& src_type = types[info.source_type];
  const Type& tgt_type = types[destination_type];

  auto is_packed_integral = [](const Type& type) {
    return type.Kind() == TypeKind::kIntegral ||
           type.Kind() == TypeKind::kPackedArray;
  };

  // String -> packed is handled by Cast (semantic conversion with
  // padding/truncation), not BitCast (bit-level reinterpretation).
  // If we get here with string source, it's a bug in lowering.
  if (src_type.Kind() == TypeKind::kString) {
    throw common::InternalError(
        "LowerBitCastRvalue", "string->packed should use Cast, not BitCast");
  }

  auto& builder = context.GetBuilder();
  auto src_or_err = LowerOperand(context, rvalue.operands[0]);
  if (!src_or_err) return std::unexpected(src_or_err.error());
  llvm::Value* src = *src_or_err;

  llvm::Value* result = nullptr;
  bool tgt_is_4s = IsPacked(tgt_type) && context.IsPackedFourState(tgt_type);

  if (src_type.Kind() == TypeKind::kReal && is_packed_integral(tgt_type)) {
    result =
        builder.CreateBitCast(src, llvm::Type::getInt64Ty(llvm_ctx), "bitcast");
  } else if (
      is_packed_integral(src_type) && tgt_type.Kind() == TypeKind::kReal) {
    result = builder.CreateBitCast(
        src, llvm::Type::getDoubleTy(llvm_ctx), "bitcast");
  } else if (
      src_type.Kind() == TypeKind::kShortReal && is_packed_integral(tgt_type)) {
    result =
        builder.CreateBitCast(src, llvm::Type::getInt32Ty(llvm_ctx), "bitcast");
  } else if (
      is_packed_integral(src_type) && tgt_type.Kind() == TypeKind::kShortReal) {
    result =
        builder.CreateBitCast(src, llvm::Type::getFloatTy(llvm_ctx), "bitcast");
  } else {
    llvm_unreachable("invalid bitcast types");
  }

  if (tgt_is_4s) {
    auto* zero = llvm::ConstantInt::get(result->getType(), 0);
    return RvalueValue::FourState(result, zero);
  }
  return RvalueValue::TwoState(result);
}

auto LowerTimeToTicks64(Context& context, llvm::Value* time_value)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
  auto* double_ty = llvm::Type::getDoubleTy(llvm_ctx);

  llvm::Type* val_type = time_value->getType();

  // Integer path: zext/trunc to i64
  if (val_type->isIntegerTy()) {
    unsigned width = val_type->getIntegerBitWidth();
    if (width < 64) {
      return builder.CreateZExt(time_value, i64_ty, "time.zext");
    }
    if (width > 64) {
      // Truncation for >64 bit integers is acceptable for time values.
      // Time widths >64 are rare; if needed, consider saturating instead.
      return builder.CreateTrunc(time_value, i64_ty, "time.trunc");
    }
    return time_value;
  }

  // Real path: use control flow to avoid FPToUI on invalid inputs (poison
  // avoidance). POISON AVOIDANCE: FPToUI on NaN/Inf/out-of-range produces
  // poison in LLVM IR. We use control flow (branches + PHI) to ensure FPToUI
  // only executes on valid inputs. This mirrors the semantic layer's explicit
  // branching and avoids poison propagation.
  if (val_type->isFloatingPointTy()) {
    llvm::Value* dval = time_value;
    if (val_type->isFloatTy()) {
      dval = builder.CreateFPExt(time_value, double_ty, "time.fpext");
    }

    // Constants
    auto* zero_fp = llvm::ConstantFP::get(double_ty, 0.0);
    auto* max_fp =
        llvm::ConstantFP::get(double_ty, static_cast<double>(UINT64_MAX));
    auto* i64_zero = llvm::ConstantInt::get(i64_ty, 0);
    auto* i64_max = llvm::ConstantInt::get(i64_ty, UINT64_MAX);

    // Get current function and create basic blocks
    llvm::Function* func = builder.GetInsertBlock()->getParent();
    auto* check_over_bb =
        llvm::BasicBlock::Create(llvm_ctx, "time.check_over", func);
    auto* convert_bb = llvm::BasicBlock::Create(llvm_ctx, "time.convert", func);
    auto* merge_bb = llvm::BasicBlock::Create(llvm_ctx, "time.merge", func);

    // Check: NaN || negative (includes -Inf) => return 0
    auto* is_nan = builder.CreateFCmpUNO(dval, dval, "time.isnan");
    auto* is_neg = builder.CreateFCmpOLT(dval, zero_fp, "time.isneg");
    auto* is_nan_or_neg = builder.CreateOr(is_nan, is_neg, "time.invalid_lo");
    // Capture block right before branch to ensure correct PHI predecessor
    auto* nan_neg_bb = builder.GetInsertBlock();
    builder.CreateCondBr(is_nan_or_neg, merge_bb, check_over_bb);

    // check_over_bb: value is valid and >= 0, check if > max (+Inf saturates to
    // MAX)
    builder.SetInsertPoint(check_over_bb);
    auto* is_over = builder.CreateFCmpOGT(dval, max_fp, "time.isover");
    builder.CreateCondBr(is_over, merge_bb, convert_bb);

    // convert_bb: value is in valid range [0, UINT64_MAX], safe to FPToUI
    builder.SetInsertPoint(convert_bb);
    auto* converted = builder.CreateFPToUI(dval, i64_ty, "time.fptoui");
    builder.CreateBr(merge_bb);

    // merge_bb: PHI to select result
    builder.SetInsertPoint(merge_bb);
    auto* phi = builder.CreatePHI(i64_ty, 3, "time.result");
    phi->addIncoming(i64_zero, nan_neg_bb);    // NaN or negative => 0
    phi->addIncoming(i64_max, check_over_bb);  // over max (+Inf) => UINT64_MAX
    phi->addIncoming(converted, convert_bb);   // in-range => FPToUI result

    return phi;
  }

  throw common::InternalError(
      "LowerTimeToTicks64", "expected integer or real time value type");
}

}  // namespace lyra::lowering::mir_to_llvm
