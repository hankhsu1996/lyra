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
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/compute/ops.hpp"
#include "lyra/llvm_backend/context.hpp"
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

auto GetTargetBitWidth(Context& context, mir::PlaceId target) -> uint32_t {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  const auto& place = arena[target];
  TypeId type_id = mir::TypeOfPlace(types, place);
  return GetSemanticBitWidth(types, type_id);
}

auto LoadFourStateOperand(Context& context, const mir::Operand& operand)
    -> Result<FourStateValue> {
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  bool is_four_state = std::visit(
      [&](const auto& payload) -> bool {
        using T = std::decay_t<decltype(payload)>;
        if constexpr (std::is_same_v<T, Constant>) {
          return IsTypeFourState(types, payload.type);
        } else {
          const auto& place = arena[payload];
          return IsTypeFourState(types, mir::TypeOfPlace(types, place));
        }
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
    Context& context, const mir::Compute& compute, llvm::Value** unknown_out)
    -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const auto& info = std::get<mir::CastRvalueInfo>(compute.value.info);
  const mir::Operand& source_operand = compute.value.operands[0];

  const Type& src_type = types[info.source_type];
  const Type& tgt_type = types[info.target_type];

  if (tgt_type.Kind() == TypeKind::kString ||
      src_type.Kind() == TypeKind::kString) {
    throw common::InternalError(
        "LowerCastRvalue",
        std::format(
            "string casts not supported: {} -> {}", ToString(src_type.Kind()),
            ToString(tgt_type.Kind())));
  }

  bool src_is_float = IsRealKind(src_type.Kind());
  bool tgt_is_float = IsRealKind(tgt_type.Kind());
  bool src_is_4s = IsPacked(src_type) && IsPackedFourState(src_type, types);
  bool tgt_is_4s = IsPacked(tgt_type) && IsPackedFourState(tgt_type, types);

  // Initialize unknown_out
  if (unknown_out != nullptr) {
    *unknown_out = nullptr;
  }

  // Float -> Float
  if (src_is_float && tgt_is_float) {
    auto source_or_err = LowerOperand(context, source_operand);
    if (!source_or_err) return std::unexpected(source_or_err.error());
    llvm::Value* source = *source_or_err;

    if (src_type.Kind() == TypeKind::kReal &&
        tgt_type.Kind() == TypeKind::kShortReal) {
      return builder.CreateFPTrunc(
          source, llvm::Type::getFloatTy(context.GetLlvmContext()), "fptrunc");
    }
    if (src_type.Kind() == TypeKind::kShortReal &&
        tgt_type.Kind() == TypeKind::kReal) {
      return builder.CreateFPExt(
          source, llvm::Type::getDoubleTy(context.GetLlvmContext()), "fpext");
    }
    return source;
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
    if (is_signed) {
      return builder.CreateSIToFP(source, float_ty, "sitofp");
    }
    return builder.CreateUIToFP(source, float_ty, "uitofp");
  }

  // Float -> Int
  if (src_is_float && !tgt_is_float) {
    auto source_or_err = LowerOperand(context, source_operand);
    if (!source_or_err) return std::unexpected(source_or_err.error());
    llvm::Value* source = *source_or_err;

    auto target_type_or_err = context.GetPlaceLlvmType(compute.target);
    if (!target_type_or_err) return std::unexpected(target_type_or_err.error());
    llvm::Type* target_llvm_type = *target_type_or_err;

    llvm::Type* elem_type = target_llvm_type;
    if (tgt_is_4s) {
      auto* struct_type = llvm::cast<llvm::StructType>(target_llvm_type);
      elem_type = GetFourStateElemIntType(struct_type);
    }

    bool is_signed = IsSignedIntegral(types, info.target_type);
    llvm::Value* result = nullptr;
    if (is_signed) {
      result = builder.CreateFPToSI(source, elem_type, "fptosi");
    } else {
      result = builder.CreateFPToUI(source, elem_type, "fptoui");
    }

    uint32_t bit_width = GetTargetBitWidth(context, compute.target);
    result = ApplyWidthMask(context, result, bit_width);
    return result;
  }

  // Int -> Int
  auto target_type_or_err = context.GetPlaceLlvmType(compute.target);
  if (!target_type_or_err) return std::unexpected(target_type_or_err.error());
  llvm::Type* target_llvm_type = *target_type_or_err;

  llvm::Type* elem_type = target_llvm_type;
  if (tgt_is_4s) {
    auto* struct_type = llvm::cast<llvm::StructType>(target_llvm_type);
    elem_type = GetFourStateElemIntType(struct_type);
  }

  bool is_signed = IsSignedIntegral(types, info.source_type);
  uint32_t source_bits = GetSemanticBitWidth(types, info.source_type);
  uint32_t bit_width = GetTargetBitWidth(context, compute.target);

  if (src_is_4s) {
    auto fs_or_err = LoadFourStateOperand(context, source_operand);
    if (!fs_or_err) return std::unexpected(fs_or_err.error());
    auto fs = *fs_or_err;

    fs = ResizeFourStatePlanes(builder, fs, elem_type, is_signed, source_bits);
    fs.value = ApplyWidthMask(context, fs.value, bit_width);
    fs.unknown = ApplyWidthMask(context, fs.unknown, bit_width);

    if (tgt_is_4s && unknown_out != nullptr) {
      *unknown_out = fs.unknown;
    }
    return fs.value;
  }

  // 2s source
  auto source_or_err = LowerOperand(context, source_operand);
  if (!source_or_err) return std::unexpected(source_or_err.error());
  llvm::Value* source = *source_or_err;

  llvm::Value* result = ExtOrTrunc(builder, source, elem_type, is_signed);
  result = ApplyWidthMask(context, result, bit_width);
  return result;
}

auto LowerBitCastRvalue(
    Context& context, const mir::Compute& compute, llvm::Value** unknown_out)
    -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();
  const auto& info = std::get<mir::BitCastRvalueInfo>(compute.value.info);

  auto src_or_err = LowerOperand(context, compute.value.operands[0]);
  if (!src_or_err) return std::unexpected(src_or_err.error());
  llvm::Value* src = *src_or_err;

  const Type& src_type = types[info.source_type];
  const Type& tgt_type = types[info.target_type];

  if (unknown_out != nullptr) {
    *unknown_out = nullptr;
  }

  auto is_packed_integral = [](const Type& type) {
    return type.Kind() == TypeKind::kIntegral ||
           type.Kind() == TypeKind::kPackedArray;
  };

  llvm::Value* result = nullptr;

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

  return result;
}

}  // namespace lyra::lowering::mir_to_llvm
