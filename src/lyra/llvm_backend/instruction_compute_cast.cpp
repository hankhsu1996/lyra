#include "lyra/llvm_backend/instruction_compute_cast.hpp"

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
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/four_state_utils.hpp"
#include "lyra/llvm_backend/instruction_compute_ops.hpp"
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

auto IsSignedIntegral(const TypeArena& types, TypeId type_id) -> bool {
  const Type& type = types[type_id];
  if (!IsPacked(type)) {
    return false;
  }
  return IsPackedSigned(type, types);
}

// Sign-extend from semantic bit width (not storage width).
// For a signed N-bit value stored in M-bit storage (M >= N), the sign bit is at
// position N-1, not M-1. This function sign-extends from the semantic width.
auto SignExtendFromSemanticWidth(
    llvm::IRBuilderBase& builder, llvm::Value* val, llvm::Type* dst_type,
    uint32_t semantic_bits) -> llvm::Value* {
  auto dst_bits = dst_type->getIntegerBitWidth();
  // First zero-extend to target width
  llvm::Value* extended = builder.CreateZExtOrTrunc(val, dst_type);
  if (semantic_bits == 0 || semantic_bits >= dst_bits) {
    // No sign extension needed
    return extended;
  }
  // Shift left to put sign bit at MSB, then arithmetic shift right to replicate
  uint32_t shift_amount = dst_bits - semantic_bits;
  auto* shift_const = llvm::ConstantInt::get(dst_type, shift_amount);
  llvm::Value* shifted_left = builder.CreateShl(extended, shift_const);
  return builder.CreateAShr(shifted_left, shift_const);
}

// Extend or truncate using storage-width signedness (legacy helper).
auto ExtOrTrunc(
    llvm::IRBuilderBase& builder, llvm::Value* val, llvm::Type* dst_type,
    bool is_signed) -> llvm::Value* {
  if (is_signed) {
    return builder.CreateSExtOrTrunc(val, dst_type, "cast.sext");
  }
  return builder.CreateZExtOrTrunc(val, dst_type, "cast.zext");
}

// Extend or truncate a value to destination type.
// For signed values, uses semantic_bits to determine the sign bit position.
// If semantic_bits is 0, falls back to storage-width sign extension.
auto ExtOrTruncSemantic(
    llvm::IRBuilderBase& builder, llvm::Value* val, llvm::Type* dst_type,
    bool is_signed, uint32_t semantic_bits) -> llvm::Value* {
  if (is_signed && semantic_bits > 0) {
    return SignExtendFromSemanticWidth(builder, val, dst_type, semantic_bits);
  }
  if (is_signed) {
    return builder.CreateSExtOrTrunc(val, dst_type, "cast.sext");
  }
  return builder.CreateZExtOrTrunc(val, dst_type, "cast.zext");
}

// Resize 4-state planes to target element type.
// INVARIANT: Value plane uses source signedness from semantic width;
// unknown plane ALWAYS zero-extends (it's a mask, signedness doesn't apply).
auto ResizeFourStatePlanes(
    llvm::IRBuilderBase& builder, FourStateValue fs, llvm::Type* dst_type,
    bool is_signed, uint32_t source_semantic_bits) -> FourStateValue {
  llvm::Value* val =
      is_signed ? SignExtendFromSemanticWidth(
                      builder, fs.value, dst_type, source_semantic_bits)
                : builder.CreateZExtOrTrunc(fs.value, dst_type);
  // Unknown plane: ALWAYS zero-extend (mask semantics, never sign-extend)
  llvm::Value* unk = builder.CreateZExtOrTrunc(fs.unknown, dst_type);
  return FourStateValue{.value = val, .unknown = unk};
}

// Get the semantic bit width of a packed type.
// Returns 0 for non-packed types (e.g., real/shortreal).
auto GetSemanticBitWidth(const TypeArena& types, TypeId type_id) -> uint32_t {
  const Type& type = types[type_id];
  if (!IsPacked(type)) {
    return 0;
  }
  return PackedBitWidth(type, types);
}

// Get the semantic bit width of a packed target place.
// Returns 0 for non-packed types (e.g., real/shortreal).
auto GetTargetBitWidth(Context& context, mir::PlaceId target) -> uint32_t {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  const auto& place = arena[target];
  TypeId type_id = mir::TypeOfPlace(types, place);
  return GetSemanticBitWidth(types, type_id);
}

// Load a 4-state operand at its natural width (no resizing).
// Returns FourStateValue with planes at source storage width.
auto LoadFourStateOperand(Context& context, const mir::Operand& operand)
    -> Result<FourStateValue> {
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  // Check if the operand is 4-state
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

  // 2-state operand -> wrap as 4-state (unknown = 0, no resize)
  auto loaded_val_or_err = LowerOperand(context, operand);
  if (!loaded_val_or_err) return std::unexpected(loaded_val_or_err.error());
  llvm::Value* val = *loaded_val_or_err;
  auto* unk = llvm::ConstantInt::get(val->getType(), 0);
  return FourStateValue{.value = val, .unknown = unk};
}

// Cast 2-state int -> 2-state int
auto LowerCast2sTo2s(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  auto source_or_err = LowerOperand(context, source_operand);
  if (!source_or_err) return std::unexpected(source_or_err.error());
  llvm::Value* source = *source_or_err;

  auto target_type_or_err = context.GetPlaceLlvmType(target);
  if (!target_type_or_err) return std::unexpected(target_type_or_err.error());
  llvm::Type* target_type = *target_type_or_err;

  bool is_signed = IsSignedIntegral(types, info.source_type);

  llvm::Value* result = ExtOrTrunc(builder, source, target_type, is_signed);

  // Apply width mask to handle semantic width < storage width
  uint32_t bit_width = GetTargetBitWidth(context, target);
  result = ApplyWidthMask(context, result, bit_width);

  auto target_ptr_or_err = context.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  builder.CreateStore(result, target_ptr);
  return {};
}

// Cast 2-state int -> 4-state int
auto LowerCast2sTo4s(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  auto source_or_err = LowerOperand(context, source_operand);
  if (!source_or_err) return std::unexpected(source_or_err.error());
  llvm::Value* source = *source_or_err;

  auto storage_type_or_err = context.GetPlaceLlvmType(target);
  if (!storage_type_or_err) return std::unexpected(storage_type_or_err.error());
  llvm::Type* storage_type = *storage_type_or_err;

  auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
  auto* elem_type = GetFourStateElemIntType(struct_type);

  bool is_signed = IsSignedIntegral(types, info.source_type);
  llvm::Value* val = ExtOrTrunc(builder, source, elem_type, is_signed);

  // Apply width mask to value plane
  uint32_t bit_width = GetTargetBitWidth(context, target);
  val = ApplyWidthMask(context, val, bit_width);

  llvm::Value* result = MakeKnown(builder, struct_type, val);

  auto target_ptr_or_err = context.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  builder.CreateStore(result, target_ptr);
  return {};
}

// Cast 4-state int -> 2-state int
auto LowerCast4sTo2s(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  auto source_raw_or_err = LowerOperandRaw(context, source_operand);
  if (!source_raw_or_err) return std::unexpected(source_raw_or_err.error());
  llvm::Value* source_raw = *source_raw_or_err;

  auto fs = ExtractFourState(builder, source_raw);

  auto target_type_or_err = context.GetPlaceLlvmType(target);
  if (!target_type_or_err) return std::unexpected(target_type_or_err.error());
  llvm::Type* target_type = *target_type_or_err;

  // Use source semantic width for sign extension
  bool is_signed = IsSignedIntegral(types, info.source_type);
  uint32_t source_bits = GetSemanticBitWidth(types, info.source_type);
  llvm::Value* result = ExtOrTruncSemantic(
      builder, fs.value, target_type, is_signed, source_bits);

  // Apply width mask to handle semantic width < storage width
  uint32_t bit_width = GetTargetBitWidth(context, target);
  result = ApplyWidthMask(context, result, bit_width);

  auto target_ptr_or_err = context.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  builder.CreateStore(result, target_ptr);
  return {};
}

// Cast 4-state int -> 4-state int
auto LowerCast4sTo4s(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  auto storage_type_or_err = context.GetPlaceLlvmType(target);
  if (!storage_type_or_err) return std::unexpected(storage_type_or_err.error());
  llvm::Type* storage_type = *storage_type_or_err;

  auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
  auto* elem_type = GetFourStateElemIntType(struct_type);

  // Load at natural width
  auto fs_or_err = LoadFourStateOperand(context, source_operand);
  if (!fs_or_err) return std::unexpected(fs_or_err.error());
  auto fs = *fs_or_err;

  // Resize to target element type using SOURCE signedness and semantic width
  bool is_signed = IsSignedIntegral(types, info.source_type);
  uint32_t source_bits = GetSemanticBitWidth(types, info.source_type);
  fs = ResizeFourStatePlanes(builder, fs, elem_type, is_signed, source_bits);

  // Apply width mask to both planes
  uint32_t bit_width = GetTargetBitWidth(context, target);
  fs.value = ApplyWidthMask(context, fs.value, bit_width);
  fs.unknown = ApplyWidthMask(context, fs.unknown, bit_width);

  llvm::Value* result =
      PackFourState(builder, struct_type, fs.value, fs.unknown);

  auto target_ptr_or_err = context.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  builder.CreateStore(result, target_ptr);
  return {};
}

// Cast int -> float (2-state source)
auto LowerCastIntToFloat(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  auto source_or_err = LowerOperand(context, source_operand);
  if (!source_or_err) return std::unexpected(source_or_err.error());
  llvm::Value* source = *source_or_err;

  const Type& tgt_type = types[info.target_type];

  llvm::Type* float_ty = (tgt_type.Kind() == TypeKind::kReal)
                             ? llvm::Type::getDoubleTy(context.GetLlvmContext())
                             : llvm::Type::getFloatTy(context.GetLlvmContext());

  bool is_signed = IsSignedIntegral(types, info.source_type);
  llvm::Value* result = nullptr;
  if (is_signed) {
    result = builder.CreateSIToFP(source, float_ty, "sitofp");
  } else {
    result = builder.CreateUIToFP(source, float_ty, "uitofp");
  }

  auto target_ptr_or_err = context.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  builder.CreateStore(result, target_ptr);
  return {};
}

// Cast float -> 2-state int
auto LowerCastFloatTo2sInt(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  auto source_or_err = LowerOperand(context, source_operand);
  if (!source_or_err) return std::unexpected(source_or_err.error());
  llvm::Value* source = *source_or_err;

  auto target_type_or_err = context.GetPlaceLlvmType(target);
  if (!target_type_or_err) return std::unexpected(target_type_or_err.error());
  llvm::Type* target_type = *target_type_or_err;

  bool is_signed = IsSignedIntegral(types, info.target_type);
  llvm::Value* result = nullptr;
  if (is_signed) {
    result = builder.CreateFPToSI(source, target_type, "fptosi");
  } else {
    result = builder.CreateFPToUI(source, target_type, "fptoui");
  }

  // Apply width mask to handle semantic width < storage width
  uint32_t bit_width = GetTargetBitWidth(context, target);
  result = ApplyWidthMask(context, result, bit_width);

  auto target_ptr_or_err = context.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  builder.CreateStore(result, target_ptr);
  return {};
}

// Cast float -> 4-state int (THE BUG FIX)
// Previously this would try to FPToSI to the struct type, crashing.
// Now we FPToSI to the element type, then wrap with MakeKnown.
auto LowerCastFloatTo4sInt(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  auto source_or_err = LowerOperand(context, source_operand);
  if (!source_or_err) return std::unexpected(source_or_err.error());
  llvm::Value* source = *source_or_err;

  auto storage_type_or_err = context.GetPlaceLlvmType(target);
  if (!storage_type_or_err) return std::unexpected(storage_type_or_err.error());
  llvm::Type* storage_type = *storage_type_or_err;

  auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
  auto* elem_type = GetFourStateElemIntType(struct_type);

  bool is_signed = IsSignedIntegral(types, info.target_type);
  llvm::Value* int_val = nullptr;
  if (is_signed) {
    int_val = builder.CreateFPToSI(source, elem_type, "fptosi");
  } else {
    int_val = builder.CreateFPToUI(source, elem_type, "fptoui");
  }

  // Apply width mask to value
  uint32_t bit_width = GetTargetBitWidth(context, target);
  int_val = ApplyWidthMask(context, int_val, bit_width);

  llvm::Value* result = MakeKnown(builder, struct_type, int_val);

  auto target_ptr_or_err = context.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  builder.CreateStore(result, target_ptr);
  return {};
}

// Cast float -> float (fptrunc or fpext)
auto LowerCastFloatToFloat(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  auto source_or_err = LowerOperand(context, source_operand);
  if (!source_or_err) return std::unexpected(source_or_err.error());
  llvm::Value* source = *source_or_err;

  const Type& src_type = types[info.source_type];
  const Type& tgt_type = types[info.target_type];

  auto target_ptr_or_err = context.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  llvm::Value* result = nullptr;

  // real -> shortreal (fptrunc)
  if (src_type.Kind() == TypeKind::kReal &&
      tgt_type.Kind() == TypeKind::kShortReal) {
    result = builder.CreateFPTrunc(
        source, llvm::Type::getFloatTy(context.GetLlvmContext()), "fptrunc");
    builder.CreateStore(result, target_ptr);
    return {};
  }

  // shortreal -> real (fpext)
  if (src_type.Kind() == TypeKind::kShortReal &&
      tgt_type.Kind() == TypeKind::kReal) {
    result = builder.CreateFPExt(
        source, llvm::Type::getDoubleTy(context.GetLlvmContext()), "fpext");
    builder.CreateStore(result, target_ptr);
    return {};
  }

  // Same type -> no-op
  builder.CreateStore(source, target_ptr);
  return {};
}

}  // namespace

auto LowerCastUnified(Context& context, const mir::Compute& compute)
    -> Result<void> {
  const auto& types = context.GetTypeArena();
  const auto& info = std::get<mir::CastRvalueInfo>(compute.value.info);
  const mir::Operand& source_operand = compute.value.operands[0];

  const Type& src_type = types[info.source_type];
  const Type& tgt_type = types[info.target_type];

  if (tgt_type.Kind() == TypeKind::kString ||
      src_type.Kind() == TypeKind::kString) {
    throw common::InternalError(
        "LowerCastUnified",
        std::format(
            "string casts not supported: {} -> {}", ToString(src_type.Kind()),
            ToString(tgt_type.Kind())));
  }

  bool src_is_float = IsRealKind(src_type.Kind());
  bool tgt_is_float = IsRealKind(tgt_type.Kind());
  bool src_is_4s = IsPacked(src_type) && IsPackedFourState(src_type, types);
  bool tgt_is_4s = IsPacked(tgt_type) && IsPackedFourState(tgt_type, types);

  // Float -> Float
  if (src_is_float && tgt_is_float) {
    return LowerCastFloatToFloat(context, info, source_operand, compute.target);
  }

  // Int -> Float
  if (!src_is_float && tgt_is_float) {
    // For 4-state source, LowerOperand extracts the value plane
    return LowerCastIntToFloat(context, info, source_operand, compute.target);
  }

  // Float -> Int
  if (src_is_float && !tgt_is_float) {
    if (tgt_is_4s) {
      return LowerCastFloatTo4sInt(
          context, info, source_operand, compute.target);
    }
    return LowerCastFloatTo2sInt(context, info, source_operand, compute.target);
  }

  // Int -> Int
  if (src_is_4s && tgt_is_4s) {
    return LowerCast4sTo4s(context, info, source_operand, compute.target);
  }
  if (src_is_4s && !tgt_is_4s) {
    return LowerCast4sTo2s(context, info, source_operand, compute.target);
  }
  if (!src_is_4s && tgt_is_4s) {
    return LowerCast2sTo4s(context, info, source_operand, compute.target);
  }
  return LowerCast2sTo2s(context, info, source_operand, compute.target);
}

auto LowerBitCastUnified(Context& context, const mir::Compute& compute)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();
  const auto& info = std::get<mir::BitCastRvalueInfo>(compute.value.info);

  auto src_or_err = LowerOperand(context, compute.value.operands[0]);
  if (!src_or_err) return std::unexpected(src_or_err.error());
  llvm::Value* src = *src_or_err;

  const Type& src_type = types[info.source_type];
  const Type& tgt_type = types[info.target_type];

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

  auto target_ptr_or_err = context.GetPlacePointer(compute.target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  builder.CreateStore(result, target_ptr);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
