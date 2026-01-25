#include "lyra/llvm_backend/instruction_compute_cast.hpp"

#include <cstdint>
#include <format>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/ErrorHandling.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/four_state_utils.hpp"
#include "lyra/llvm_backend/instruction_compute_ops.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
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

auto ExtOrTrunc(
    llvm::IRBuilderBase& builder, llvm::Value* val, llvm::Type* dst_type,
    bool is_signed) -> llvm::Value* {
  if (is_signed) {
    return builder.CreateSExtOrTrunc(val, dst_type, "cast.sext");
  }
  return builder.CreateZExtOrTrunc(val, dst_type, "cast.zext");
}

// Get the semantic bit width of a packed target place.
// Returns 0 for non-packed types (e.g., real/shortreal).
auto GetTargetBitWidth(Context& context, mir::PlaceId target) -> uint32_t {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  const auto& place = arena[target];
  TypeId type_id = mir::TypeOfPlace(types, place);
  const Type& type = types[type_id];

  if (!IsPacked(type)) {
    return 0;
  }
  return PackedBitWidth(type, types);
}

// Load a 4-state operand, returning FourStateValue with planes adjusted to
// target element type.
auto LoadFourStateOperand(
    Context& context, const mir::Operand& operand, llvm::Type* elem_type)
    -> FourStateValue {
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
    llvm::Value* loaded = LowerOperandRaw(context, operand);
    auto fs = ExtractFourState(builder, loaded);
    return {
        .value = builder.CreateZExtOrTrunc(fs.value, elem_type, "cast4.val"),
        .unknown =
            builder.CreateZExtOrTrunc(fs.unknown, elem_type, "cast4.unk"),
    };
  }

  // 2-state operand -> extend to 4-state element type, unknown = 0
  llvm::Value* loaded_val = LowerOperand(context, operand);
  auto* val = builder.CreateZExtOrTrunc(loaded_val, elem_type, "cast2to4.val");
  auto* unk = llvm::ConstantInt::get(elem_type, 0);
  return {.value = val, .unknown = unk};
}

// Cast 2-state int -> 2-state int
void LowerCast2sTo2s(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  llvm::Value* source = LowerOperand(context, source_operand);
  llvm::Type* target_type = context.GetPlaceLlvmType(target);
  bool is_signed = IsSignedIntegral(types, info.source_type);

  llvm::Value* result = ExtOrTrunc(builder, source, target_type, is_signed);

  // Apply width mask to handle semantic width < storage width
  uint32_t bit_width = GetTargetBitWidth(context, target);
  result = ApplyWidthMask(context, result, bit_width);

  llvm::Value* target_ptr = context.GetPlacePointer(target);
  builder.CreateStore(result, target_ptr);
}

// Cast 2-state int -> 4-state int
void LowerCast2sTo4s(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  llvm::Value* source = LowerOperand(context, source_operand);
  llvm::Type* storage_type = context.GetPlaceLlvmType(target);
  auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
  auto* elem_type = GetFourStateElemIntType(struct_type);

  bool is_signed = IsSignedIntegral(types, info.source_type);
  llvm::Value* val = ExtOrTrunc(builder, source, elem_type, is_signed);

  // Apply width mask to value plane
  uint32_t bit_width = GetTargetBitWidth(context, target);
  val = ApplyWidthMask(context, val, bit_width);

  llvm::Value* result = MakeKnown(builder, struct_type, val);

  llvm::Value* target_ptr = context.GetPlacePointer(target);
  builder.CreateStore(result, target_ptr);
}

// Cast 4-state int -> 2-state int
void LowerCast4sTo2s(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  llvm::Value* source_raw = LowerOperandRaw(context, source_operand);
  auto fs = ExtractFourState(builder, source_raw);
  llvm::Type* target_type = context.GetPlaceLlvmType(target);

  bool is_signed = IsSignedIntegral(types, info.source_type);
  llvm::Value* result = ExtOrTrunc(builder, fs.value, target_type, is_signed);

  // Apply width mask to handle semantic width < storage width
  uint32_t bit_width = GetTargetBitWidth(context, target);
  result = ApplyWidthMask(context, result, bit_width);

  llvm::Value* target_ptr = context.GetPlacePointer(target);
  builder.CreateStore(result, target_ptr);
}

// Cast 4-state int -> 4-state int
void LowerCast4sTo4s(
    Context& context, const mir::CastRvalueInfo& /*info*/,
    const mir::Operand& source_operand, mir::PlaceId target) {
  auto& builder = context.GetBuilder();

  llvm::Type* storage_type = context.GetPlaceLlvmType(target);
  auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
  auto* elem_type = GetFourStateElemIntType(struct_type);

  auto fs = LoadFourStateOperand(context, source_operand, elem_type);

  // Apply width mask to both planes
  uint32_t bit_width = GetTargetBitWidth(context, target);
  fs.value = ApplyWidthMask(context, fs.value, bit_width);
  fs.unknown = ApplyWidthMask(context, fs.unknown, bit_width);

  llvm::Value* result =
      PackFourState(builder, struct_type, fs.value, fs.unknown);

  llvm::Value* target_ptr = context.GetPlacePointer(target);
  builder.CreateStore(result, target_ptr);
}

// Cast int -> float (2-state source)
void LowerCastIntToFloat(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  llvm::Value* source = LowerOperand(context, source_operand);
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

  llvm::Value* target_ptr = context.GetPlacePointer(target);
  builder.CreateStore(result, target_ptr);
}

// Cast float -> 2-state int
void LowerCastFloatTo2sInt(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  llvm::Value* source = LowerOperand(context, source_operand);
  llvm::Type* target_type = context.GetPlaceLlvmType(target);

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

  llvm::Value* target_ptr = context.GetPlacePointer(target);
  builder.CreateStore(result, target_ptr);
}

// Cast float -> 4-state int (THE BUG FIX)
// Previously this would try to FPToSI to the struct type, crashing.
// Now we FPToSI to the element type, then wrap with MakeKnown.
void LowerCastFloatTo4sInt(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  llvm::Value* source = LowerOperand(context, source_operand);
  llvm::Type* storage_type = context.GetPlaceLlvmType(target);
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

  llvm::Value* target_ptr = context.GetPlacePointer(target);
  builder.CreateStore(result, target_ptr);
}

// Cast float -> float (fptrunc or fpext)
void LowerCastFloatToFloat(
    Context& context, const mir::CastRvalueInfo& info,
    const mir::Operand& source_operand, mir::PlaceId target) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  llvm::Value* source = LowerOperand(context, source_operand);
  const Type& src_type = types[info.source_type];
  const Type& tgt_type = types[info.target_type];

  llvm::Value* result = nullptr;
  llvm::Value* target_ptr = context.GetPlacePointer(target);

  // real -> shortreal (fptrunc)
  if (src_type.Kind() == TypeKind::kReal &&
      tgt_type.Kind() == TypeKind::kShortReal) {
    result = builder.CreateFPTrunc(
        source, llvm::Type::getFloatTy(context.GetLlvmContext()), "fptrunc");
    builder.CreateStore(result, target_ptr);
    return;
  }

  // shortreal -> real (fpext)
  if (src_type.Kind() == TypeKind::kShortReal &&
      tgt_type.Kind() == TypeKind::kReal) {
    result = builder.CreateFPExt(
        source, llvm::Type::getDoubleTy(context.GetLlvmContext()), "fpext");
    builder.CreateStore(result, target_ptr);
    return;
  }

  // Same type -> no-op
  builder.CreateStore(source, target_ptr);
}

}  // namespace

void LowerCastUnified(Context& context, const mir::Compute& compute) {
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
    LowerCastFloatToFloat(context, info, source_operand, compute.target);
    return;
  }

  // Int -> Float
  if (!src_is_float && tgt_is_float) {
    // For 4-state source, LowerOperand extracts the value plane
    LowerCastIntToFloat(context, info, source_operand, compute.target);
    return;
  }

  // Float -> Int
  if (src_is_float && !tgt_is_float) {
    if (tgt_is_4s) {
      LowerCastFloatTo4sInt(context, info, source_operand, compute.target);
    } else {
      LowerCastFloatTo2sInt(context, info, source_operand, compute.target);
    }
    return;
  }

  // Int -> Int
  if (src_is_4s && tgt_is_4s) {
    LowerCast4sTo4s(context, info, source_operand, compute.target);
  } else if (src_is_4s && !tgt_is_4s) {
    LowerCast4sTo2s(context, info, source_operand, compute.target);
  } else if (!src_is_4s && tgt_is_4s) {
    LowerCast2sTo4s(context, info, source_operand, compute.target);
  } else {
    LowerCast2sTo2s(context, info, source_operand, compute.target);
  }
}

void LowerBitCastUnified(Context& context, const mir::Compute& compute) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();
  const auto& info = std::get<mir::BitCastRvalueInfo>(compute.value.info);

  llvm::Value* src = LowerOperand(context, compute.value.operands[0]);
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

  llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
  builder.CreateStore(result, target_ptr);
}

}  // namespace lyra::lowering::mir_to_llvm
