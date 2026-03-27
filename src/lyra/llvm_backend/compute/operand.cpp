#include "lyra/llvm_backend/compute/operand.hpp"

#include <cstdint>
#include <expected>
#include <utility>
#include <variant>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/four_state.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/packed_storage_view.hpp"
#include "lyra/llvm_backend/slot_access.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto LoadBitRange(Context& context, mir::PlaceId place_id)
    -> Result<llvm::Value*> {
  auto elem_type_result = context.GetPlaceLlvmType(place_id);
  if (!elem_type_result) return std::unexpected(elem_type_result.error());
  return LoadPackedPlace(context, place_id, *elem_type_result);
}

}  // namespace

auto LowerOperandRaw(Context& context, const mir::Operand& operand)
    -> Result<llvm::Value*> {
  CanonicalSlotAccess canonical(context);
  return LowerOperandRaw(context, canonical, operand);
}

auto LowerOperand(Context& context, const mir::Operand& operand)
    -> Result<llvm::Value*> {
  CanonicalSlotAccess canonical(context);
  return LowerOperand(context, canonical, operand);
}

auto LowerOperandAsStorage(
    Context& context, const mir::Operand& operand, llvm::Type* target_type)
    -> Result<llvm::Value*> {
  auto val_or_err = LowerOperandRaw(context, operand);
  if (!val_or_err) return std::unexpected(val_or_err.error());
  llvm::Value* val = *val_or_err;

  if (val->getType() == target_type) {
    return val;
  }

  auto& builder = context.GetBuilder();

  // 2-state integer -> 4-state struct: wrap as {zext(value), 0}
  if (target_type->isStructTy() && val->getType()->isIntegerTy()) {
    auto* struct_type = llvm::cast<llvm::StructType>(target_type);
    auto* elem_type = struct_type->getElementType(0);
    llvm::Value* coerced =
        builder.CreateZExtOrTrunc(val, elem_type, "stor.val");
    llvm::Value* packed = llvm::UndefValue::get(struct_type);
    packed = builder.CreateInsertValue(packed, coerced, 0);
    packed = builder.CreateInsertValue(
        packed, llvm::ConstantInt::get(elem_type, 0), 1);
    return packed;
  }

  // 4-state struct -> 4-state struct (different storage width): widen/narrow
  // both planes
  if (target_type->isStructTy() && val->getType()->isStructTy()) {
    auto* target_struct = llvm::cast<llvm::StructType>(target_type);
    auto* target_elem = target_struct->getElementType(0);
    llvm::Value* src_val = builder.CreateExtractValue(val, 0, "stor.src.val");
    llvm::Value* src_unk = builder.CreateExtractValue(val, 1, "stor.src.unk");
    src_val = builder.CreateZExtOrTrunc(src_val, target_elem, "stor.val.fit");
    src_unk = builder.CreateZExtOrTrunc(src_unk, target_elem, "stor.unk.fit");
    llvm::Value* packed = llvm::UndefValue::get(target_struct);
    packed = builder.CreateInsertValue(packed, src_val, 0);
    packed = builder.CreateInsertValue(packed, src_unk, 1);
    return packed;
  }

  // 2-state integer width coercion (storage rounding: e.g. i4 -> i8)
  if (val->getType()->isIntegerTy() && target_type->isIntegerTy()) {
    return builder.CreateZExtOrTrunc(val, target_type, "stor.ext");
  }

  throw common::InternalError(
      "LowerOperandAsStorage",
      "unsupported storage coercion: source and target types incompatible");
}

auto LowerConstant(Context& context, const Constant& constant)
    -> Result<llvm::Value*> {
  auto& llvm_ctx = context.GetLlvmContext();

  return std::visit(
      common::Overloaded{
          [&](const IntegralConstant& integral) -> Result<llvm::Value*> {
            // Get semantic bit width from type
            const Type& type = context.GetTypeArena()[constant.type];
            uint32_t bit_width = PackedBitWidth(type, context.GetTypeArena());

            // Create APInt from word arrays (little-endian in both MIR and
            // LLVM)
            llvm::APInt value_ap(bit_width, integral.value);

            // 4-state type: always create {value, unknown} struct
            // Type determines LLVM shape, not whether value has unknown bits.
            // Uses the same canonical predicate as IsOperandFourState.
            if (IsPacked(type) && context.IsPackedFourState(type)) {
              llvm::APInt unknown_ap =
                  integral.IsKnown() ? llvm::APInt::getZero(bit_width)
                                     : llvm::APInt(bit_width, integral.unknown);
              FourStatePair pair{
                  .value = std::move(value_ap),
                  .unknown = std::move(unknown_ap)};
              MaskFourState(pair, bit_width);

              // Get the struct type {iN_storage, iN_storage}
              auto* struct_type = llvm::cast<llvm::StructType>(
                  context.GetPlaceLlvmType4State(bit_width));
              auto* elem_type = struct_type->getElementType(0);
              uint32_t storage_width = elem_type->getIntegerBitWidth();

              // Extend to storage width if needed
              auto* val_const = llvm::ConstantInt::get(
                  elem_type, pair.value.zextOrTrunc(storage_width));
              auto* unk_const = llvm::ConstantInt::get(
                  elem_type, pair.unknown.zextOrTrunc(storage_width));
              return llvm::ConstantStruct::get(
                  struct_type, {val_const, unk_const});
            }

            // 2-state constant: backing-domain width (power-of-2 up to 64,
            // exact semantic width above 64) to match alloca/PHI types.
            // Lane normalization happens at the PSV store boundary.
            auto* storage_type = GetBackingLlvmType(llvm_ctx, bit_width);
            uint32_t storage_width = storage_type->getIntegerBitWidth();
            return llvm::ConstantInt::get(
                storage_type, value_ap.zextOrTrunc(storage_width));
          },
          [&](const StringConstant& str) -> Result<llvm::Value*> {
            auto& builder = context.GetBuilder();
            auto* data = builder.CreateGlobalStringPtr(str.value);
            auto* len = llvm::ConstantInt::get(
                llvm::Type::getInt64Ty(llvm_ctx), str.value.size());
            return builder.CreateCall(
                context.GetLyraStringFromLiteral(), {data, len}, "str.handle");
          },
          [&](const RealConstant& real) -> Result<llvm::Value*> {
            const Type& type = context.GetTypeArena()[constant.type];
            if (type.Kind() == TypeKind::kShortReal) {
              return llvm::ConstantFP::get(
                  llvm::Type::getFloatTy(llvm_ctx),
                  static_cast<float>(real.value));
            }
            return llvm::ConstantFP::get(
                llvm::Type::getDoubleTy(llvm_ctx), real.value);
          },
          [&](const StructConstant& /*s*/) -> Result<llvm::Value*> {
            return std::unexpected(
                context.GetDiagnosticContext().MakeUnsupported(
                    context.GetCurrentOrigin(),
                    "struct constants not yet supported",
                    UnsupportedCategory::kType));
          },
          [&](const ArrayConstant& /*a*/) -> Result<llvm::Value*> {
            return std::unexpected(
                context.GetDiagnosticContext().MakeUnsupported(
                    context.GetCurrentOrigin(),
                    "array constants not yet supported",
                    UnsupportedCategory::kType));
          },
          [&](const NullConstant& /*n*/) -> Result<llvm::Value*> {
            const Type& ty = context.GetTypeArena()[constant.type];
            if (ty.Kind() != TypeKind::kChandle) {
              return std::unexpected(
                  context.GetDiagnosticContext().MakeUnsupported(
                      context.GetCurrentOrigin(),
                      std::format(
                          "null constant for non-chandle type: {}",
                          ToString(ty)),
                      UnsupportedCategory::kType));
            }
            return llvm::ConstantPointerNull::get(
                llvm::PointerType::getUnqual(llvm_ctx));
          },
      },
      constant.value);
}

auto LowerOperandRaw(
    Context& context, SlotAccessResolver& resolver, const mir::Operand& operand)
    -> Result<llvm::Value*> {
  return std::visit(
      common::Overloaded{
          [&context](const Constant& constant) -> Result<llvm::Value*> {
            return LowerConstant(context, constant);
          },
          [&context, &resolver](mir::PlaceId place_id) -> Result<llvm::Value*> {
            // Projected reads (bit-range) are canonical-only in v1.
            if (context.HasBitRangeProjection(place_id)) {
              return LoadBitRange(context, place_id);
            }
            const auto& place = context.GetMirArena()[place_id];
            if (place.root.kind == mir::PlaceRoot::Kind::kModuleSlot &&
                place.projections.empty()) {
              return resolver.LoadSlotValue(place_id);
            }
            return context.LoadPlaceValue(place_id);
          },
          [&context](mir::TempId temp_id) -> Result<llvm::Value*> {
            const auto& tv = context.ReadTempValue(temp_id.value);
            return BuildRawValueFromTempValue(context.GetBuilder(), tv);
          },
      },
      operand.payload);
}

auto LowerOperand(
    Context& context, SlotAccessResolver& resolver, const mir::Operand& operand)
    -> Result<llvm::Value*> {
  auto val_or_err = LowerOperandRaw(context, resolver, operand);
  if (!val_or_err) return std::unexpected(val_or_err.error());
  llvm::Value* val = *val_or_err;

  if (val->getType()->isStructTy()) {
    auto& builder = context.GetBuilder();
    auto* value_bits = builder.CreateExtractValue(val, 0, "coerce.val");
    auto* unk_bits = builder.CreateExtractValue(val, 1, "coerce.unk");
    auto* not_unk = builder.CreateNot(unk_bits, "coerce.notunk");
    val = builder.CreateAnd(value_bits, not_unk, "coerce.known");
  }
  return val;
}

auto BuildRawValueFromTempValue(llvm::IRBuilder<>& builder, const TempValue& tv)
    -> llvm::Value* {
  if (tv.domain == ValueDomain::kFourState) {
    llvm::Type* val_ty = tv.value->getType();
    auto* struct_ty =
        llvm::StructType::get(val_ty->getContext(), {val_ty, val_ty});
    return PackFourState(builder, struct_ty, tv.value, tv.unknown);
  }
  // kTwoState: return scalar directly. This is the domain-aware raw shape.
  return tv.value;
}

}  // namespace lyra::lowering::mir_to_llvm
