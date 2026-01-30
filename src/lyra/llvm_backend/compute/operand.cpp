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
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Load a place with BitRangeProjection: load base, lshr by offset, mask.
// Invariant: offset is always in-range (dynamic offsets are guarded by
// GuardedUse/IndexValidity which branches to OOB default before reaching here).
auto LoadBitRange(Context& context, mir::PlaceId place_id)
    -> Result<llvm::Value*> {
  auto& builder = context.GetBuilder();
  auto br_result = context.ComposeBitRange(place_id);
  if (!br_result) return std::unexpected(br_result.error());
  auto [offset, width] = *br_result;

  // Load the full base value (via read-only API)
  auto base_result = context.LoadPlaceBaseValue(place_id);
  if (!base_result) return std::unexpected(base_result.error());
  llvm::Value* base = *base_result;
  llvm::Type* base_type = base->getType();

  // Get the element LLVM type for the result
  auto elem_type_result = context.GetPlaceLlvmType(place_id);
  if (!elem_type_result) return std::unexpected(elem_type_result.error());
  llvm::Type* elem_type = *elem_type_result;

  if (base_type->isStructTy()) {
    // 4-state base: shift+trunc both planes
    auto* base_struct = llvm::cast<llvm::StructType>(base_type);
    auto* plane_type = base_struct->getElementType(0);

    llvm::Value* val = builder.CreateExtractValue(base, 0, "br.val");
    llvm::Value* unk = builder.CreateExtractValue(base, 1, "br.unk");

    auto* shift_amt =
        builder.CreateZExtOrTrunc(offset, plane_type, "br.offset");
    val = builder.CreateLShr(val, shift_amt, "br.val.shr");
    unk = builder.CreateLShr(unk, shift_amt, "br.unk.shr");

    if (elem_type->isStructTy()) {
      // 4-state result
      auto* result_struct = llvm::cast<llvm::StructType>(elem_type);
      auto* result_elem = result_struct->getElementType(0);
      if (result_elem != plane_type) {
        val = builder.CreateTrunc(val, result_elem, "br.val.trunc");
        unk = builder.CreateTrunc(unk, result_elem, "br.unk.trunc");
      }

      // Mask to semantic width (storage type may be wider due to rounding)
      uint32_t result_width = result_elem->getIntegerBitWidth();
      if (width < result_width) {
        auto mask = llvm::APInt::getLowBitsSet(result_width, width);
        auto* mask_val = llvm::ConstantInt::get(result_elem, mask);
        val = builder.CreateAnd(val, mask_val, "br.val.mask");
        unk = builder.CreateAnd(unk, mask_val, "br.unk.mask");
      }

      llvm::Value* result = llvm::UndefValue::get(result_struct);
      result = builder.CreateInsertValue(result, val, 0);
      result = builder.CreateInsertValue(result, unk, 1);
      return result;
    }

    // 2-state result from 4-state base: trunc/mask, then extract known bits
    if (elem_type != plane_type) {
      val = builder.CreateTrunc(val, elem_type, "br.val.trunc");
      unk = builder.CreateTrunc(unk, elem_type, "br.unk.trunc");
    }
    uint32_t elem_width = elem_type->getIntegerBitWidth();
    if (width < elem_width) {
      auto mask = llvm::APInt::getLowBitsSet(elem_width, width);
      auto* mask_val = llvm::ConstantInt::get(elem_type, mask);
      val = builder.CreateAnd(val, mask_val, "br.val.mask");
      unk = builder.CreateAnd(unk, mask_val, "br.unk.mask");
    }
    auto* not_unk = builder.CreateNot(unk, "br.notunk");
    return builder.CreateAnd(val, not_unk, "br.known");
  }

  // 2-state base: shift + mask to semantic width
  auto* shift_amt = builder.CreateZExtOrTrunc(offset, base_type, "br.offset");
  llvm::Value* shifted = builder.CreateLShr(base, shift_amt, "br.shr");
  uint32_t base_width = base_type->getIntegerBitWidth();
  if (width < base_width) {
    auto mask = llvm::APInt::getLowBitsSet(base_width, width);
    shifted = builder.CreateAnd(
        shifted, llvm::ConstantInt::get(base_type, mask), "br.mask");
  }
  return shifted;
}

}  // namespace

auto LowerOperandRaw(Context& context, const mir::Operand& operand)
    -> Result<llvm::Value*> {
  return std::visit(
      common::Overloaded{
          [&context](const Constant& constant) -> Result<llvm::Value*> {
            return LowerConstant(context, constant);
          },
          [&context](mir::PlaceId place_id) -> Result<llvm::Value*> {
            if (context.HasBitRangeProjection(place_id)) {
              return LoadBitRange(context, place_id);
            }
            return context.LoadPlaceValue(place_id);
          },
      },
      operand.payload);
}

auto LowerOperand(Context& context, const mir::Operand& operand)
    -> Result<llvm::Value*> {
  auto val_or_err = LowerOperandRaw(context, operand);
  if (!val_or_err) return std::unexpected(val_or_err.error());
  llvm::Value* val = *val_or_err;

  // Coerce 4-state struct to 2-state integer: value & ~unknown (known bits)
  if (val->getType()->isStructTy()) {
    auto& builder = context.GetBuilder();
    auto* value_bits = builder.CreateExtractValue(val, 0, "coerce.val");
    auto* unk_bits = builder.CreateExtractValue(val, 1, "coerce.unk");
    auto* not_unk = builder.CreateNot(unk_bits, "coerce.notunk");
    val = builder.CreateAnd(value_bits, not_unk, "coerce.known");
  }
  return val;
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

  // 2-state integer → 4-state struct: wrap as {zext(value), 0}
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

  // 4-state struct → 4-state struct (different storage width): widen/narrow
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

  // 2-state integer width coercion (storage rounding: e.g. i4 → i8)
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
            if (IsPacked(type) &&
                IsPackedFourState(type, context.GetTypeArena())) {
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

            // 2-state constant: simple integer
            return llvm::ConstantInt::get(llvm_ctx, value_ap);
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
      },
      constant.value);
}

}  // namespace lyra::lowering::mir_to_llvm
