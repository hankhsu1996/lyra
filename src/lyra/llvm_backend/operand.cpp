#include "lyra/llvm_backend/operand.hpp"

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "lyra/common/four_state.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Load a place with BitRangeProjection: load base, lshr by offset, trunc.
auto LoadBitRange(Context& context, mir::PlaceId place_id) -> llvm::Value* {
  auto& builder = context.GetBuilder();
  const auto& bitrange = context.GetBitRangeProjection(place_id);

  // Load the full base value
  llvm::Value* ptr = context.GetPlacePointer(place_id);
  llvm::Type* base_type = context.GetPlaceBaseType(place_id);
  llvm::Value* base = builder.CreateLoad(base_type, ptr, "base");

  // Lower the bit offset and coerce to base integer width
  llvm::Value* offset = LowerOperand(context, bitrange.bit_offset);

  // Get the element LLVM type for the result
  llvm::Type* elem_type = context.GetPlaceLlvmType(place_id);

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

      llvm::Value* result = llvm::UndefValue::get(result_struct);
      result = builder.CreateInsertValue(result, val, 0);
      result = builder.CreateInsertValue(result, unk, 1);
      return result;
    }

    // 2-state result from 4-state base: trunc to element width, mask unknown
    if (elem_type != plane_type) {
      val = builder.CreateTrunc(val, elem_type, "br.val.trunc");
      unk = builder.CreateTrunc(unk, elem_type, "br.unk.trunc");
    }
    auto* not_unk = builder.CreateNot(unk, "br.notunk");
    return builder.CreateAnd(val, not_unk, "br.known");
  }

  // 2-state base: shift + trunc
  auto* shift_amt = builder.CreateZExtOrTrunc(offset, base_type, "br.offset");
  llvm::Value* shifted = builder.CreateLShr(base, shift_amt, "br.shr");
  if (elem_type == base_type) {
    return shifted;
  }
  return builder.CreateTrunc(shifted, elem_type, "br.trunc");
}

}  // namespace

auto LowerOperandRaw(Context& context, const mir::Operand& operand)
    -> llvm::Value* {
  return std::visit(
      Overloaded{
          [&context](const Constant& constant) {
            return LowerConstant(context, constant);
          },
          [&context](mir::PlaceId place_id) -> llvm::Value* {
            if (context.HasBitRangeProjection(place_id)) {
              return LoadBitRange(context, place_id);
            }
            llvm::Value* ptr = context.GetPlacePointer(place_id);
            llvm::Type* type = context.GetPlaceLlvmType(place_id);
            return context.GetBuilder().CreateLoad(type, ptr, "load");
          },
      },
      operand.payload);
}

auto LowerOperand(Context& context, const mir::Operand& operand)
    -> llvm::Value* {
  llvm::Value* val = LowerOperandRaw(context, operand);

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

auto LowerConstant(Context& context, const Constant& constant) -> llvm::Value* {
  auto& llvm_ctx = context.GetLlvmContext();

  return std::visit(
      Overloaded{
          [&](const IntegralConstant& integral) -> llvm::Value* {
            // Get semantic bit width from type
            const Type& type = context.GetTypeArena()[constant.type];
            uint32_t bit_width = PackedBitWidth(type, context.GetTypeArena());

            // Create APInt from word arrays (little-endian in both MIR and
            // LLVM)
            llvm::APInt value_ap(bit_width, integral.value);

            // 4-state constant: create {value, unknown} struct directly
            if (!integral.IsKnown()) {
              llvm::APInt unknown_ap(bit_width, integral.unknown);
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
          [&](const StringConstant& str) -> llvm::Value* {
            auto& builder = context.GetBuilder();
            auto* data = builder.CreateGlobalStringPtr(str.value);
            auto* len = llvm::ConstantInt::get(
                llvm::Type::getInt64Ty(llvm_ctx), str.value.size());
            return builder.CreateCall(
                context.GetLyraStringFromLiteral(), {data, len}, "str.handle");
          },
          [&](const RealConstant& real) -> llvm::Value* {
            return llvm::ConstantFP::get(
                llvm::Type::getDoubleTy(llvm_ctx), real.value);
          },
          [&](const StructConstant& /*s*/) -> llvm::Value* {
            throw common::UnsupportedErrorException(
                common::UnsupportedLayer::kMirToLlvm,
                common::UnsupportedKind::kType, context.GetCurrentOrigin(),
                "struct constants not yet supported");
          },
          [&](const ArrayConstant& /*a*/) -> llvm::Value* {
            throw common::UnsupportedErrorException(
                common::UnsupportedLayer::kMirToLlvm,
                common::UnsupportedKind::kType, context.GetCurrentOrigin(),
                "array constants not yet supported");
          },
      },
      constant.value);
}

}  // namespace lyra::lowering::mir_to_llvm
