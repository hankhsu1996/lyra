#include "lyra/lowering/mir_to_llvm/operand.hpp"

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Instructions.h"
#include "lyra/common/four_state.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerOperandRaw(Context& context, const mir::Operand& operand)
    -> llvm::Value* {
  return std::visit(
      Overloaded{
          [&context](const Constant& constant) {
            return LowerConstant(context, constant);
          },
          [&context](mir::PlaceId place_id) -> llvm::Value* {
            // Get pointer to place storage and load value
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
