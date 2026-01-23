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

  // Coerce 4-state struct to 2-state integer: extract a & ~b (known bits)
  if (val->getType()->isStructTy()) {
    auto& builder = context.GetBuilder();
    auto* a = builder.CreateExtractValue(val, 0, "coerce.a");
    auto* b = builder.CreateExtractValue(val, 1, "coerce.b");
    auto* not_b = builder.CreateNot(b, "coerce.notb");
    val = builder.CreateAnd(a, not_b, "coerce.val");
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

            // Create APInt from word array (little-endian in both MIR and LLVM)
            llvm::APInt ap_value(bit_width, integral.value);

            // 4-state constant: convert to (a, b) struct
            if (!integral.x_mask.empty() || !integral.z_mask.empty()) {
              llvm::APInt x_mask_ap(bit_width, integral.x_mask);
              llvm::APInt z_mask_ap(bit_width, integral.z_mask);
              auto pair = ToAB(ap_value, x_mask_ap, z_mask_ap);
              MaskFourState(pair, bit_width);

              // Get the struct type {iN_storage, iN_storage}
              auto* struct_type = llvm::cast<llvm::StructType>(
                  context.GetPlaceLlvmType4State(bit_width));
              auto* elem_type = struct_type->getElementType(0);
              uint32_t storage_width = elem_type->getIntegerBitWidth();

              // Extend to storage width if needed
              auto* a_const = llvm::ConstantInt::get(
                  elem_type, pair.a.zextOrTrunc(storage_width));
              auto* b_const = llvm::ConstantInt::get(
                  elem_type, pair.b.zextOrTrunc(storage_width));
              return llvm::ConstantStruct::get(struct_type, {a_const, b_const});
            }

            // 2-state constant: simple integer
            return llvm::ConstantInt::get(llvm_ctx, ap_value);
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
