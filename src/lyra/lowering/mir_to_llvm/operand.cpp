#include "lyra/lowering/mir_to_llvm/operand.hpp"

#include "llvm/IR/Instructions.h"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerOperand(Context& context, const mir::Operand& operand)
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

auto LowerConstant(Context& context, const Constant& constant) -> llvm::Value* {
  auto& llvm_ctx = context.GetLlvmContext();

  return std::visit(
      Overloaded{
          [&](const IntegralConstant& integral) -> llvm::Value* {
            // 4-state not supported yet
            if (!integral.x_mask.empty() || !integral.z_mask.empty()) {
              throw common::UnsupportedErrorException(
                  common::UnsupportedLayer::kMirToLlvm,
                  common::UnsupportedKind::kType, context.GetCurrentOrigin(),
                  "4-state integral constants not yet supported");
            }

            // Get semantic bit width from type
            const Type& type = context.GetTypeArena()[constant.type];
            uint32_t bit_width = PackedBitWidth(type, context.GetTypeArena());

            // Create APInt from word array (little-endian in both MIR and LLVM)
            llvm::APInt ap_value(bit_width, integral.value);

            // Create constant with the exact bit width
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
