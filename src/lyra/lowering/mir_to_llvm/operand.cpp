#include "lyra/lowering/mir_to_llvm/operand.hpp"

#include "llvm/IR/Instructions.h"
#include "lyra/common/overloaded.hpp"
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
            // Look up the alloca for this place
            llvm::AllocaInst* alloca = context.GetPlaceStorage(place_id);
            if (alloca == nullptr) {
              // Place hasn't been assigned yet - try to create storage
              alloca = context.GetOrCreatePlaceStorage(place_id);
            }
            // Load the value from the alloca
            return context.GetBuilder().CreateLoad(
                alloca->getAllocatedType(), alloca, "load");
          },
      },
      operand.payload);
}

auto LowerConstant(Context& context, const Constant& constant) -> llvm::Value* {
  auto& llvm_ctx = context.GetLlvmContext();

  return std::visit(
      Overloaded{
          [&](const IntegralConstant& integral) -> llvm::Value* {
            // For now, only handle single-word 2-state values
            if (integral.value.size() != 1 || !integral.x_mask.empty() ||
                !integral.z_mask.empty()) {
              throw common::UnsupportedErrorException(
                  common::UnsupportedLayer::kMirToLlvm,
                  common::UnsupportedKind::kType, context.GetCurrentOrigin(),
                  "multi-word or 4-state integral constants not yet supported");
            }
            return llvm::ConstantInt::get(
                llvm::Type::getInt64Ty(llvm_ctx), integral.value[0]);
          },
          [&](const StringConstant& str) -> llvm::Value* {
            return context.GetBuilder().CreateGlobalStringPtr(str.value);
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
