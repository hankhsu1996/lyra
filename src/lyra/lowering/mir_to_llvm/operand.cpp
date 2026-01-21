#include "lyra/lowering/mir_to_llvm/operand.hpp"

#include "lyra/common/overloaded.hpp"

namespace lyra::lowering::mir_to_llvm {

auto LowerOperand(Context& context, const mir::Operand& operand)
    -> llvm::Value* {
  return std::visit(
      Overloaded{
          [&context](const Constant& constant) {
            return LowerConstant(context, constant);
          },
          [](mir::PlaceId /*place*/) -> llvm::Value* {
            // TODO: Load from place
            return nullptr;
          },
      },
      operand.payload);
}

auto LowerConstant(Context& context, const Constant& constant) -> llvm::Value* {
  auto& llvm_ctx = context.GetLlvmContext();

  return std::visit(
      Overloaded{
          [&llvm_ctx](const IntegralConstant& /*integral*/) -> llvm::Value* {
            // TODO: Handle arbitrary-width integers
            // For now, just return 0
            return llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm_ctx), 0);
          },
          [&context](const StringConstant& str) -> llvm::Value* {
            return context.GetBuilder().CreateGlobalStringPtr(str.value);
          },
          [&llvm_ctx](const RealConstant& real) -> llvm::Value* {
            return llvm::ConstantFP::get(
                llvm::Type::getDoubleTy(llvm_ctx), real.value);
          },
          [](const StructConstant& /*s*/) -> llvm::Value* {
            // TODO: Handle struct constants
            return nullptr;
          },
          [](const ArrayConstant& /*a*/) -> llvm::Value* {
            // TODO: Handle array constants
            return nullptr;
          },
      },
      constant.value);
}

}  // namespace lyra::lowering::mir_to_llvm
