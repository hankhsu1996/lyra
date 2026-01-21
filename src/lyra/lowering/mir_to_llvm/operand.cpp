#include "lyra/lowering/mir_to_llvm/operand.hpp"

#include <stdexcept>

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
            throw std::runtime_error(
                "variable reads (PlaceId) not yet supported in LLVM backend");
          },
      },
      operand.payload);
}

auto LowerConstant(Context& context, const Constant& constant) -> llvm::Value* {
  auto& llvm_ctx = context.GetLlvmContext();

  return std::visit(
      Overloaded{
          [&llvm_ctx](const IntegralConstant& integral) -> llvm::Value* {
            // For now, only handle single-word 2-state values
            if (integral.value.size() != 1 || !integral.x_mask.empty() ||
                !integral.z_mask.empty()) {
              throw std::runtime_error(
                  "multi-word or 4-state integral constants not yet supported "
                  "in LLVM backend");
            }
            return llvm::ConstantInt::get(
                llvm::Type::getInt64Ty(llvm_ctx), integral.value[0]);
          },
          [&context](const StringConstant& str) -> llvm::Value* {
            return context.GetBuilder().CreateGlobalStringPtr(str.value);
          },
          [&llvm_ctx](const RealConstant& real) -> llvm::Value* {
            return llvm::ConstantFP::get(
                llvm::Type::getDoubleTy(llvm_ctx), real.value);
          },
          [](const StructConstant& /*s*/) -> llvm::Value* {
            throw std::runtime_error(
                "struct constants not yet supported in LLVM backend");
          },
          [](const ArrayConstant& /*a*/) -> llvm::Value* {
            throw std::runtime_error(
                "array constants not yet supported in LLVM backend");
          },
      },
      constant.value);
}

}  // namespace lyra::lowering::mir_to_llvm
