#include "lyra/llvm_backend/value_repr.hpp"

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

namespace lyra::lowering::mir_to_llvm {

auto GetBackingLlvmType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::Type* {
  if (bit_width <= 8) {
    return llvm::Type::getInt8Ty(ctx);
  }
  if (bit_width <= 16) {
    return llvm::Type::getInt16Ty(ctx);
  }
  if (bit_width <= 32) {
    return llvm::Type::getInt32Ty(ctx);
  }
  if (bit_width <= 64) {
    return llvm::Type::getInt64Ty(ctx);
  }
  return llvm::Type::getIntNTy(ctx, bit_width);
}

auto GetBackingFourStateType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::StructType* {
  auto* elem = GetBackingLlvmType(ctx, bit_width);
  return llvm::StructType::get(ctx, {elem, elem});
}

}  // namespace lyra::lowering::mir_to_llvm
