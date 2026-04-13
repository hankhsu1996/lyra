#include "lyra/llvm_backend/emit_string_conv.hpp"

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"

namespace lyra::lowering::mir_to_llvm {

auto EmitPackedToString(
    llvm::IRBuilder<>& builder, const CuFacts& facts,
    llvm::Function* from_packed_fn, llvm::Value* packed_value,
    const Type& packed_type) -> llvm::Value* {
  if (!IsPacked(packed_type)) {
    throw common::InternalError(
        "EmitPackedToString", "called with non-packed type");
  }

  const auto& types = *facts.types;
  uint32_t bit_width = PackedBitWidth(packed_type, types);

  // Handle 4-state: extract value plane (unknown bits ignored per cast.cpp)
  llvm::Value* value = packed_value;
  llvm::Type* storage_type = value->getType();
  if (storage_type->isStructTy()) {
    value = builder.CreateExtractValue(value, 0, "tostr.val");
    storage_type = value->getType();
  }

  auto* alloca = builder.CreateAlloca(storage_type, nullptr, "tostr.tmp");
  builder.CreateStore(value, alloca);

  auto* i32_ty = llvm::Type::getInt32Ty(builder.getContext());
  return builder.CreateCall(
      from_packed_fn, {alloca, llvm::ConstantInt::get(i32_ty, bit_width)},
      "tostr.handle");
}

}  // namespace lyra::lowering::mir_to_llvm
