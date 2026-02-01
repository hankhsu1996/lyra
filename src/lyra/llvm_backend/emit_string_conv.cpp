#include "lyra/llvm_backend/emit_string_conv.hpp"

#include <cassert>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>

#include "lyra/common/type_queries.hpp"

namespace lyra::lowering::mir_to_llvm {

auto EmitPackedToString(
    Context& context, llvm::Value* packed_value, const Type& packed_type)
    -> llvm::Value* {
  // Guardrail: must be called with a packed type
  assert(
      IsPacked(packed_type) &&
      "EmitPackedToString called with non-packed type");

  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();

  // Compute bit width from type (single source of truth)
  uint32_t bit_width = PackedBitWidth(packed_type, types);

  // Handle 4-state: extract value plane (unknown bits ignored per cast.cpp)
  llvm::Value* value = packed_value;
  llvm::Type* storage_type = value->getType();
  if (storage_type->isStructTy()) {
    // 4-state: {iN, iN} struct - extract value plane (index 0)
    value = builder.CreateExtractValue(value, 0, "tostr.val");
    storage_type = value->getType();
  }

  // Allocate stack space and store the packed value
  // LLVM stores iN as little-endian bytes, matching runtime expectation
  auto* alloca = builder.CreateAlloca(storage_type, nullptr, "tostr.tmp");
  builder.CreateStore(value, alloca);

  // Call LyraStringFromPacked(ptr data, i32 bit_width)
  // Returns a newly allocated handle (refcount=1) that caller must release
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  return builder.CreateCall(
      context.GetLyraStringFromPacked(),
      {alloca, llvm::ConstantInt::get(i32_ty, bit_width)}, "tostr.handle");
}

}  // namespace lyra::lowering::mir_to_llvm
