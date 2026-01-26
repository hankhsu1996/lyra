#include "lyra/llvm_backend/four_state_utils.hpp"

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"

namespace lyra::lowering::mir_to_llvm {

auto IsTypeFourState(const TypeArena& types, TypeId type_id) -> bool {
  const Type& type = types[type_id];
  if (!IsPacked(type)) {
    return false;
  }
  return IsPackedFourState(type, types);
}

auto ExtractFourState(llvm::IRBuilderBase& builder, llvm::Value* struct_val)
    -> FourStateValue {
  auto* val = builder.CreateExtractValue(struct_val, 0, "fs.val");
  auto* unk = builder.CreateExtractValue(struct_val, 1, "fs.unk");
  return {.value = val, .unknown = unk};
}

auto PackFourState(
    llvm::IRBuilderBase& builder, llvm::StructType* struct_type,
    llvm::Value* val, llvm::Value* unk) -> llvm::Value* {
  llvm::Value* result = llvm::UndefValue::get(struct_type);
  result = builder.CreateInsertValue(result, val, 0, "fs.pack.val");
  result = builder.CreateInsertValue(result, unk, 1, "fs.pack.unk");
  return result;
}

auto MakeKnown(
    llvm::IRBuilderBase& builder, llvm::StructType* struct_type,
    llvm::Value* val) -> llvm::Value* {
  auto* elem_type = GetFourStateElemIntType(struct_type);
  auto* zero = llvm::ConstantInt::get(elem_type, 0);
  return PackFourState(builder, struct_type, val, zero);
}

auto GetFourStateElemIntType(llvm::StructType* struct_type)
    -> llvm::IntegerType* {
  return llvm::cast<llvm::IntegerType>(struct_type->getElementType(0));
}

}  // namespace lyra::lowering::mir_to_llvm
