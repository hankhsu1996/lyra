#pragma once

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"

namespace lyra::lowering::mir_to_llvm {

struct FourStateValue {
  llvm::Value* value;
  llvm::Value* unknown;
};

// Type queries (no Context needed)
auto IsTypeFourState(const TypeArena& types, TypeId type_id) -> bool;

// Extract value and unknown planes from a 4-state struct {iN, iN}
auto ExtractFourState(llvm::IRBuilderBase& builder, llvm::Value* struct_val)
    -> FourStateValue;

// Pack value and unknown planes into a 4-state struct {iN, iN}
auto PackFourState(
    llvm::IRBuilderBase& builder, llvm::StructType* struct_type,
    llvm::Value* val, llvm::Value* unk) -> llvm::Value*;

// Create a known 4-state value: wraps integer as {val, 0}
auto MakeKnown(
    llvm::IRBuilderBase& builder, llvm::StructType* struct_type,
    llvm::Value* val) -> llvm::Value*;

// Get the element integer type for a 4-state storage type.
// For a 4-state type stored as {iN, iN}, returns iN.
// This prevents accidentally casting to the struct type instead of the element.
auto GetFourStateElemIntType(llvm::StructType* struct_type)
    -> llvm::IntegerType*;

}  // namespace lyra::lowering::mir_to_llvm
