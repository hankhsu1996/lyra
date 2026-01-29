#pragma once

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

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

// Bitwise operations with correct 4-state propagation.
// These implement IEEE 1800 truth tables where dominant values override X/Z:
// - AND: 0 & X = 0 (known-zero dominates)
// - OR:  1 | X = 1 (known-one dominates)
// - XOR: unknown always propagates
// - NOT: inverts known bits, preserves unknown

auto FourStateAnd(
    llvm::IRBuilderBase& builder, FourStateValue lhs, FourStateValue rhs)
    -> FourStateValue;
auto FourStateOr(
    llvm::IRBuilderBase& builder, FourStateValue lhs, FourStateValue rhs)
    -> FourStateValue;
auto FourStateXor(
    llvm::IRBuilderBase& builder, FourStateValue lhs, FourStateValue rhs)
    -> FourStateValue;
auto FourStateNot(llvm::IRBuilderBase& builder, FourStateValue src)
    -> FourStateValue;

}  // namespace lyra::lowering::mir_to_llvm
