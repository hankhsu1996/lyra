#pragma once

#include <cstdint>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct CuFacts;

// Union storage info - cached layout for union types
struct UnionStorageInfo {
  uint32_t size;             // Storage size in bytes
  uint32_t align;            // ABI alignment in bytes
  llvm::Type* storage_type;  // [size x i8] array type
};

// Build LLVM type for a TypeId (Context-aware version).
// Use this when unions may be encountered - uses DataLayout for correct sizing.
// Returns error for unsupported types (e.g., 4-state unions).
auto BuildLlvmTypeForTypeId(
    Context& context, const CuFacts& facts, TypeId type_id)
    -> Result<llvm::Type*>;

// Build LLVM type for a TypeId (LLVMContext-only, intrinsic semantics).
// Uses intrinsic type four-state-ness only -- does NOT respect force_two_state.
// For backend-effective storage decisions, use
// BuildLlvmTypeForTypeId(Context&). WARNING: Cannot handle unions (throws
// InternalError).
auto BuildLlvmTypeForTypeId(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> llvm::Type*;

// Get union storage info (cached). Validates 4-state restriction.
auto GetUnionStorageInfo(
    Context& context, const CuFacts& facts, TypeId union_type_id)
    -> Result<UnionStorageInfo>;

// Build LLVM storage type for an unpacked union (byte array with alignment)
// Returns error for unsupported types (e.g., 4-state unions).
auto BuildUnpackedUnionType(
    Context& context, const CuFacts& facts, TypeId union_type_id,
    const TypeArena& types) -> Result<llvm::Type*>;

}  // namespace lyra::lowering::mir_to_llvm
