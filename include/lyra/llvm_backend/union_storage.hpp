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

// Union storage info - cached layout for union types
struct UnionStorageInfo {
  uint32_t size;             // Storage size in bytes
  uint32_t align;            // ABI alignment in bytes
  llvm::Type* storage_type;  // [size x i8] array type
};

// Get LLVM storage type for an integral, rounding up to power-of-2.
auto GetLlvmStorageType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::Type*;

// Build LLVM type for a TypeId (Context-aware version).
// Use this when unions may be encountered - uses DataLayout for correct sizing.
// Returns error for unsupported types (e.g., 4-state unions).
auto BuildLlvmTypeForTypeId(Context& context, TypeId type_id)
    -> Result<llvm::Type*>;

// Build LLVM type for a TypeId (LLVMContext-only version).
// WARNING: This version cannot correctly handle unions (throws InternalError).
// Only use for types guaranteed not to contain unions.
auto BuildLlvmTypeForTypeId(
    llvm::LLVMContext& ctx, TypeId type_id, const TypeArena& types)
    -> llvm::Type*;

// Get union storage info (cached). Validates 4-state restriction.
auto GetUnionStorageInfo(Context& context, TypeId union_type_id)
    -> Result<UnionStorageInfo>;

// Build LLVM storage type for an unpacked union (byte array with alignment)
// Returns error for unsupported types (e.g., 4-state unions).
auto BuildUnpackedUnionType(
    Context& context, TypeId union_type_id, const TypeArena& types)
    -> Result<llvm::Type*>;

}  // namespace lyra::lowering::mir_to_llvm
