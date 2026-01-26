#pragma once

#include <cstdint>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/handle.hpp"

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

// Release owned resources for a value at ptr of given type.
// This is the primitive operation - every assignment should Destroy(dst) first.
void Destroy(Context& context, llvm::Value* ptr, TypeId type_id);

// Recursively destroy fields of a struct
void DestroyStructFields(
    Context& context, llvm::Value* ptr, TypeId struct_type_id);

// Check if a target place is a design slot (needs notify on write)
auto IsDesignPlace(Context& context, mir::PlaceId place_id) -> bool;

// Get the signal_id for a design place (slot index)
auto GetSignalId(Context& context, mir::PlaceId place_id) -> uint32_t;

// Store a non-string value to a design slot with change notification.
void StoreDesignWithNotify(
    Context& context, llvm::Value* new_value, llvm::Value* target_ptr,
    llvm::Type* storage_type, mir::PlaceId target_place);

// Get union storage info (cached). Validates 4-state restriction.
auto GetUnionStorageInfo(Context& context, TypeId union_type_id)
    -> Result<UnionStorageInfo>;

// Build LLVM storage type for an unpacked union (byte array with alignment)
// Returns error for unsupported types (e.g., 4-state unions).
auto BuildUnpackedUnionType(
    Context& context, TypeId union_type_id, const TypeArena& types)
    -> Result<llvm::Type*>;

}  // namespace lyra::lowering::mir_to_llvm
