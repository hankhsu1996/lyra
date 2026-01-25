#pragma once

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// Get LLVM storage type for an integral, rounding up to power-of-2.
auto GetLlvmStorageType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::Type*;

// Build LLVM type for a TypeId (used for struct field access)
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

}  // namespace lyra::lowering::mir_to_llvm
