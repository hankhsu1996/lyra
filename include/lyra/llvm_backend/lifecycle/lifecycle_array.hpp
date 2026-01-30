#pragma once

#include <llvm/ADT/STLFunctionalExtras.h>
#include <llvm/IR/Value.h>

#include "lyra/common/type.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

namespace detail {

using ArrayElementCallback = llvm::function_ref<void(llvm::Value*, TypeId)>;

// Iterate over fixed-size unpacked array elements, calling callback for each.
// Generates an LLVM loop with constant trip count (from type table).
// The callback receives (element_ptr, element_type) for each index.
void ForEachArrayElementPtr(
    Context& ctx, llvm::Value* array_ptr, TypeId array_type_id,
    ArrayElementCallback callback);

// Destroy all elements in an unpacked array.
// Early-exits if element type doesn't contain managed content.
// Recursively calls top-level Destroy() for each element.
void DestroyArray(Context& ctx, llvm::Value* array_ptr, TypeId array_type_id);

// Null out managed fields after move from source array.
// Early-exits if element type doesn't contain managed content.
// Recursively calls top-level MoveCleanup() for each element.
void MoveCleanupArray(
    Context& ctx, llvm::Value* array_ptr, TypeId array_type_id);

// Copy-initialize dst array from src array (element-by-element).
// Recursively calls top-level CopyInit() for each element.
// Note: Uses element-by-element copy even for POD arrays (no memcpy fast-path).
void CopyInitArray(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr,
    TypeId array_type_id);

}  // namespace detail
}  // namespace lyra::lowering::mir_to_llvm
