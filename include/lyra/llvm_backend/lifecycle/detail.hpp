#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/type.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// INTERNAL: Null out managed fields after move from src_ptr.
//
// This is an implementation detail used by MoveInit internally and by the
// commit layer during field-by-field assignment. Prefer MoveInit/MoveAssign
// for new code.
//
// Per-TypeKind semantics:
// - Scalars: no-op
// - String, DynamicArray, Queue: store nullptr
// - UnpackedStruct: recurse for managed fields
// - UnpackedArray: recurse for managed elements
void MoveCleanup(Context& ctx, llvm::Value* src_ptr, TypeId type_id);

// INTERNAL: Clone/retain a leaf value (string or container handle).
//
// For leaf managed types only (string, dynarray, queue). Returns a new owned
// value. Throws InternalError for aggregates (structs, arrays) which must use
// CopyInit instead.
//
// Use CopyInit/CopyAssign for new code - this is retained for commit layer
// paths that work with loaded handle values rather than pointers.
[[nodiscard]] auto CloneLeafValue(
    Context& ctx, llvm::Value* value, TypeId type_id) -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
