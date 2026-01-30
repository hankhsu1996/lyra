#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/type.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// Design Note: ConstructDefault is deliberately deferred.
//
// Current state:
// - Strings/dynarray default to nullptr - no explicit construct needed
// - Structs with all-POD fields use aggregate zero at init time (lower.cpp)
// - Structs with managed fields: each managed field is nullptr anyway
//
// When to revisit and add ConstructDefault():
// - Non-null default handles (e.g., pre-allocated empty string/array)
// - User-defined field initializers in struct declarations
// - Class types with constructors
// - Associative arrays (may need non-trivial default state)

// Release owned resources at ptr.
//
// Per-TypeKind semantics:
// - Scalars (integral, real, enum, packed): no-op
// - String: release the handle
// - DynamicArray, Queue: release the handle
// - UnpackedStruct: recurse for managed fields
// - UnpackedUnion: no-op (2-state only, no managed content)
// - UnpackedArray: recurse for managed elements
// - Void: InternalError
void Destroy(Context& ctx, llvm::Value* ptr, TypeId type_id);

// Clone/retain a value, returning new owned value.
//
// Per-TypeKind semantics:
// - Scalars: return value unchanged
// - String: retain the handle
// - DynamicArray, Queue: clone the handle
// - Aggregates (struct, union, array): InternalError (must use field-by-field)
// - Void: InternalError
[[nodiscard]] auto CloneValue(Context& ctx, llvm::Value* value, TypeId type_id)
    -> llvm::Value*;

// Null out managed fields after move from src_ptr.
//
// Per-TypeKind semantics:
// - Scalars: no-op
// - String, DynamicArray, Queue: store nullptr
// - UnpackedStruct: recurse for managed fields
// - UnpackedUnion: no-op (2-state only)
// - UnpackedArray: recurse for managed elements
// - Void: InternalError
void MoveCleanup(Context& ctx, llvm::Value* src_ptr, TypeId type_id);

// Move-initialize dst_ptr from src_ptr.
// Transfers ownership: loads from src, stores to dst, nulls out src.
// More efficient than CloneValue (no retain/release).
//
// CONTRACT:
// - Pre-condition: dst_ptr is UNINITIALIZED (no live value to destroy).
//   Caller must Destroy() any previous value before calling this.
// - Post-condition: dst_ptr holds the value, src_ptr is nulled out.
//
// This is NOT MoveAssign (which would destroy dst internally). The caller
// is responsible for ensuring dst is clean before the move.
void MoveInit(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id);

}  // namespace lyra::lowering::mir_to_llvm
