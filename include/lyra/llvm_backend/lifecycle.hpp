#pragma once

#include <llvm/IR/Value.h>

#include "lyra/common/type.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;

// Lifecycle API Contracts
//
// CopyInit(dst, src):
//   PRE:  dst is uninitialized storage (no live value)
//   POST: dst becomes a clone of src; src unchanged
//
// CopyAssign(dst, src):
//   PRE:  dst is initialized (may hold a live value)
//   POST: Destroy(dst) then CopyInit(dst, src)
//
// MoveInit(dst, src):
//   PRE:  dst is uninitialized storage (no live value)
//   POST: dst owns src's value; src is nulled/empty (moved-from)
//
// MoveAssign(dst, src):
//   PRE:  dst is initialized (may hold a live value)
//   POST: Destroy(dst) then MoveInit(dst, src)
//
// Important: The storage initialization invariant (all locals/temps/return
// slots are default-initialized at function entry) makes Destroy() safe on
// first access. But Init vs Assign semantics matter for correctness - misusing
// Init on an initialized slot leaks the old value.

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

// Copy-initialize dst_ptr from src_ptr (dst is uninitialized).
//
// Per-TypeKind semantics:
// - Scalars: load + store
// - String: retain/clone handle
// - DynamicArray: clone container
// - Queue: clone container
// - UnpackedStruct: recurse field-by-field
// - UnpackedArray: recurse element-by-element
// - Void: InternalError
void CopyInit(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id);

// Copy-assign src_ptr to dst_ptr (dst may hold a live value).
// Equivalent to: Destroy(dst) + CopyInit(dst, src)
void CopyAssign(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id);

// Move-initialize dst_ptr from src_ptr (dst is uninitialized).
// Transfers ownership: loads from src, stores to dst, nulls out src.
// More efficient than CopyInit (no retain/clone).
void MoveInit(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id);

// Move-assign src_ptr to dst_ptr (dst may hold a live value).
// Equivalent to: Destroy(dst) + MoveInit(dst, src)
void MoveAssign(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id);

// Clone/retain a value, returning new owned value.
//
// Per-TypeKind semantics:
// - Scalars: return value unchanged
// - String: retain the handle
// - DynamicArray, Queue: clone the handle
// - Aggregates (struct, union, array): InternalError (must use CopyInit)
// - Void: InternalError
//
// Note: For new code, prefer CopyInit/CopyAssign which work with pointers
// and support aggregates. CloneValue is retained for leaf-value paths.
[[nodiscard]] auto CloneValue(Context& ctx, llvm::Value* value, TypeId type_id)
    -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
