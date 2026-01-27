#pragma once

#include <cstdint>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/type_ops.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct WriteTarget;

// Design Note: ConstructDefault API is deliberately deferred.
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

// Store a value to a WriteTarget.
// If canonical_signal_id has value, calls StoreDesignWithNotify.
// Otherwise, performs a plain store.
void StoreToWriteTarget(
    Context& context, llvm::Value* new_value, const WriteTarget& target);

// Store a non-string value to a design slot with change notification.
// Internal helper called by StoreToWriteTarget for design places.
// Asserts that target.canonical_signal_id has value.
void StoreDesignWithNotify(
    Context& context, llvm::Value* new_value, const WriteTarget& target);

// Store a dynamic array handle to a WriteTarget.
// Handles destroy-old, store-new, and notify if design place.
// The new_handle must already have the correct ownership (cloned if needed).
void StoreDynArrayToWriteTarget(
    Context& context, llvm::Value* new_handle, const WriteTarget& target,
    TypeId type_id);

// Store a string handle to a WriteTarget.
// Handles destroy-old, store-new, and notify if design place.
// The new_val must already have the correct ownership (retained if needed).
void StoreStringToWriteTarget(
    Context& context, llvm::Value* new_val, const WriteTarget& target,
    TypeId type_id);

// Store a string handle to a field pointer with ownership handling.
// Unlike StoreStringToWriteTarget, this takes a raw pointer (not WriteTarget)
// because field-by-field assignment doesn't have WriteTarget per field.
//
// This function handles:
// - Clone: retain the handle
// - Move: use handle directly (caller already nulled source)
// - Destroy old value at target
// - Store new handle
//
// NOTE: Does NOT handle design-slot notify (design slots with string-containing
// structs are rejected at AssignStruct level).
void StoreStringFieldRaw(
    Context& context, llvm::Value* target_ptr, llvm::Value* handle,
    OwnershipPolicy policy, TypeId type_id);

// Store a plain (non-managed) value to a field pointer.
// For field-by-field assignment - no design-slot notify (fields don't have
// WriteTarget). Future extension point if we need field-level notify.
void StorePlainFieldRaw(
    Context& context, llvm::Value* target_ptr, llvm::Value* value,
    TypeId type_id);

// Get union storage info (cached). Validates 4-state restriction.
auto GetUnionStorageInfo(Context& context, TypeId union_type_id)
    -> Result<UnionStorageInfo>;

// Notify after union byte-copy store to design slot.
// Called by AssignUnion after memcpy to trigger change notification.
// No-op if target is not a design slot.
void NotifyUnionStore(
    Context& context, const WriteTarget& target, uint32_t size);

// Build LLVM storage type for an unpacked union (byte array with alignment)
// Returns error for unsupported types (e.g., 4-state unions).
auto BuildUnpackedUnionType(
    Context& context, TypeId union_type_id, const TypeArena& types)
    -> Result<llvm::Type*>;

// Store a pre-lowered RAW value to a WriteTarget with type-appropriate
// handling.
//
// Contract:
// - raw_value MUST come from LowerOperandRaw (not LowerOperand)
// - This function handles: ownership (retain/clone), stateness coercion,
//   store+notify
// - This function does NOT handle: source null-out (caller's responsibility)
//
// Used by:
// - AssignXxx handlers (after LowerOperandRaw + source null-out)
// - LowerGuardedAssign (after LowerOperandRaw, always kClone)
auto StoreRawToTarget(
    Context& context, const WriteTarget& target, llvm::Value* raw_value,
    TypeId type_id, OwnershipPolicy policy) -> Result<void>;

// Null-out source managed fields if this is a move from a temp place.
//
// Conditions for null-out (all must be true):
// - policy == kMove
// - source is a PlaceId (not a Const)
// - source place root is kTemp
//
// If kMove is passed but source is not temp, throws InternalError (this
// indicates a bug in the caller or DetermineOwnership).
//
// Supported types:
// - kString, kDynamicArray, kQueue: null out the handle
// - kUnpackedStruct: recursively null out managed fields
// - Other types: no-op (no managed content)
//
// This is a mandatory helper that AssignXxx handlers for managed types and
// struct types (that may contain managed fields) MUST call after copying.
void NullOutSourceIfMoveTemp(
    Context& context, const mir::Operand& source, OwnershipPolicy policy,
    TypeId type_id);

}  // namespace lyra::lowering::mir_to_llvm
