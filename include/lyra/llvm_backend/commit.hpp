#pragma once

#include <cstdint>
#include <optional>

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct WriteTarget;

// Pointers to packed storage planes for direct word-level access.
// Used for runtime helpers that write directly to storage.
struct PackedPlanesPtr {
  llvm::Value* root_ptr = nullptr;  // Original storage pointer (for notify)
  llvm::Value* val_ptr = nullptr;   // Pointer to value plane (opaque ptr)
  llvm::Value* unk_ptr =
      nullptr;  // Pointer to unknown plane (null for 2-state)
  std::optional<uint32_t> signal_id;  // For design-slot notification
};

// Store raw value to target place with type-appropriate handling.
// Handles: WriteTarget resolution, ownership (retain/clone), stateness
// coercion, store+notify. Contract: raw_value from LowerOperandRaw, caller
// handles source cleanup.
auto CommitValue(
    Context& ctx, mir::PlaceId target, llvm::Value* raw_value, TypeId type_id,
    OwnershipPolicy policy) -> Result<void>;

// Low-level escape hatch for already-coerced packed values.
// Use when caller has performed RMW or type coercion and has final LLVM value.
// Does NOT accept TypeId or OwnershipPolicy - caller asserts value is ready.
// Resolves WriteTarget internally, stores, notifies if design slot.
void CommitPackedValueRaw(
    Context& ctx, mir::PlaceId target, llvm::Value* value);

// Get pointers to packed storage planes for direct write access.
// For 4-state {iN, iN}: GEPs to field 0 (val) and field 1 (unk).
// For 2-state iN: val_ptr is storage pointer, unk_ptr is null.
// Pointers are bitcast to i64* for word-level runtime access.
auto GetPackedPlanesPtr(Context& ctx, mir::PlaceId target, TypeId type_id)
    -> Result<PackedPlanesPtr>;

// Notify after union byte-copy (memcpy already done by caller).
// Invariant: caller has performed memcpy to target.
// Conditional: no-op if target is not a design slot.
void CommitNotifyUnionMemcpyIfDesignSlot(
    Context& ctx, mir::PlaceId target, uint32_t byte_size);

// Notify queue/container mutation (handle unchanged, content changed).
// Invariant: handle at target unchanged, but logical content mutated
// (push/pop/delete).
// Conditional: no-op if target is not a design slot.
void CommitNotifyMutationIfDesignSlot(Context& ctx, mir::PlaceId target);

// Notify after aggregate (struct/array) field-by-field assignment.
// Precondition: target is the aggregate root place (not a subplace).
// Conditional: no-op if target is not a design slot.
// Semantics: guarantees level-sensitive re-evaluation (always_comb, always
// @(*)); edge-sensitive events (posedge/negedge) on aggregates are NOT
// supported.
void CommitNotifyAggregateIfDesignSlot(Context& ctx, mir::PlaceId target);

// NBA-specific signal_id extraction.
// NBA always targets design slots, so fail-fast is correct behavior.
// Throws InternalError if target is not a design slot.
auto GetSignalIdForNba(Context& ctx, mir::PlaceId target) -> uint32_t;

// Null-out source managed fields if move from temp.
// Policy gate: only acts if policy==kMove AND source place root is kTemp.
// Caller provides PlaceId; commit verifies temp-ness and calls
// lifecycle::MoveCleanup.
void CommitMoveCleanupIfTemp(
    Context& ctx, mir::PlaceId source, OwnershipPolicy policy, TypeId type_id);

// Struct field-by-field assignment for structs containing string fields.
// Handles design-slot detection internally, returning error if design slot
// with string-containing struct (not yet supported).
// Caller ensures NeedsFieldByField(struct_type_id, types) is true.
auto CommitStructFieldByField(
    Context& ctx, mir::PlaceId target, mir::PlaceId source,
    TypeId struct_type_id, OwnershipPolicy policy) -> Result<void>;

// Array element-by-element assignment for arrays containing managed elements.
// Handles design-slot detection internally, returning error if design slot
// with managed-containing array (not yet supported).
// Caller ensures NeedsFieldByField(array_type_id, types) is true.
auto CommitArrayFieldByField(
    Context& ctx, mir::PlaceId target, mir::PlaceId source,
    TypeId array_type_id, OwnershipPolicy policy) -> Result<void>;

// Resolve design slot ID for a target place (after alias resolution).
// Returns slot_id if design slot, nullopt if not.
auto GetDesignSlotId(Context& ctx, mir::PlaceId target)
    -> std::optional<uint32_t>;

namespace detail {

// Field-level store for struct field-by-field assignment.
// No WriteTarget (fields don't have signal_id), no notify.
void CommitStringField(
    Context& ctx, llvm::Value* ptr, llvm::Value* handle,
    OwnershipPolicy policy);

void CommitPlainField(Context& ctx, llvm::Value* ptr, llvm::Value* value);

}  // namespace detail

}  // namespace lyra::lowering::mir_to_llvm
