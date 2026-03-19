#pragma once

#include <cstdint>
#include <optional>

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit/signal_id_expr.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct WriteTarget;

// Pointers to packed storage planes for direct word-level access.
// Used for runtime helpers that write directly to storage.
struct PackedPlanesPtr {
  // Original storage pointer (for notify)
  llvm::Value* root_ptr = nullptr;
  // Pointer to value plane (opaque ptr)
  llvm::Value* val_ptr = nullptr;
  // Pointer to unknown plane (null for 2-state)
  llvm::Value* unk_ptr = nullptr;
  // For design-slot notification
  std::optional<SignalIdExpr> signal_id;
};

// Raw-value commit adapter. Routes through DispatchWrite with RawValueSource.
// Does not own semantic write routing -- see write_plan.hpp for the canonical
// write dispatch boundary. Retained for callers (assoc_op, call, etc.) that
// have an already-loaded llvm::Value*.
auto CommitValue(
    Context& ctx, mir::PlaceId target, llvm::Value* raw_value, TypeId type_id,
    OwnershipPolicy policy) -> Result<void>;

// Store a non-managed value to a target place.
//
// Caller provides the value in SSA compute form (iN for 2-state packed,
// {iN, iN} for 4-state packed, float/double, or SSA aggregate compute
// form for non-managed unpacked structs/arrays). This function performs:
// 1. Storage-lane width lowering (semantic -> storage width for packed)
// 2. Canonical storage materialization (for design-slot aggregates)
// 3. Design-slot notification (compare/store + dirty mark)
//
// Not for managed types (string/container) -- those go through CommitValue.
// TypeId is needed to resolve storage spec for the lowering and commit.
void CommitPackedValueRaw(
    Context& ctx, mir::PlaceId target, llvm::Value* value, TypeId type_id);

// Get pointers to packed storage planes for direct write access.
// For 4-state: val_ptr is slot_ptr, unk_ptr is GEP to unknown lane offset.
// For 2-state: val_ptr is slot_ptr, unk_ptr is null.
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
auto GetSignalIdForNba(Context& ctx, mir::PlaceId target) -> SignalIdExpr;

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

// Resolve design signal ID for a target place (after alias resolution).
// Returns SignalIdExpr if design slot, nullopt if not.
auto GetDesignSignalId(Context& ctx, mir::PlaceId target)
    -> std::optional<SignalIdExpr>;

// Flush an activation-local managed value to canonical whole-slot storage
// under the current store mode and notification policy.
//
// Performs store + compare + dirty-mark (or plain store under kDirectInit
// or kDeferred policy). The value must already be in canonical storage
// representation (same LLVM type as the slot's canonical memory layout).
//
// This is the single synchronization boundary for activation-local ->
// canonical slot commits. Do not open-code store + dirty-mark in
// slot_access.cpp or elsewhere.
void EmitActivationLocalFlush(
    Context& ctx, llvm::Value* canonical_ptr, llvm::Value* value,
    const SignalIdExpr& signal_id);

namespace detail {

// Lifecycle-aware struct field-by-field transfer from source pointer to
// destination pointer. Threads ownership policy through recursion, dispatches
// managed field lifecycle (string retain/release) via CopyAssign/MoveAssign.
// Callers should not reimplement field traversal or managed dispatch for
// unpacked structs containing managed fields.
auto TransferManagedStructFields(
    Context& ctx, llvm::Value* source_ptr, llvm::Value* target_ptr,
    TypeId struct_type_id, OwnershipPolicy policy) -> Result<void>;

// Field-level store for struct field-by-field assignment.
// No WriteTarget (fields don't have signal_id), no notify.
void CommitStringField(
    Context& ctx, llvm::Value* ptr, llvm::Value* handle,
    OwnershipPolicy policy);

void CommitPlainField(Context& ctx, llvm::Value* ptr, llvm::Value* value);

}  // namespace detail

}  // namespace lyra::lowering::mir_to_llvm
