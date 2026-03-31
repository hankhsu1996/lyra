#pragma once

#include <optional>

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit/signal_id_expr.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/packed_storage_view.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct WriteTarget;

// Raw-value commit adapter. Routes through DispatchWrite with RawValueSource.
// Does not own semantic write routing -- see write_plan.hpp for the canonical
// write dispatch boundary. Retained for callers (assoc_op, call, etc.) that
// have an already-loaded llvm::Value*.
auto CommitValue(
    Context& ctx, mir::PlaceId target, llvm::Value* raw_value, TypeId type_id,
    OwnershipPolicy policy) -> Result<void>;

// Non-lossy packed value commit. Accepts a PackedRValue with preserved
// 2-state/4-state semantics (unk == nullptr means provably 2-state).
// For design targets: routes through PSV with store plan.
// For non-design targets (process locals): direct LLVM store, outside
// the packed-store policy architecture.
void CommitPackedValue(
    Context& ctx, mir::PlaceId target, const PackedRValue& rvalue,
    TypeId type_id);

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
auto GetSignalCoordForNba(Context& ctx, mir::PlaceId target) -> SignalCoordExpr;

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
// Returns SignalCoordExpr if design slot, nullopt if not.
auto GetDesignSignalCoord(Context& ctx, mir::PlaceId target)
    -> std::optional<SignalCoordExpr>;

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
