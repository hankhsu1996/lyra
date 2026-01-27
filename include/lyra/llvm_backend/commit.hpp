#pragma once

#include <cstdint>

#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm {

class Context;
struct WriteTarget;

// Store raw value to target place with type-appropriate handling.
// Handles: WriteTarget resolution, ownership (retain/clone), stateness
// coercion, store+notify. Contract: raw_value from LowerOperandRaw, caller
// handles source cleanup.
auto CommitValue(
    Context& ctx, mir::PlaceId target, llvm::Value* raw_value, TypeId type_id,
    OwnershipPolicy policy) -> Result<void>;

// Notify after union byte-copy (memcpy already done by caller).
// Invariant: caller has performed memcpy to target, this triggers design-slot
// notify. Fail-fast: throws InternalError if target is not a design slot.
void CommitNotifyUnionMemcpy(
    Context& ctx, mir::PlaceId target, uint32_t byte_size);

// Notify queue/container mutation (handle unchanged, content changed).
// Invariant: handle at target unchanged, but logical content mutated
// (push/pop/delete). Fail-fast: throws InternalError if target is not a design
// slot.
void CommitNotifyMutation(Context& ctx, mir::PlaceId target);

// Null-out source managed fields if move from temp.
// Policy gate: only acts if policy==kMove AND source place root is kTemp.
// Caller provides PlaceId; commit verifies temp-ness and calls
// lifecycle::MoveCleanup.
void CommitMoveCleanupIfTemp(
    Context& ctx, mir::PlaceId source, OwnershipPolicy policy, TypeId type_id);

namespace detail {

// Field-level store for struct field-by-field assignment.
// No WriteTarget (fields don't have signal_id), no notify.
void CommitStringField(
    Context& ctx, llvm::Value* ptr, llvm::Value* handle, OwnershipPolicy policy,
    TypeId type_id);

void CommitPlainField(
    Context& ctx, llvm::Value* ptr, llvm::Value* value, TypeId type_id);

// Store a pre-coerced packed value to WriteTarget with design-slot
// notification. For RMW operations and array stores where caller has already
// prepared the final LLVM value. If canonical_signal_id has value, notifies
// runtime.
void StorePackedToWriteTarget(
    Context& ctx, llvm::Value* new_value, const WriteTarget& wt);

}  // namespace detail

}  // namespace lyra::lowering::mir_to_llvm
