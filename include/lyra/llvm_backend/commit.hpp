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

// Low-level escape hatch for already-coerced packed values.
// Use when caller has performed RMW or type coercion and has final LLVM value.
// Does NOT accept TypeId or OwnershipPolicy - caller asserts value is ready.
// Resolves WriteTarget internally, stores, notifies if design slot.
void CommitPackedValueRaw(
    Context& ctx, mir::PlaceId target, llvm::Value* value);

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

namespace detail {

// Field-level store for struct field-by-field assignment.
// No WriteTarget (fields don't have signal_id), no notify.
void CommitStringField(
    Context& ctx, llvm::Value* ptr, llvm::Value* handle, OwnershipPolicy policy,
    TypeId type_id);

void CommitPlainField(
    Context& ctx, llvm::Value* ptr, llvm::Value* value, TypeId type_id);

}  // namespace detail

}  // namespace lyra::lowering::mir_to_llvm
