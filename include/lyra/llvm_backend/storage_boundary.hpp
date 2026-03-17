#pragma once

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/llvm_backend/layout/storage_contract.hpp"

namespace lyra::lowering::mir_to_llvm {

// Persistent storage ABI boundary.
//
// These helpers materialize between canonical arena storage bytes and
// transient SSA typed values. SSA typed values ({iN, iN} for 4-state,
// iN for 2-state, float/double) exist only in registers. No typed LLVM
// object is ever stored directly to arena bytes.
//
// All decisions are driven by SlotStorageSpec. No LLVM type introspection.
//
// SSA contract for packed values at the storage boundary:
// - 2-state: iN where N = GetStorageByteSize(bit_width) * 8
// - 4-state: {iN, iN} where N = GetStorageByteSize(bit_width) * 8
// - float: float (4 bytes) or double (8 bytes)
//
// Values from the compute pipeline may be at semantic bit width (e.g. i130)
// rather than storage lane width (e.g. i136). Callers must invoke
// LowerToStorageLaneWidth before passing values to store/flatten helpers.

// Lower a packed SSA value from semantic bit width to storage lane width.
//
// This is an explicit lowering step that callers must invoke before calling
// EmitStoreToCanonicalStorage or EmitPackedToCanonicalBits. The storage
// boundary helpers require values at storage lane width and will assert
// on mismatch.
//
// - 2-state iM -> iN (ZExtOrTrunc where N = storage lane bits)
// - 4-state {iM, iM} -> {iN, iN} (ZExtOrTrunc each lane)
// - Already at storage width: identity (no-op)
//
// Invariant: value must be an integer (2-state) or {iN, iN} struct
// (4-state). Anything else is a hard failure.
auto LowerToStorageLaneWidth(
    llvm::IRBuilderBase& builder, llvm::Value* value,
    const PackedStorageSpec& spec) -> llvm::Value*;

// Load a value from canonical storage into SSA typed form.
//
// - PackedStorageSpec 2-state: load(iN, slot_ptr)
// - PackedStorageSpec 4-state: load known lane at offset 0,
//   load unknown lane at spec.UnknownLaneOffset(), pack into {iN, iN}
// - FloatStorageSpec 4 bytes: load(float, slot_ptr)
// - FloatStorageSpec 8 bytes: load(double, slot_ptr)
//
// Aggregates are not loaded at slot level. Aggregate storage is
// traversed through canonical field/element specs via projection chains.
auto EmitLoadFromCanonicalStorage(
    llvm::IRBuilderBase& builder, llvm::Value* slot_ptr,
    const SlotStorageSpec& spec) -> llvm::Value*;

// Store an SSA typed value to canonical storage.
//
// Precondition: packed values must be at storage lane width. Callers
// must invoke LowerToStorageLaneWidth first. Mismatched width is a
// hard invariant failure.
//
// Scalar specs (PackedStorageSpec, FloatStorageSpec):
// - PackedStorageSpec 2-state: store(value, slot_ptr)
// - PackedStorageSpec 4-state: extractvalue known + unknown,
//   store each at dense lane offsets
// - FloatStorageSpec: store(value, slot_ptr)
//
// Aggregate specs (ArrayStorageSpec, StructStorageSpec):
// - Recursively extract each element/field from the SSA value
//   and store to the correct canonical byte offset.
// - Requires arena for child spec resolution.
auto EmitStoreToCanonicalStorage(
    llvm::IRBuilderBase& builder, llvm::Value* slot_ptr, llvm::Value* value,
    const SlotStorageSpec& spec, const StorageSpecArena& arena) -> void;

// Flatten a packed SSA value to its canonical bit representation as a
// single LLVM integer. Used for inline commit compare/store fast path.
//
// Precondition: value must be at storage lane width. Callers must
// invoke LowerToStorageLaneWidth first. Mismatched width is a hard
// invariant failure.
//
// - PackedStorageSpec 2-state iN: identity
// - PackedStorageSpec 4-state {iN, iN}: extractvalue + zext + shift + or
//   -> i(2N), with value lane in low bits and unknown lane in high bits
// - FloatStorageSpec: bitcast to same-width integer
//
// Decision driven by spec, not LLVM type shape.
auto EmitPackedToCanonicalBits(
    llvm::IRBuilderBase& builder, llvm::Value* value,
    const SlotStorageSpec& spec) -> llvm::Value*;

// Store X-encoded value (value=0, unknown=semantic_mask) to canonical
// 4-state packed storage. Uses canonical byte-offset addressing only.
void EmitStoreFourStateXToCanonical(
    llvm::IRBuilderBase& builder, llvm::Value* slot_ptr, uint32_t bit_width);

// Store the unknown-plane mask to canonical 4-state packed storage.
// Value plane is assumed to already be zero (e.g., after memset).
// Uses canonical byte-offset addressing only.
void EmitStoreUnknownMaskToCanonical(
    llvm::IRBuilderBase& builder, llvm::Value* slot_ptr, uint32_t bit_width);

// Load a 4-state packed value from canonical storage into SSA {iN, iN} form.
// Loads planes separately via byte GEP using canonical offsets.
// Returns the packed struct value.
// For use when the caller knows the type is 4-state packed and needs
// the SSA struct form (e.g., for assignment, display, etc.).
auto EmitLoadFourStateFromCanonical(
    llvm::IRBuilderBase& builder, llvm::LLVMContext& llvm_ctx,
    llvm::Value* slot_ptr, uint32_t bit_width) -> llvm::Value*;

}  // namespace lyra::lowering::mir_to_llvm
