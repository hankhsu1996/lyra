#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/lifecycle/detail.hpp"
#include "lyra/llvm_backend/ownership.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace detail {

// Store a string handle to a field pointer with ownership handling.
// Unlike WriteTarget-based stores, this takes a raw pointer (not WriteTarget)
// because field-by-field assignment doesn't have WriteTarget per field.
//
// Order: Load(old) -> Retain(new if clone) -> Store(new) -> Release(old)
// This order is safe for aliasing: if new == old, we've retained before
// release.
//
// NOTE: Does NOT handle design-slot notify (design slots with string-containing
// structs are rejected at AssignStruct level).
void CommitStringField(
    Context& ctx, llvm::Value* ptr, llvm::Value* handle,
    OwnershipPolicy policy) {
  auto& builder = ctx.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx.GetLlvmContext());

  // Load old handle first (before any mutation)
  auto* old_handle = builder.CreateLoad(ptr_ty, ptr, "sf.old");

  // Apply ownership policy
  if (policy == OwnershipPolicy::kClone) {
    handle = builder.CreateCall(ctx.GetLyraStringRetain(), {handle});
  }
  // kMove: handle already has ownership, no retain needed

  // Store new handle, then release old
  builder.CreateStore(handle, ptr);
  builder.CreateCall(ctx.GetLyraStringRelease(), {old_handle});
}

}  // namespace detail

namespace {

// Store a string handle to a WriteTarget.
// Order: Load(old) -> Store(new) -> Release(old)
// The new_val must already have the correct ownership (retained if needed).
void StoreStringToWriteTarget(
    Context& ctx, llvm::Value* new_val, const WriteTarget& wt) {
  auto& builder = ctx.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx.GetLlvmContext());

  // Order: Load(old) -> Store(new) -> Release(old)
  // Safe for aliasing: if new == old, store completes before release.
  auto* old_val = builder.CreateLoad(ptr_ty, wt.ptr, "str.old");

  bool is_design_notify =
      wt.canonical_signal_id.has_value() &&
      ctx.GetDesignStoreMode() != DesignStoreMode::kDirectInit;

  auto emit_notifying_store = [&]() {
    if (ctx.GetNotificationPolicy() == NotificationPolicy::kDeferred) {
      throw common::InternalError(
          "StoreStringToWriteTarget",
          "deferred notification not supported for string store path");
    }
    if (wt.canonical_signal_id->IsExtRef()) {
      builder.CreateStore(new_val, wt.ptr);
      builder.CreateCall(
          ctx.GetLyraMarkDirtyExtRef(),
          {ctx.GetEnginePointer(), ctx.GetInstancePointer(),
           wt.canonical_signal_id->Emit(builder), builder.getInt32(0),
           builder.getInt32(0)});
    } else if (wt.canonical_signal_id->IsLocal()) {
      builder.CreateCall(
          ctx.GetLyraStoreStringLocal(),
          {ctx.GetEnginePointer(),
           wt.canonical_signal_id->GetInstancePointer(ctx.GetInstancePointer()),
           wt.ptr, new_val, wt.canonical_signal_id->Emit(builder)});
    } else {
      builder.CreateCall(
          ctx.GetLyraStoreStringGlobal(),
          {ctx.GetEnginePointer(), wt.ptr, new_val,
           wt.canonical_signal_id->Emit(builder)});
    }
  };

  auto emit_plain_store = [&]() { builder.CreateStore(new_val, wt.ptr); };

  if (is_design_notify && wt.requires_static_dirty_propagation) {
    emit_notifying_store();
  } else if (is_design_notify && !wt.mutation_signal.has_value()) {
    emit_notifying_store();
  } else if (is_design_notify) {
    ctx.EmitTraceBranch(
        *wt.mutation_signal, "str.notify", "str.plain", emit_notifying_store,
        emit_plain_store);
  } else {
    emit_plain_store();
  }

  builder.CreateCall(ctx.GetLyraStringRelease(), {old_val});
}

}  // namespace

// String store: clone if kClone, then store via WriteTarget
auto CommitStringValue(
    Context& ctx, const CuFacts& facts, const WriteTarget& wt,
    llvm::Value* handle, OwnershipPolicy policy, TypeId type_id)
    -> Result<void> {
  if (policy == OwnershipPolicy::kClone) {
    handle = CloneLeafValue(ctx, facts, handle, type_id);
  }
  // kMove: handle already has ownership, no retain needed
  StoreStringToWriteTarget(ctx, handle, wt);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
