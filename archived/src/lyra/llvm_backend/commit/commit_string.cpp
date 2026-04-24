#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
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
void CommitStringField(
    llvm::IRBuilder<>& builder, llvm::Function* retain_fn,
    llvm::Function* release_fn, llvm::Value* ptr, llvm::Value* handle,
    OwnershipPolicy policy) {
  auto* ptr_ty = llvm::PointerType::getUnqual(builder.getContext());

  // Load old handle first (before any mutation)
  auto* old_handle = builder.CreateLoad(ptr_ty, ptr, "sf.old");

  // Apply ownership policy
  if (policy == OwnershipPolicy::kClone) {
    handle = builder.CreateCall(retain_fn, {handle});
  }
  // kMove: handle already has ownership, no retain needed

  // Store new handle, then release old
  builder.CreateStore(handle, ptr);
  builder.CreateCall(release_fn, {old_handle});
}

}  // namespace detail

namespace {

// Routing-only: store a string handle to a WriteTarget with design-slot
// notification. Uses string-specific runtime store functions for atomicity.
// Does NOT handle lifecycle (no retain, no release).
void RouteStringStore(
    Context& ctx, llvm::Value* new_handle, const WriteTarget& wt) {
  auto& builder = ctx.GetBuilder();

  bool is_design_notify =
      wt.canonical_signal_id.has_value() &&
      ctx.GetDesignStoreMode() != DesignStoreMode::kDirectInit;

  auto emit_notifying_store = [&]() {
    if (ctx.GetNotificationPolicy() == NotificationPolicy::kDeferred) {
      throw common::InternalError(
          "RouteStringStore",
          "deferred notification not supported for string store path");
    }
    if (wt.canonical_signal_id->IsExtRef()) {
      builder.CreateStore(new_handle, wt.ptr);
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
           wt.ptr, new_handle, wt.canonical_signal_id->Emit(builder)});
    } else {
      builder.CreateCall(
          ctx.GetLyraStoreStringGlobal(),
          {ctx.GetEnginePointer(), wt.ptr, new_handle,
           wt.canonical_signal_id->Emit(builder)});
    }
  };

  auto emit_plain_store = [&]() { builder.CreateStore(new_handle, wt.ptr); };

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
}

}  // namespace

// String store with lifecycle.
// 1. Lifecycle: clone handle if kClone
// 2. Lifecycle: load old handle (for release after store)
// 3. Routing: store new handle via RouteStringStore
// 4. Lifecycle: release old handle
auto CommitStringValue(
    Context& ctx, const CuFacts& facts, const WriteTarget& wt,
    llvm::Value* handle, OwnershipPolicy policy, TypeId type_id)
    -> Result<void> {
  auto& builder = ctx.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx.GetLlvmContext());

  // 1. Lifecycle: clone if copy semantics
  if (policy == OwnershipPolicy::kClone) {
    handle = CloneLeafValue(ctx, facts, handle, type_id);
  }

  // 2. Lifecycle: load old handle before mutation
  auto* old_handle = builder.CreateLoad(ptr_ty, wt.ptr, "str.old");

  // 3. Routing: store new handle with design-slot notification
  RouteStringStore(ctx, handle, wt);

  // 4. Lifecycle: release old handle
  builder.CreateCall(ctx.GetLyraStringRelease(), {old_handle});
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
