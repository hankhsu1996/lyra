#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/lifecycle/detail.hpp"
#include "lyra/llvm_backend/ownership.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Store a container handle to a WriteTarget.
// Handles destroy-old, store-new, and notify if design place.
// The new_handle must already have the correct ownership (cloned if needed).
void StoreContainerToWriteTarget(
    Context& ctx, const CuFacts& facts, llvm::Value* new_handle,
    const WriteTarget& wt, TypeId type_id) {
  auto& builder = ctx.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx.GetLlvmContext());

  bool is_design_notify =
      wt.canonical_signal_id.has_value() &&
      ctx.GetDesignStoreMode() != DesignStoreMode::kDirectInit;

  auto emit_notifying_store = [&]() {
    auto* old_handle = builder.CreateLoad(ptr_ty, wt.ptr, "ctr.old");
    if (wt.canonical_signal_id->IsExtRef()) {
      builder.CreateCall(
          ctx.GetLyraStoreDynArrayGlobal(),
          {ctx.GetEnginePointer(), wt.ptr, new_handle,
           builder.getInt32(UINT32_MAX)});
      builder.CreateCall(
          ctx.GetLyraMarkDirtyExtRef(),
          {ctx.GetEnginePointer(), ctx.GetInstancePointer(),
           wt.canonical_signal_id->Emit(builder), builder.getInt32(0),
           builder.getInt32(0)});
    } else if (wt.canonical_signal_id->IsLocal()) {
      auto* inst_ptr =
          wt.canonical_signal_id->GetInstancePointer(ctx.GetInstancePointer());
      builder.CreateCall(
          ctx.GetLyraStoreDynArrayLocal(),
          {ctx.GetEnginePointer(), wt.ptr, new_handle, inst_ptr,
           wt.canonical_signal_id->Emit(builder)});
    } else {
      builder.CreateCall(
          ctx.GetLyraStoreDynArrayGlobal(),
          {ctx.GetEnginePointer(), wt.ptr, new_handle,
           wt.canonical_signal_id->Emit(builder)});
    }
    const auto& types = *facts.types;
    if (types[type_id].Kind() == TypeKind::kAssociativeArray) {
      builder.CreateCall(ctx.GetLyraAssocRelease(), {old_handle});
    } else {
      builder.CreateCall(ctx.GetLyraDynArrayRelease(), {old_handle});
    }
  };

  auto emit_plain_store = [&]() {
    Destroy(ctx, facts, wt.ptr, type_id);
    builder.CreateStore(new_handle, wt.ptr);
  };

  if (is_design_notify) {
    if (ctx.GetNotificationPolicy() == NotificationPolicy::kDeferred) {
      throw common::InternalError(
          "StoreContainerToWriteTarget",
          "deferred notification not supported for container store path");
    }
    if (wt.requires_static_dirty_propagation ||
        !wt.mutation_signal.has_value()) {
      emit_notifying_store();
    } else {
      ctx.EmitTraceBranch(
          *wt.mutation_signal, "ctr.notify", "ctr.plain", emit_notifying_store,
          emit_plain_store);
    }
  } else {
    emit_plain_store();
  }
}

}  // namespace

// Container (DynArray, Queue) store: clone if kClone, then store via
// WriteTarget
auto CommitContainerValue(
    Context& ctx, const CuFacts& facts, const WriteTarget& wt,
    llvm::Value* handle, OwnershipPolicy policy, TypeId type_id)
    -> Result<void> {
  const auto& types = *facts.types;
  const Type& type = types[type_id];

  if (type.Kind() != TypeKind::kDynamicArray &&
      type.Kind() != TypeKind::kQueue &&
      type.Kind() != TypeKind::kAssociativeArray) {
    throw common::InternalError(
        "CommitContainerValue", "called with non-container type");
  }

  if (policy == OwnershipPolicy::kClone) {
    handle = CloneLeafValue(ctx, facts, handle, type_id);
  }
  // kMove: handle already has ownership, no clone needed
  StoreContainerToWriteTarget(ctx, facts, handle, wt, type_id);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
