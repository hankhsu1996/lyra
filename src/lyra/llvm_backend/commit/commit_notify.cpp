#include <optional>

#include <llvm/IR/Constants.h>
#include <llvm/IR/Type.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

void CommitNotifyMutationIfDesignSlot(Context& ctx, mir::PlaceId target) {
  // Init contract: no engine, no notification needed.
  if (ctx.GetDesignStoreMode() == DesignStoreMode::kDirectInit) return;

  auto mutation_sig = ctx.ResolveMutationSignalRef(target);
  if (!mutation_sig.has_value()) return;

  bool static_hit = ctx.RequiresStaticDirtyPropagation(*mutation_sig);

  if (ctx.GetNotificationPolicy() == NotificationPolicy::kDeferred) {
    throw common::InternalError(
        "CommitNotifyMutationIfDesignSlot",
        "deferred notification not supported for container mutation path");
  }

  auto signal_id = ctx.EmitMutationTargetSignalCoord(*mutation_sig);
  auto& builder = ctx.GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());

  auto emit_notify = [&]() {
    if (signal_id.IsLocal()) {
      builder.CreateCall(
          ctx.GetLyraNotifyContainerMutationLocal(),
          {ctx.GetEnginePointer(),
           signal_id.GetInstancePointer(ctx.GetInstancePointer()),
           signal_id.Emit(builder), llvm::ConstantInt::get(i32_ty, 1),
           llvm::ConstantInt::get(i32_ty, 0)});
    } else {
      builder.CreateCall(
          ctx.GetLyraNotifyContainerMutationGlobal(),
          {ctx.GetEnginePointer(), signal_id.Emit(builder),
           llvm::ConstantInt::get(i32_ty, 1),
           llvm::ConstantInt::get(i32_ty, 0)});
    }
  };

  if (static_hit) {
    emit_notify();
  } else {
    ctx.EmitTraceBranch(
        *mutation_sig, "notify.mutation", "notify.skip", emit_notify, []() {});
  }
}

auto GetDesignSignalCoord(Context& ctx, mir::PlaceId target)
    -> std::optional<SignalCoordExpr> {
  return commit::Access::GetMutationTargetSignalCoord(ctx, target);
}

auto GetSignalCoordForNba(Context& ctx, mir::PlaceId target)
    -> SignalCoordExpr {
  auto signal_id_opt =
      commit::Access::GetMutationTargetSignalCoord(ctx, target);
  if (!signal_id_opt.has_value()) {
    throw common::InternalError(
        "GetSignalCoordForNba", "NBA target must resolve to a design slot");
  }
  return *signal_id_opt;
}

void CommitNotifyAggregateIfDesignSlot(Context& ctx, mir::PlaceId target) {
  // Init contract: no engine, no notification needed.
  if (ctx.GetDesignStoreMode() == DesignStoreMode::kDirectInit) return;

  auto mutation_sig = ctx.ResolveMutationSignalRef(target);
  if (!mutation_sig.has_value()) return;

  bool static_hit = ctx.RequiresStaticDirtyPropagation(*mutation_sig);

  if (ctx.GetNotificationPolicy() == NotificationPolicy::kDeferred) {
    throw common::InternalError(
        "CommitNotifyAggregateIfDesignSlot",
        "deferred notification not supported for aggregate notify path");
  }

  auto signal_id = ctx.EmitMutationTargetSignalCoord(*mutation_sig);

  auto target_ptr_or_err = ctx.GetPlacePointer(target);
  if (!target_ptr_or_err) {
    throw common::InternalError(
        "CommitNotifyAggregateIfDesignSlot",
        "failed to get target place pointer");
  }

  auto& builder = ctx.GetBuilder();

  auto emit_notify = [&]() {
    if (signal_id.IsLocal()) {
      builder.CreateCall(
          ctx.GetLyraNotifySignalLocal(),
          {ctx.GetEnginePointer(),
           signal_id.GetInstancePointer(ctx.GetInstancePointer()),
           *target_ptr_or_err, signal_id.Emit(builder)});
    } else {
      builder.CreateCall(
          ctx.GetLyraNotifySignalGlobal(),
          {ctx.GetEnginePointer(), *target_ptr_or_err,
           signal_id.Emit(builder)});
    }
  };

  if (static_hit) {
    emit_notify();
  } else {
    ctx.EmitTraceBranch(
        *mutation_sig, "notify.aggregate", "notify.skip", emit_notify, []() {});
  }
}

void CommitNotifyAggregateIfDesignSlot(
    Context& ctx, const mir::WriteTarget& target) {
  if (ctx.GetDesignStoreMode() == DesignStoreMode::kDirectInit) return;

  auto wt_or_err = commit::Access::GetWriteTarget(ctx, target);
  if (!wt_or_err) return;
  const auto& wt = *wt_or_err;
  if (!wt.canonical_signal_id.has_value()) return;

  bool static_hit = wt.requires_static_dirty_propagation;
  auto& builder = ctx.GetBuilder();

  auto emit_notify = [&]() {
    if (wt.canonical_signal_id->IsLocal()) {
      builder.CreateCall(
          ctx.GetLyraNotifySignalLocal(),
          {ctx.GetEnginePointer(),
           wt.canonical_signal_id->GetInstancePointer(ctx.GetInstancePointer()),
           wt.ptr, wt.canonical_signal_id->Emit(builder)});
    } else {
      builder.CreateCall(
          ctx.GetLyraNotifySignalGlobal(),
          {ctx.GetEnginePointer(), wt.ptr,
           wt.canonical_signal_id->Emit(builder)});
    }
  };

  if (static_hit) {
    emit_notify();
  } else if (wt.mutation_signal.has_value()) {
    ctx.EmitTraceBranch(
        *wt.mutation_signal, "notify.aggregate", "notify.skip", emit_notify,
        []() {});
  }
}

}  // namespace lyra::lowering::mir_to_llvm
