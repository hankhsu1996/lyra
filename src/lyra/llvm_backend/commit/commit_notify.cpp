#include <cstdint>
#include <optional>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

void CommitNotifyUnionMemcpyIfDesignSlot(
    Context& ctx, mir::PlaceId target, uint32_t byte_size) {
  auto wt_or_err = commit::Access::GetWriteTarget(ctx, target);
  if (!wt_or_err) {
    throw common::InternalError(
        "CommitNotifyUnionMemcpyIfDesignSlot",
        "failed to resolve WriteTarget for target");
  }
  const WriteTarget& wt = *wt_or_err;

  // Conditional: no-op if not design slot
  if (!wt.canonical_signal_id.has_value()) {
    return;
  }

  auto& builder = ctx.GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());
  builder.CreateCall(
      ctx.GetLyraStorePacked(),
      {ctx.GetEnginePointer(), wt.ptr,
       wt.ptr,  // For unions, source = target after memcpy
       llvm::ConstantInt::get(i32_ty, byte_size),
       llvm::ConstantInt::get(i32_ty, *wt.canonical_signal_id),
       llvm::ConstantInt::get(i32_ty, 0), llvm::ConstantInt::get(i32_ty, 0)});
}

void CommitNotifyMutationIfDesignSlot(Context& ctx, mir::PlaceId target) {
  auto signal_id_opt = commit::Access::GetCanonicalRootSignalId(ctx, target);

  // Conditional: no-op if not design slot
  if (!signal_id_opt.has_value()) {
    return;
  }

  auto& builder = ctx.GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());

  // kStructural = 1, off = 0, size = 0
  builder.CreateCall(
      ctx.GetLyraNotifyContainerMutation(),
      {ctx.GetEnginePointer(), llvm::ConstantInt::get(i32_ty, *signal_id_opt),
       llvm::ConstantInt::get(i32_ty, 1), llvm::ConstantInt::get(i32_ty, 0),
       llvm::ConstantInt::get(i32_ty, 0)});
}

auto GetDesignSlotId(Context& ctx, mir::PlaceId target)
    -> std::optional<uint32_t> {
  return commit::Access::GetCanonicalRootSignalId(ctx, target);
}

auto GetSignalIdForNba(Context& ctx, mir::PlaceId target) -> uint32_t {
  auto signal_id_opt = commit::Access::GetCanonicalRootSignalId(ctx, target);
  if (!signal_id_opt.has_value()) {
    throw common::InternalError(
        "GetSignalIdForNba", "NBA target must resolve to a design slot");
  }
  return *signal_id_opt;
}

void CommitNotifyAggregateIfDesignSlot(Context& ctx, mir::PlaceId target) {
  auto signal_id_opt = commit::Access::GetCanonicalRootSignalId(ctx, target);
  if (!signal_id_opt.has_value()) {
    return;  // No-op for non-design slots
  }

  auto target_ptr_or_err = ctx.GetPlacePointer(target);
  if (!target_ptr_or_err) {
    throw common::InternalError(
        "CommitNotifyAggregateIfDesignSlot",
        "failed to get target place pointer");
  }

  auto& builder = ctx.GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());

  builder.CreateCall(
      ctx.GetLyraNotifySignal(),
      {ctx.GetEnginePointer(), *target_ptr_or_err,
       llvm::ConstantInt::get(i32_ty, *signal_id_opt)});
}

}  // namespace lyra::lowering::mir_to_llvm
