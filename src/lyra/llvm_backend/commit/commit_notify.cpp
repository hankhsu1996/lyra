#include <cstdint>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/context.hpp"

namespace lyra::lowering::mir_to_llvm {

void CommitNotifyUnionMemcpyIfDesignSlot(
    Context& ctx, mir::PlaceId target, uint32_t byte_size) {
  auto wt_or_err = ctx.GetWriteTarget(target);
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
       llvm::ConstantInt::get(i32_ty, *wt.canonical_signal_id)});
}

void CommitNotifyMutationIfDesignSlot(Context& ctx, mir::PlaceId target) {
  auto signal_id_opt = ctx.GetCanonicalRootSignalId(target);

  // Conditional: no-op if not design slot
  if (!signal_id_opt.has_value()) {
    return;
  }

  auto& builder = ctx.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx.GetLlvmContext());
  auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());

  auto recv_ptr_or_err = ctx.GetPlacePointer(target);
  if (!recv_ptr_or_err) {
    throw common::InternalError(
        "CommitNotifyMutationIfDesignSlot",
        "failed to get target place pointer");
  }
  llvm::Value* recv_ptr = *recv_ptr_or_err;

  llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "notify.h");
  builder.CreateCall(
      ctx.GetLyraStoreDynArray(),
      {ctx.GetEnginePointer(), recv_ptr, handle,
       llvm::ConstantInt::get(i32_ty, *signal_id_opt)});
}

auto GetSignalIdForNba(Context& ctx, mir::PlaceId target) -> uint32_t {
  auto signal_id_opt = ctx.GetCanonicalRootSignalId(target);
  if (!signal_id_opt.has_value()) {
    throw common::InternalError(
        "GetSignalIdForNba", "NBA target must resolve to a design slot");
  }
  return *signal_id_opt;
}

}  // namespace lyra::lowering::mir_to_llvm
