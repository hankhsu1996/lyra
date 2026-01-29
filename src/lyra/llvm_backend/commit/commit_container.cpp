#include <llvm/IR/DerivedTypes.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Store a dynamic array handle to a WriteTarget.
// Handles destroy-old, store-new, and notify if design place.
// The new_handle must already have the correct ownership (cloned if needed).
void StoreDynArrayToWriteTarget(
    Context& ctx, llvm::Value* new_handle, const WriteTarget& wt,
    TypeId type_id) {
  auto& builder = ctx.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx.GetLlvmContext());

  if (wt.canonical_signal_id.has_value()) {
    // Design slot: load old first, then atomic store+notify, then release old
    auto* old_handle = builder.CreateLoad(ptr_ty, wt.ptr, "da.old");
    auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());
    builder.CreateCall(
        ctx.GetLyraStoreDynArray(),
        {ctx.GetEnginePointer(), wt.ptr, new_handle,
         llvm::ConstantInt::get(i32_ty, *wt.canonical_signal_id)});
    builder.CreateCall(ctx.GetLyraDynArrayRelease(), {old_handle});
  } else {
    // Non-design: destroy old, store new
    Destroy(ctx, wt.ptr, type_id);
    builder.CreateStore(new_handle, wt.ptr);
  }
}

}  // namespace

// Container (DynArray, Queue) store: clone if kClone, then store via
// WriteTarget
auto CommitContainerValue(
    Context& ctx, const WriteTarget& wt, llvm::Value* handle,
    OwnershipPolicy policy, TypeId type_id) -> Result<void> {
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[type_id];

  // HARD REQUIREMENT: Only kDynamicArray and kQueue use this path.
  // If ManagedKind::kContainer expands to other types, they need their own
  // path.
  if (type.Kind() != TypeKind::kDynamicArray &&
      type.Kind() != TypeKind::kQueue) {
    throw common::InternalError(
        "CommitContainerValue", "called with non-container type");
  }

  if (policy == OwnershipPolicy::kClone) {
    handle = CloneValue(ctx, handle, type_id);
  }
  // kMove: handle already has ownership, no clone needed
  StoreDynArrayToWriteTarget(ctx, handle, wt, type_id);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
