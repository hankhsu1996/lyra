#include <expected>

#include <llvm/IR/DerivedTypes.h>

#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace detail {

// Store a string handle to a field pointer with ownership handling.
// Unlike WriteTarget-based stores, this takes a raw pointer (not WriteTarget)
// because field-by-field assignment doesn't have WriteTarget per field.
//
// This function handles:
// - Clone: retain the handle
// - Move: use handle directly (caller already nulled source)
// - Destroy old value at target
// - Store new handle
//
// NOTE: Does NOT handle design-slot notify (design slots with string-containing
// structs are rejected at AssignStruct level).
void CommitStringField(
    Context& ctx, llvm::Value* ptr, llvm::Value* handle, OwnershipPolicy policy,
    TypeId type_id) {
  // Apply ownership policy via CloneValue
  if (policy == OwnershipPolicy::kClone) {
    handle = CloneValue(ctx, handle, type_id);
  }
  // kMove: handle already has ownership, no retain needed

  // Destroy old value and store new
  Destroy(ctx, ptr, type_id);
  ctx.GetBuilder().CreateStore(handle, ptr);
}

}  // namespace detail

namespace {

// Store a string handle to a WriteTarget.
// Handles destroy-old, store-new, and notify if design place.
// The new_val must already have the correct ownership (retained if needed).
void StoreStringToWriteTarget(
    Context& ctx, llvm::Value* new_val, const WriteTarget& wt, TypeId type_id) {
  auto& builder = ctx.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx.GetLlvmContext());

  if (wt.canonical_signal_id.has_value()) {
    // Design slot: load old first, then atomic store+notify, then release old
    auto* old_val = builder.CreateLoad(ptr_ty, wt.ptr, "str.old");
    auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());
    builder.CreateCall(
        ctx.GetLyraStoreString(),
        {ctx.GetEnginePointer(), wt.ptr, new_val,
         llvm::ConstantInt::get(i32_ty, *wt.canonical_signal_id)});
    builder.CreateCall(ctx.GetLyraStringRelease(), {old_val});
  } else {
    // Non-design: destroy old, store new
    Destroy(ctx, wt.ptr, type_id);
    builder.CreateStore(new_val, wt.ptr);
  }
}

}  // namespace

// String store: clone if kClone, then store via WriteTarget
auto CommitStringValue(
    Context& ctx, const WriteTarget& wt, llvm::Value* handle,
    OwnershipPolicy policy, TypeId type_id) -> Result<void> {
  if (policy == OwnershipPolicy::kClone) {
    handle = CloneValue(ctx, handle, type_id);
  }
  // kMove: handle already has ownership, no retain needed
  StoreStringToWriteTarget(ctx, handle, wt, type_id);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
