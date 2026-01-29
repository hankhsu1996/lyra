#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/ownership.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace detail {

// Store a string handle to a field pointer with ownership handling.
// Unlike WriteTarget-based stores, this takes a raw pointer (not WriteTarget)
// because field-by-field assignment doesn't have WriteTarget per field.
//
// Order: Load(old) → Retain(new if clone) → Store(new) → Release(old)
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
// Order: Load(old) → Store(new) → Release(old)
// The new_val must already have the correct ownership (retained if needed).
void StoreStringToWriteTarget(
    Context& ctx, llvm::Value* new_val, const WriteTarget& wt) {
  auto& builder = ctx.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx.GetLlvmContext());

  // Order: Load(old) → Store(new) → Release(old)
  // Safe for aliasing: if new == old, store completes before release.
  auto* old_val = builder.CreateLoad(ptr_ty, wt.ptr, "str.old");

  if (wt.canonical_signal_id.has_value()) {
    // Design slot: atomic store+notify
    auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());
    builder.CreateCall(
        ctx.GetLyraStoreString(),
        {ctx.GetEnginePointer(), wt.ptr, new_val,
         llvm::ConstantInt::get(i32_ty, *wt.canonical_signal_id)});
  } else {
    // Non-design: plain store
    builder.CreateStore(new_val, wt.ptr);
  }

  builder.CreateCall(ctx.GetLyraStringRelease(), {old_val});
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
  StoreStringToWriteTarget(ctx, handle, wt);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
