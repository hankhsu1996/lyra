#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

namespace lyra::lowering::mir_to_llvm::detail {

namespace {

auto GetHandlePtrType(llvm::IRBuilder<>& builder) -> llvm::PointerType* {
  return llvm::PointerType::getUnqual(builder.getContext());
}

}  // namespace

void DestroyString(
    llvm::IRBuilder<>& builder, llvm::Function* release_fn, llvm::Value* ptr) {
  auto* handle_ty = GetHandlePtrType(builder);
  auto* handle = builder.CreateLoad(handle_ty, ptr, "destroy.str");
  builder.CreateCall(release_fn, {handle});
}

auto CloneString(
    llvm::IRBuilder<>& builder, llvm::Function* retain_fn, llvm::Value* handle)
    -> llvm::Value* {
  return builder.CreateCall(retain_fn, {handle});
}

void MoveCleanupString(llvm::IRBuilder<>& builder, llvm::Value* ptr) {
  auto* handle_ty = GetHandlePtrType(builder);
  auto* null_val =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(handle_ty));
  builder.CreateStore(null_val, ptr);
}

void CopyInitString(
    llvm::IRBuilder<>& builder, llvm::Function* retain_fn, llvm::Value* dst_ptr,
    llvm::Value* src_ptr) {
  auto* handle_ty = GetHandlePtrType(builder);

  // Load source handle, clone/retain it, store to dst
  auto* src_handle = builder.CreateLoad(handle_ty, src_ptr, "copy.str.src");
  auto* cloned = builder.CreateCall(retain_fn, {src_handle});
  builder.CreateStore(cloned, dst_ptr);
}

void MoveInitString(
    llvm::IRBuilder<>& builder, llvm::Value* dst_ptr, llvm::Value* src_ptr) {
  auto* handle_ty = GetHandlePtrType(builder);

  // Move: load handle from src, store to dst, null out src
  auto* handle = builder.CreateLoad(handle_ty, src_ptr, "move.str");
  builder.CreateStore(handle, dst_ptr);

  // Null out source (inline, not via MoveCleanup)
  auto* null_val =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(handle_ty));
  builder.CreateStore(null_val, src_ptr);
}

}  // namespace lyra::lowering::mir_to_llvm::detail
