#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/llvm_backend/context.hpp"

namespace lyra::lowering::mir_to_llvm::detail {

namespace {

// Get the handle type from the runtime function prototype.
// All string runtime functions use the same opaque pointer type.
auto GetStringHandleType(Context& ctx) -> llvm::Type* {
  return ctx.GetLyraStringRelease()->getFunctionType()->getParamType(0);
}

}  // namespace

void DestroyString(Context& ctx, llvm::Value* ptr) {
  auto* handle_ty = GetStringHandleType(ctx);
  auto* handle = ctx.GetBuilder().CreateLoad(handle_ty, ptr, "destroy.str");
  ctx.GetBuilder().CreateCall(ctx.GetLyraStringRelease(), {handle});
}

auto CloneString(Context& ctx, llvm::Value* handle) -> llvm::Value* {
  return ctx.GetBuilder().CreateCall(ctx.GetLyraStringRetain(), {handle});
}

void MoveCleanupString(Context& ctx, llvm::Value* ptr) {
  auto* handle_ty = GetStringHandleType(ctx);
  auto* null_val =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(handle_ty));
  ctx.GetBuilder().CreateStore(null_val, ptr);
}

void CopyInitString(Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr) {
  auto& builder = ctx.GetBuilder();
  auto* handle_ty = GetStringHandleType(ctx);

  // Load source handle, clone/retain it, store to dst
  auto* src_handle = builder.CreateLoad(handle_ty, src_ptr, "copy.str.src");
  auto* cloned = builder.CreateCall(ctx.GetLyraStringRetain(), {src_handle});
  builder.CreateStore(cloned, dst_ptr);
}

void MoveInitString(Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr) {
  auto& builder = ctx.GetBuilder();
  auto* handle_ty = GetStringHandleType(ctx);

  // Move: load handle from src, store to dst, null out src
  auto* handle = builder.CreateLoad(handle_ty, src_ptr, "move.str");
  builder.CreateStore(handle, dst_ptr);

  // Null out source (inline, not via MoveCleanup)
  auto* null_val =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(handle_ty));
  builder.CreateStore(null_val, src_ptr);
}

}  // namespace lyra::lowering::mir_to_llvm::detail
