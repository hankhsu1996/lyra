#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>

#include "lyra/llvm_backend/context.hpp"

namespace lyra::lowering::mir_to_llvm::detail {

namespace {

// Get the handle type from the runtime function prototype.
// All container runtime functions use the same opaque pointer type.
auto GetContainerHandleType(Context& ctx) -> llvm::Type* {
  return ctx.GetLyraDynArrayRelease()->getFunctionType()->getParamType(0);
}

}  // namespace

void DestroyContainer(Context& ctx, llvm::Value* ptr) {
  auto* handle_ty = GetContainerHandleType(ctx);
  auto* handle = ctx.GetBuilder().CreateLoad(handle_ty, ptr, "destroy.da");
  ctx.GetBuilder().CreateCall(ctx.GetLyraDynArrayRelease(), {handle});
}

auto CloneContainer(Context& ctx, llvm::Value* handle) -> llvm::Value* {
  return ctx.GetBuilder().CreateCall(ctx.GetLyraDynArrayClone(), {handle});
}

void MoveCleanupContainer(Context& ctx, llvm::Value* ptr) {
  auto* handle_ty = GetContainerHandleType(ctx);
  auto* null_val =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(handle_ty));
  ctx.GetBuilder().CreateStore(null_val, ptr);
}

}  // namespace lyra::lowering::mir_to_llvm::detail
