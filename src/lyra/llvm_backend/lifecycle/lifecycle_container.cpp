#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"

namespace lyra::lowering::mir_to_llvm::detail {

namespace {

// Get the handle type from the runtime function prototype.
// All container runtime functions use the same opaque pointer type.
auto GetContainerHandleType(Context& ctx) -> llvm::Type* {
  return ctx.GetLyraDynArrayRelease()->getFunctionType()->getParamType(0);
}

}  // namespace

void DestroyContainer(Context& ctx, llvm::Value* ptr, TypeId type_id) {
  auto& builder = ctx.GetBuilder();
  auto* handle_ty = GetContainerHandleType(ctx);
  auto* handle = builder.CreateLoad(handle_ty, ptr, "destroy.ctr");

  const auto& types = ctx.GetTypeArena();
  if (types[type_id].Kind() == TypeKind::kAssociativeArray) {
    builder.CreateCall(ctx.GetLyraAssocRelease(), {handle});
  } else {
    builder.CreateCall(ctx.GetLyraDynArrayRelease(), {handle});
  }
}

auto CloneContainer(Context& ctx, llvm::Value* handle, TypeId type_id)
    -> llvm::Value* {
  const auto& types = ctx.GetTypeArena();
  if (types[type_id].Kind() == TypeKind::kAssociativeArray) {
    return ctx.GetBuilder().CreateCall(ctx.GetLyraAssocClone(), {handle});
  }
  return ctx.GetBuilder().CreateCall(ctx.GetLyraDynArrayClone(), {handle});
}

void MoveCleanupContainer(Context& ctx, llvm::Value* ptr) {
  auto* handle_ty = GetContainerHandleType(ctx);
  auto* null_val =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(handle_ty));
  ctx.GetBuilder().CreateStore(null_val, ptr);
}

void CopyInitContainer(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id) {
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[type_id];

  auto& builder = ctx.GetBuilder();
  auto* handle_ty = GetContainerHandleType(ctx);
  auto* src_handle = builder.CreateLoad(handle_ty, src_ptr, "copy.ctr.src");

  // Type-directed dispatch: each container type has its own clone function.
  // No assumptions about representation equality between types.
  llvm::Value* cloned = nullptr;
  switch (type.Kind()) {
    case TypeKind::kDynamicArray:
      cloned = builder.CreateCall(ctx.GetLyraDynArrayClone(), {src_handle});
      break;
    case TypeKind::kQueue:
      throw common::InternalError(
          "CopyInitContainer", "queue clone is not implemented");
    case TypeKind::kAssociativeArray:
      cloned = builder.CreateCall(ctx.GetLyraAssocClone(), {src_handle});
      break;
    default:
      throw common::InternalError(
          "CopyInitContainer", "unexpected type kind for container");
  }

  builder.CreateStore(cloned, dst_ptr);
}

void MoveInitContainer(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr) {
  auto& builder = ctx.GetBuilder();
  auto* handle_ty = GetContainerHandleType(ctx);

  // Move: load handle from src, store to dst, null out src
  auto* handle = builder.CreateLoad(handle_ty, src_ptr, "move.ctr");
  builder.CreateStore(handle, dst_ptr);

  // Null out source (inline, not via MoveCleanup)
  auto* null_val =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(handle_ty));
  builder.CreateStore(null_val, src_ptr);
}

}  // namespace lyra::lowering::mir_to_llvm::detail
