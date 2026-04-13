#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"

namespace lyra::lowering::mir_to_llvm::detail {

namespace {

auto GetHandlePtrType(llvm::IRBuilder<>& builder) -> llvm::PointerType* {
  return llvm::PointerType::getUnqual(builder.getContext());
}

}  // namespace

void DestroyContainer(
    llvm::IRBuilder<>& builder, const CuFacts& facts,
    llvm::Function* assoc_release_fn, llvm::Function* dynarray_release_fn,
    llvm::Value* ptr, TypeId type_id) {
  auto* handle_ty = GetHandlePtrType(builder);
  auto* handle = builder.CreateLoad(handle_ty, ptr, "destroy.ctr");

  const auto& types = *facts.types;
  if (types[type_id].Kind() == TypeKind::kAssociativeArray) {
    builder.CreateCall(assoc_release_fn, {handle});
  } else {
    builder.CreateCall(dynarray_release_fn, {handle});
  }
}

auto CloneContainer(
    llvm::IRBuilder<>& builder, const CuFacts& facts,
    llvm::Function* assoc_clone_fn, llvm::Function* dynarray_clone_fn,
    llvm::Value* handle, TypeId type_id) -> llvm::Value* {
  const auto& types = *facts.types;
  if (types[type_id].Kind() == TypeKind::kAssociativeArray) {
    return builder.CreateCall(assoc_clone_fn, {handle});
  }
  return builder.CreateCall(dynarray_clone_fn, {handle});
}

void MoveCleanupContainer(llvm::IRBuilder<>& builder, llvm::Value* ptr) {
  auto* handle_ty = GetHandlePtrType(builder);
  auto* null_val =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(handle_ty));
  builder.CreateStore(null_val, ptr);
}

void CopyInitContainer(
    llvm::IRBuilder<>& builder, const CuFacts& facts,
    llvm::Function* dynarray_clone_fn, llvm::Function* assoc_clone_fn,
    llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id) {
  const auto& types = *facts.types;
  const Type& type = types[type_id];

  auto* handle_ty = GetHandlePtrType(builder);
  auto* src_handle = builder.CreateLoad(handle_ty, src_ptr, "copy.ctr.src");

  // Type-directed dispatch: each container type has its own clone function.
  // No assumptions about representation equality between types.
  llvm::Value* cloned = nullptr;
  switch (type.Kind()) {
    case TypeKind::kDynamicArray:
      cloned = builder.CreateCall(dynarray_clone_fn, {src_handle});
      break;
    case TypeKind::kQueue:
      throw common::InternalError(
          "CopyInitContainer", "queue clone is not implemented");
    case TypeKind::kAssociativeArray:
      cloned = builder.CreateCall(assoc_clone_fn, {src_handle});
      break;
    default:
      throw common::InternalError(
          "CopyInitContainer", "unexpected type kind for container");
  }

  builder.CreateStore(cloned, dst_ptr);
}

void MoveInitContainer(
    llvm::IRBuilder<>& builder, llvm::Value* dst_ptr, llvm::Value* src_ptr) {
  auto* handle_ty = GetHandlePtrType(builder);

  // Move: load handle from src, store to dst, null out src
  auto* handle = builder.CreateLoad(handle_ty, src_ptr, "move.ctr");
  builder.CreateStore(handle, dst_ptr);

  // Null out source (inline, not via MoveCleanup)
  auto* null_val =
      llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(handle_ty));
  builder.CreateStore(null_val, src_ptr);
}

}  // namespace lyra::lowering::mir_to_llvm::detail
