#include <format>

#include <llvm/IR/Value.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/lifecycle/detail.hpp"
#include "lyra/llvm_backend/lifecycle/lifecycle_array.hpp"
#include "lyra/llvm_backend/lifecycle/lifecycle_pod.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace detail {

// String lifecycle helpers (defined in lifecycle_string.cpp)
void DestroyString(
    llvm::IRBuilder<>& builder, llvm::Function* release_fn, llvm::Value* ptr);
auto CloneString(
    llvm::IRBuilder<>& builder, llvm::Function* retain_fn, llvm::Value* handle)
    -> llvm::Value*;
void MoveCleanupString(llvm::IRBuilder<>& builder, llvm::Value* ptr);
void CopyInitString(
    llvm::IRBuilder<>& builder, llvm::Function* retain_fn, llvm::Value* dst_ptr,
    llvm::Value* src_ptr);
void MoveInitString(
    llvm::IRBuilder<>& builder, llvm::Value* dst_ptr, llvm::Value* src_ptr);

// Container lifecycle helpers (defined in lifecycle_container.cpp)
void DestroyContainer(
    llvm::IRBuilder<>& builder, const CuFacts& facts,
    llvm::Function* assoc_release_fn, llvm::Function* dynarray_release_fn,
    llvm::Value* ptr, TypeId type_id);
auto CloneContainer(
    llvm::IRBuilder<>& builder, const CuFacts& facts,
    llvm::Function* assoc_clone_fn, llvm::Function* dynarray_clone_fn,
    llvm::Value* handle, TypeId type_id) -> llvm::Value*;
void MoveCleanupContainer(llvm::IRBuilder<>& builder, llvm::Value* ptr);
void CopyInitContainer(
    llvm::IRBuilder<>& builder, const CuFacts& facts,
    llvm::Function* dynarray_clone_fn, llvm::Function* assoc_clone_fn,
    llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id);
void MoveInitContainer(
    llvm::IRBuilder<>& builder, llvm::Value* dst_ptr, llvm::Value* src_ptr);

// Struct lifecycle helpers (defined in lifecycle_struct.cpp)
void DestroyStruct(
    Context& ctx, const CuFacts& facts, llvm::Value* ptr, TypeId type_id);
void MoveCleanupStruct(
    Context& ctx, const CuFacts& facts, llvm::Value* ptr, TypeId type_id);
void CopyInitStruct(
    Context& ctx, const CuFacts& facts, llvm::Value* dst_ptr,
    llvm::Value* src_ptr, TypeId type_id);
void MoveInitStruct(
    Context& ctx, const CuFacts& facts, llvm::Value* dst_ptr,
    llvm::Value* src_ptr, TypeId type_id);

}  // namespace detail

void Destroy(
    Context& ctx, const CuFacts& facts, llvm::Value* ptr, TypeId type_id) {
  const auto& types = *facts.types;

  // Early exit for POD types
  if (!mir_to_llvm::TypeContainsManaged(type_id, types)) {
    return;
  }

  // Past the guard: type contains managed content. Switch must handle all
  // managed-bearing cases. Unhandled cases are bugs (missing lifecycle
  // support).
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kString:
      detail::DestroyString(ctx.GetBuilder(), ctx.GetLyraStringRelease(), ptr);
      return;

    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
      detail::DestroyContainer(
          ctx.GetBuilder(), facts, ctx.GetLyraAssocRelease(),
          ctx.GetLyraDynArrayRelease(), ptr, type_id);
      return;

    case TypeKind::kUnpackedStruct:
      detail::DestroyStruct(ctx, facts, ptr, type_id);
      return;

    case TypeKind::kUnpackedArray:
      detail::DestroyArray(ctx, facts, ptr, type_id);
      return;

    default:
      // TypeContainsManaged returned true but we don't handle this kind.
      // This is a bug: either TypeContainsManaged is wrong, or we're missing
      // lifecycle support for a new managed type.
      throw common::InternalError(
          "Destroy", std::format(
                         "unhandled TypeKind in lifecycle dispatch: {}",
                         ToString(type.Kind())));
  }
}

auto CloneLeafValue(
    Context& ctx, const CuFacts& facts, llvm::Value* value, TypeId type_id)
    -> llvm::Value* {
  const auto& types = *facts.types;

  // Early exit for non-managed types: return value unchanged
  if (!mir_to_llvm::TypeContainsManaged(type_id, types)) {
    return value;
  }

  // Past the guard: type contains managed content. Must clone or error.
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kString:
      return detail::CloneString(
          ctx.GetBuilder(), ctx.GetLyraStringRetain(), value);

    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
      return detail::CloneContainer(
          ctx.GetBuilder(), facts, ctx.GetLyraAssocClone(),
          ctx.GetLyraDynArrayClone(), value, type_id);

    case TypeKind::kUnpackedStruct:
      throw common::InternalError(
          "CloneLeafValue",
          "struct types must be cloned via CopyInit (field-by-field)");

    case TypeKind::kUnpackedArray:
      throw common::InternalError(
          "CloneLeafValue",
          "unpacked arrays must be cloned via CopyInit (element-by-element)");

    default:
      // TypeContainsManaged returned true but we don't handle this kind.
      throw common::InternalError(
          "CloneLeafValue",
          std::format(
              "unhandled managed TypeKind in lifecycle dispatch: {}",
              ToString(type.Kind())));
  }
}

void CopyInit(
    Context& ctx, const CuFacts& facts, llvm::Value* dst_ptr,
    llvm::Value* src_ptr, TypeId type_id) {
  const auto& types = *facts.types;

  // POD types: simple load + store
  if (!mir_to_llvm::TypeContainsManaged(type_id, types)) {
    detail::CopyInitPod(ctx, facts, dst_ptr, src_ptr, type_id);
    return;
  }

  // Past the guard: type contains managed content. Switch must handle all
  // managed-bearing cases.
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kString:
      detail::CopyInitString(
          ctx.GetBuilder(), ctx.GetLyraStringRetain(), dst_ptr, src_ptr);
      return;

    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
      detail::CopyInitContainer(
          ctx.GetBuilder(), facts, ctx.GetLyraDynArrayClone(),
          ctx.GetLyraAssocClone(), dst_ptr, src_ptr, type_id);
      return;

    case TypeKind::kUnpackedStruct:
      detail::CopyInitStruct(ctx, facts, dst_ptr, src_ptr, type_id);
      return;

    case TypeKind::kUnpackedArray:
      detail::CopyInitArray(ctx, facts, dst_ptr, src_ptr, type_id);
      return;

    default:
      throw common::InternalError(
          "CopyInit", std::format(
                          "unhandled TypeKind in lifecycle dispatch: {}",
                          ToString(type.Kind())));
  }
}

void CopyAssign(
    Context& ctx, const CuFacts& facts, llvm::Value* dst_ptr,
    llvm::Value* src_ptr, TypeId type_id) {
  // CopyAssign = Destroy(dst) + CopyInit(dst, src)
  Destroy(ctx, facts, dst_ptr, type_id);
  CopyInit(ctx, facts, dst_ptr, src_ptr, type_id);
}

void MoveInit(
    Context& ctx, const CuFacts& facts, llvm::Value* dst_ptr,
    llvm::Value* src_ptr, TypeId type_id) {
  // Self-move is a no-op (avoids nulling out dst when dst == src)
  if (dst_ptr == src_ptr) {
    return;
  }

  const auto& types = *facts.types;

  // POD types: simple load + store (no cleanup needed)
  if (!mir_to_llvm::TypeContainsManaged(type_id, types)) {
    detail::CopyInitPod(ctx, facts, dst_ptr, src_ptr, type_id);
    return;
  }

  // Past the guard: type contains managed content. Switch must handle all
  // managed-bearing cases.
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kString:
      detail::MoveInitString(ctx.GetBuilder(), dst_ptr, src_ptr);
      return;

    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
      detail::MoveInitContainer(ctx.GetBuilder(), dst_ptr, src_ptr);
      return;

    case TypeKind::kUnpackedStruct:
      detail::MoveInitStruct(ctx, facts, dst_ptr, src_ptr, type_id);
      return;

    case TypeKind::kUnpackedArray:
      detail::MoveInitArray(ctx, facts, dst_ptr, src_ptr, type_id);
      return;

    default:
      throw common::InternalError(
          "MoveInit", std::format(
                          "unhandled TypeKind in lifecycle dispatch: {}",
                          ToString(type.Kind())));
  }
}

void MoveAssign(
    Context& ctx, const CuFacts& facts, llvm::Value* dst_ptr,
    llvm::Value* src_ptr, TypeId type_id) {
  // Self-assign is a no-op (avoids destroy + null-out when dst == src)
  if (dst_ptr == src_ptr) {
    return;
  }
  // MoveAssign = Destroy(dst) + MoveInit(dst, src)
  Destroy(ctx, facts, dst_ptr, type_id);
  MoveInit(ctx, facts, dst_ptr, src_ptr, type_id);
}

void MoveCleanup(
    Context& ctx, const CuFacts& facts, llvm::Value* src_ptr, TypeId type_id) {
  const auto& types = *facts.types;

  // Early exit for POD types
  if (!mir_to_llvm::TypeContainsManaged(type_id, types)) {
    return;
  }

  // Past the guard: type contains managed content. Switch must handle all
  // managed-bearing cases. Unhandled cases are bugs.
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kString:
      detail::MoveCleanupString(ctx.GetBuilder(), src_ptr);
      return;

    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
      detail::MoveCleanupContainer(ctx.GetBuilder(), src_ptr);
      return;

    case TypeKind::kUnpackedStruct:
      detail::MoveCleanupStruct(ctx, facts, src_ptr, type_id);
      return;

    case TypeKind::kUnpackedArray:
      detail::MoveCleanupArray(ctx, facts, src_ptr, type_id);
      return;

    default:
      // TypeContainsManaged returned true but we don't handle this kind.
      throw common::InternalError(
          "MoveCleanup", std::format(
                             "unhandled TypeKind in lifecycle dispatch: {}",
                             ToString(type.Kind())));
  }
}

}  // namespace lyra::lowering::mir_to_llvm
