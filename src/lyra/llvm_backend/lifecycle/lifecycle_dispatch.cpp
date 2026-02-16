#include <format>

#include <llvm/IR/Value.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/lifecycle/detail.hpp"
#include "lyra/llvm_backend/lifecycle/lifecycle_array.hpp"
#include "lyra/llvm_backend/lifecycle/lifecycle_pod.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace detail {

// Destroy helpers (defined in lifecycle_*.cpp files)
void DestroyString(Context& ctx, llvm::Value* ptr);
void DestroyContainer(Context& ctx, llvm::Value* ptr, TypeId type_id);
void DestroyStruct(Context& ctx, llvm::Value* ptr, TypeId type_id);

// Clone helpers (defined in lifecycle_*.cpp files)
auto CloneString(Context& ctx, llvm::Value* handle) -> llvm::Value*;
auto CloneContainer(Context& ctx, llvm::Value* handle, TypeId type_id)
    -> llvm::Value*;

// MoveCleanup leaf helpers (defined in lifecycle_*.cpp files)
void MoveCleanupString(Context& ctx, llvm::Value* ptr);
void MoveCleanupContainer(Context& ctx, llvm::Value* ptr);
void MoveCleanupStruct(Context& ctx, llvm::Value* ptr, TypeId type_id);

// CopyInit helpers (defined in lifecycle_*.cpp files)
void CopyInitString(Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr);
void CopyInitContainer(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id);
void CopyInitStruct(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id);

// MoveInit helpers (defined in lifecycle_*.cpp files)
void MoveInitString(Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr);
void MoveInitContainer(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr);
void MoveInitStruct(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id);

}  // namespace detail

void Destroy(Context& ctx, llvm::Value* ptr, TypeId type_id) {
  const auto& types = ctx.GetTypeArena();

  // Early exit for POD types
  if (!TypeContainsManaged(type_id, types)) {
    return;
  }

  // Past the guard: type contains managed content. Switch must handle all
  // managed-bearing cases. Unhandled cases are bugs (missing lifecycle
  // support).
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kString:
      detail::DestroyString(ctx, ptr);
      return;

    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
      detail::DestroyContainer(ctx, ptr, type_id);
      return;

    case TypeKind::kUnpackedStruct:
      detail::DestroyStruct(ctx, ptr, type_id);
      return;

    case TypeKind::kUnpackedArray:
      detail::DestroyArray(ctx, ptr, type_id);
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

auto CloneLeafValue(Context& ctx, llvm::Value* value, TypeId type_id)
    -> llvm::Value* {
  const auto& types = ctx.GetTypeArena();

  // Early exit for non-managed types: return value unchanged
  if (!TypeContainsManaged(type_id, types)) {
    return value;
  }

  // Past the guard: type contains managed content. Must clone or error.
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kString:
      return detail::CloneString(ctx, value);

    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
      return detail::CloneContainer(ctx, value, type_id);

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
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id) {
  const auto& types = ctx.GetTypeArena();

  // POD types: simple load + store
  if (!TypeContainsManaged(type_id, types)) {
    detail::CopyInitPod(ctx, dst_ptr, src_ptr, type_id);
    return;
  }

  // Past the guard: type contains managed content. Switch must handle all
  // managed-bearing cases.
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kString:
      detail::CopyInitString(ctx, dst_ptr, src_ptr);
      return;

    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
      detail::CopyInitContainer(ctx, dst_ptr, src_ptr, type_id);
      return;

    case TypeKind::kUnpackedStruct:
      detail::CopyInitStruct(ctx, dst_ptr, src_ptr, type_id);
      return;

    case TypeKind::kUnpackedArray:
      detail::CopyInitArray(ctx, dst_ptr, src_ptr, type_id);
      return;

    default:
      throw common::InternalError(
          "CopyInit", std::format(
                          "unhandled TypeKind in lifecycle dispatch: {}",
                          ToString(type.Kind())));
  }
}

void CopyAssign(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id) {
  // CopyAssign = Destroy(dst) + CopyInit(dst, src)
  Destroy(ctx, dst_ptr, type_id);
  CopyInit(ctx, dst_ptr, src_ptr, type_id);
}

void MoveInit(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id) {
  // Self-move is a no-op (avoids nulling out dst when dst == src)
  if (dst_ptr == src_ptr) {
    return;
  }

  const auto& types = ctx.GetTypeArena();

  // POD types: simple load + store (no cleanup needed)
  if (!TypeContainsManaged(type_id, types)) {
    detail::CopyInitPod(ctx, dst_ptr, src_ptr, type_id);
    return;
  }

  // Past the guard: type contains managed content. Switch must handle all
  // managed-bearing cases.
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kString:
      detail::MoveInitString(ctx, dst_ptr, src_ptr);
      return;

    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
      detail::MoveInitContainer(ctx, dst_ptr, src_ptr);
      return;

    case TypeKind::kUnpackedStruct:
      detail::MoveInitStruct(ctx, dst_ptr, src_ptr, type_id);
      return;

    case TypeKind::kUnpackedArray:
      detail::MoveInitArray(ctx, dst_ptr, src_ptr, type_id);
      return;

    default:
      throw common::InternalError(
          "MoveInit", std::format(
                          "unhandled TypeKind in lifecycle dispatch: {}",
                          ToString(type.Kind())));
  }
}

void MoveAssign(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id) {
  // Self-assign is a no-op (avoids destroy + null-out when dst == src)
  if (dst_ptr == src_ptr) {
    return;
  }
  // MoveAssign = Destroy(dst) + MoveInit(dst, src)
  Destroy(ctx, dst_ptr, type_id);
  MoveInit(ctx, dst_ptr, src_ptr, type_id);
}

void MoveCleanup(Context& ctx, llvm::Value* src_ptr, TypeId type_id) {
  const auto& types = ctx.GetTypeArena();

  // Early exit for POD types
  if (!TypeContainsManaged(type_id, types)) {
    return;
  }

  // Past the guard: type contains managed content. Switch must handle all
  // managed-bearing cases. Unhandled cases are bugs.
  const Type& type = types[type_id];
  switch (type.Kind()) {
    case TypeKind::kString:
      detail::MoveCleanupString(ctx, src_ptr);
      return;

    case TypeKind::kDynamicArray:
    case TypeKind::kQueue:
    case TypeKind::kAssociativeArray:
      detail::MoveCleanupContainer(ctx, src_ptr);
      return;

    case TypeKind::kUnpackedStruct:
      detail::MoveCleanupStruct(ctx, src_ptr, type_id);
      return;

    case TypeKind::kUnpackedArray:
      detail::MoveCleanupArray(ctx, src_ptr, type_id);
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
