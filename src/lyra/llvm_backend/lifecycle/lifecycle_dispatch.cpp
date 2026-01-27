#include <format>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/type_ops_managed.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace detail {
void DestroyString(Context& ctx, llvm::Value* ptr);
void DestroyContainer(Context& ctx, llvm::Value* ptr);
void DestroyStruct(Context& ctx, llvm::Value* ptr, TypeId type_id);

auto CloneString(Context& ctx, llvm::Value* handle) -> llvm::Value*;
auto CloneContainer(Context& ctx, llvm::Value* handle) -> llvm::Value*;

void MoveCleanupString(Context& ctx, llvm::Value* ptr);
void MoveCleanupContainer(Context& ctx, llvm::Value* ptr);
void MoveCleanupStruct(Context& ctx, llvm::Value* ptr, TypeId type_id);
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
      detail::DestroyContainer(ctx, ptr);
      return;

    case TypeKind::kUnpackedStruct:
      detail::DestroyStruct(ctx, ptr, type_id);
      return;

    case TypeKind::kUnpackedArray:
      throw common::InternalError(
          "Destroy", "unpacked arrays with managed elements not yet supported");

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

auto CloneValue(Context& ctx, llvm::Value* value, TypeId type_id)
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
      return detail::CloneContainer(ctx, value);

    case TypeKind::kUnpackedStruct:
      throw common::InternalError(
          "CloneValue",
          "struct types must be cloned via field-by-field assignment");

    case TypeKind::kUnpackedArray:
      throw common::InternalError(
          "CloneValue",
          "unpacked arrays must be cloned via element-by-element assignment");

    default:
      // TypeContainsManaged returned true but we don't handle this kind.
      throw common::InternalError(
          "CloneValue",
          std::format(
              "unhandled managed TypeKind in lifecycle dispatch: {}",
              ToString(type.Kind())));
  }
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
      detail::MoveCleanupContainer(ctx, src_ptr);
      return;

    case TypeKind::kUnpackedStruct:
      detail::MoveCleanupStruct(ctx, src_ptr, type_id);
      return;

    case TypeKind::kUnpackedArray:
      throw common::InternalError(
          "MoveCleanup",
          "unpacked arrays with managed elements not yet supported");

    default:
      // TypeContainsManaged returned true but we don't handle this kind.
      throw common::InternalError(
          "MoveCleanup", std::format(
                             "unhandled TypeKind in lifecycle dispatch: {}",
                             ToString(type.Kind())));
  }
}

}  // namespace lyra::lowering::mir_to_llvm
