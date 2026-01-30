#include <format>

#include <llvm/IR/Value.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/lifecycle/lifecycle_array.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"

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

void MoveInit(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr, TypeId type_id) {
  auto& builder = ctx.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(ctx.GetLlvmContext());

  // Move-initialize: load from src, store to dst, null out src.
  // CONTRACT: dst must be uninitialized (caller responsible for Destroy).
  // For managed types (string, dynarray, queue), the value is a pointer handle.
  llvm::Value* value = builder.CreateLoad(ptr_ty, src_ptr, "move.val");
  builder.CreateStore(value, dst_ptr);
  MoveCleanup(ctx, src_ptr, type_id);
}

}  // namespace lyra::lowering::mir_to_llvm
