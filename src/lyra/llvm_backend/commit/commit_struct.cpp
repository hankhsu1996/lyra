#include <cstddef>
#include <expected>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Forward declaration for mutual recursion
auto AssignStructFieldByField(
    Context& context, llvm::Value* source_ptr, llvm::Value* target_ptr,
    TypeId struct_type_id, OwnershipPolicy policy) -> Result<void>;

// Assign a single field, handling string ref counting and nested structs.
// Uses Destroy(target) first, then stores new value.
auto AssignField(
    Context& context, llvm::Value* source_field_ptr,
    llvm::Value* target_field_ptr, llvm::Type* field_llvm_type,
    TypeId field_type_id, OwnershipPolicy policy) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& field_type = types[field_type_id];

  if (field_type.Kind() == TypeKind::kString) {
    // Use symmetric lifecycle API for string field assignment.
    // CopyAssign: Destroy(dst) + CopyInit(dst, src) - src unchanged
    // MoveAssign: Destroy(dst) + MoveInit(dst, src) - src nulled per-field
    if (policy == OwnershipPolicy::kClone) {
      CopyAssign(context, target_field_ptr, source_field_ptr, field_type_id);
    } else {
      // kMove: MoveAssign handles destroy old dst, transfer value, null out
      // src. Source null-out is per-field (not deferred to struct level).
      MoveAssign(context, target_field_ptr, source_field_ptr, field_type_id);
    }
    return {};
  }

  if (field_type.Kind() == TypeKind::kUnpackedStruct) {
    // Nested struct: check if it contains managed fields and recurse if so
    if (NeedsFieldByField(field_type_id, types)) {
      auto result = AssignStructFieldByField(
          context, source_field_ptr, target_field_ptr, field_type_id, policy);
      if (!result) return result;
    } else {
      // No managed fields: aggregate load/store via commit layer
      llvm::Value* val =
          builder.CreateLoad(field_llvm_type, source_field_ptr, "sf.agg");
      detail::CommitPlainField(context, target_field_ptr, val);
    }
    return {};
  }

  // Other fields: simple load/store via commit layer (no ownership semantics)
  llvm::Value* val =
      builder.CreateLoad(field_llvm_type, source_field_ptr, "sf.val");
  detail::CommitPlainField(context, target_field_ptr, val);
  return {};
}

// Assign struct field-by-field for structs containing string fields.
//
// CONTRACT: No rollback on failure. Field assignments proceed in order; if
// field N fails, fields 0..N-1 have already been committed. Since we only
// throw InternalError (not recoverable user errors) from lifecycle ops,
// partial assignment state is acceptable - the process would abort anyway.
// This matches SV semantics where assignment is atomic at the language level.
auto AssignStructFieldByField(
    Context& context, llvm::Value* source_ptr, llvm::Value* target_ptr,
    TypeId struct_type_id, OwnershipPolicy policy) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& struct_type = types[struct_type_id];
  const auto& struct_info = struct_type.AsUnpackedStruct();

  // Get LLVM struct type for GEP operations
  auto llvm_struct_type_result =
      BuildLlvmTypeForTypeId(context, struct_type_id);
  if (!llvm_struct_type_result)
    return std::unexpected(llvm_struct_type_result.error());
  llvm::Type* llvm_struct_type = *llvm_struct_type_result;
  auto* llvm_struct = llvm::cast<llvm::StructType>(llvm_struct_type);

  for (size_t i = 0; i < struct_info.fields.size(); ++i) {
    const auto& field = struct_info.fields[i];
    auto field_idx = static_cast<unsigned>(i);

    llvm::Value* src_field_ptr =
        builder.CreateStructGEP(llvm_struct_type, source_ptr, field_idx);
    llvm::Value* tgt_field_ptr =
        builder.CreateStructGEP(llvm_struct_type, target_ptr, field_idx);
    llvm::Type* field_llvm_type = llvm_struct->getElementType(field_idx);

    auto result = AssignField(
        context, src_field_ptr, tgt_field_ptr, field_llvm_type, field.type,
        policy);
    if (!result) return result;
  }
  return {};
}

}  // namespace

auto CommitStructFieldByField(
    Context& ctx, mir::PlaceId target, mir::PlaceId source,
    TypeId struct_type_id, OwnershipPolicy policy) -> Result<void> {
  // CONTRACT: kMove is only valid from temps (no aliasing possible).
  // MoveAssign nulls the source per-field, so kMove from a non-temp would
  // corrupt shared state.
  if (policy == OwnershipPolicy::kMove) {
    const auto& arena = ctx.GetMirArena();
    const auto& src_place = arena[source];
    if (src_place.root.kind != mir::PlaceRoot::Kind::kTemp) {
      throw common::InternalError(
          "CommitStructFieldByField", "kMove from non-temp source");
    }
  }

  // Get target pointer
  auto target_ptr_or_err = ctx.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  // Get source pointer
  auto source_ptr_or_err = ctx.GetPlacePointer(source);
  if (!source_ptr_or_err) return std::unexpected(source_ptr_or_err.error());
  llvm::Value* source_ptr = *source_ptr_or_err;

  auto result = AssignStructFieldByField(
      ctx, source_ptr, target_ptr, struct_type_id, policy);
  if (!result) return result;

  // Notify if design slot (after stores complete)
  CommitNotifyAggregateIfDesignSlot(ctx, target);

  // Note: Source cleanup for kMove is handled per-field via MoveAssign.
  // No CommitMoveCleanupIfTemp needed here - MoveAssign nulls src per-field.

  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
