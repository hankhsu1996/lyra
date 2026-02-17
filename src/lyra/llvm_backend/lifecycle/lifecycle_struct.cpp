#include <cstddef>

#include <llvm/ADT/STLFunctionalExtras.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/lifecycle/detail.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"

namespace lyra::lowering::mir_to_llvm::detail {

using ManagedFieldCallback = llvm::function_ref<void(llvm::Value*, TypeId)>;
using PairedFieldCallback =
    llvm::function_ref<void(llvm::Value*, llvm::Value*, TypeId)>;

void ForEachManagedFieldPtr(
    Context& ctx, llvm::Value* struct_ptr, TypeId struct_type_id,
    ManagedFieldCallback callback) {
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[struct_type_id];
  const auto& struct_info = type.AsUnpackedStruct();

  auto llvm_struct_type_result = BuildLlvmTypeForTypeId(ctx, struct_type_id);
  if (!llvm_struct_type_result) {
    throw common::InternalError(
        "ForEachManagedFieldPtr", "failed to get LLVM type for struct");
  }
  llvm::Type* llvm_struct_type = *llvm_struct_type_result;

  for (size_t i = 0; i < struct_info.fields.size(); ++i) {
    const auto& field = struct_info.fields[i];
    if (mir_to_llvm::TypeContainsManaged(field.type, types)) {
      auto field_idx = static_cast<unsigned>(i);
      llvm::Value* field_ptr = ctx.GetBuilder().CreateStructGEP(
          llvm_struct_type, struct_ptr, field_idx);
      callback(field_ptr, field.type);
    }
  }
}

namespace {

// Iterate over pairs of fields from src and dst structs, for ALL fields.
// Calls callback(dst_field_ptr, src_field_ptr, field_type) for each field.
void ForEachFieldPtrPaired(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr,
    TypeId struct_type_id, PairedFieldCallback callback) {
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[struct_type_id];
  const auto& struct_info = type.AsUnpackedStruct();

  auto llvm_struct_type_result = BuildLlvmTypeForTypeId(ctx, struct_type_id);
  if (!llvm_struct_type_result) {
    throw common::InternalError(
        "ForEachFieldPtrPaired", "failed to get LLVM type for struct");
  }
  llvm::Type* llvm_struct_type = *llvm_struct_type_result;

  auto& builder = ctx.GetBuilder();

  for (size_t i = 0; i < struct_info.fields.size(); ++i) {
    const auto& field = struct_info.fields[i];
    auto field_idx = static_cast<unsigned>(i);

    llvm::Value* dst_field_ptr =
        builder.CreateStructGEP(llvm_struct_type, dst_ptr, field_idx);
    llvm::Value* src_field_ptr =
        builder.CreateStructGEP(llvm_struct_type, src_ptr, field_idx);

    callback(dst_field_ptr, src_field_ptr, field.type);
  }
}

}  // namespace

void DestroyStruct(Context& ctx, llvm::Value* ptr, TypeId type_id) {
  ForEachManagedFieldPtr(
      ctx, ptr, type_id, [&](llvm::Value* field_ptr, TypeId field_type) {
        Destroy(ctx, field_ptr, field_type);
      });
}

void MoveCleanupStruct(Context& ctx, llvm::Value* ptr, TypeId type_id) {
  ForEachManagedFieldPtr(
      ctx, ptr, type_id, [&](llvm::Value* field_ptr, TypeId field_type) {
        MoveCleanup(ctx, field_ptr, field_type);
      });
}

void CopyInitStruct(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr,
    TypeId struct_type_id) {
  ForEachFieldPtrPaired(
      ctx, dst_ptr, src_ptr, struct_type_id,
      [&](llvm::Value* dst_field, llvm::Value* src_field, TypeId field_type) {
        CopyInit(ctx, dst_field, src_field, field_type);
      });
}

void MoveInitStruct(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr,
    TypeId struct_type_id) {
  ForEachFieldPtrPaired(
      ctx, dst_ptr, src_ptr, struct_type_id,
      [&](llvm::Value* dst_field, llvm::Value* src_field, TypeId field_type) {
        MoveInit(ctx, dst_field, src_field, field_type);
      });
}

}  // namespace lyra::lowering::mir_to_llvm::detail
