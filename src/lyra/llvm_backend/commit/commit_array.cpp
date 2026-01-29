#include <cstddef>
#include <cstdint>
#include <expected>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/type_ops_managed.hpp"
#include "lyra/llvm_backend/union_storage.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto AssignArrayFieldByFieldInternal(
    Context& context, llvm::Value* source_ptr, llvm::Value* target_ptr,
    TypeId array_type_id, OwnershipPolicy policy) -> Result<void>;

auto AssignStructFieldByFieldInternal(
    Context& context, llvm::Value* source_ptr, llvm::Value* target_ptr,
    TypeId struct_type_id, OwnershipPolicy policy) -> Result<void>;

auto AssignElement(
    Context& context, llvm::Value* source_elem_ptr,
    llvm::Value* target_elem_ptr, llvm::Type* elem_llvm_type,
    TypeId elem_type_id, OwnershipPolicy policy) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& elem_type = types[elem_type_id];

  if (elem_type.Kind() == TypeKind::kString) {
    auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
    llvm::Value* new_val =
        builder.CreateLoad(ptr_ty, source_elem_ptr, "ae.src");
    detail::CommitStringField(context, target_elem_ptr, new_val, policy);
    return {};
  }

  if (elem_type.Kind() == TypeKind::kUnpackedStruct) {
    if (NeedsFieldByField(elem_type_id, types)) {
      return AssignStructFieldByFieldInternal(
          context, source_elem_ptr, target_elem_ptr, elem_type_id, policy);
    }
    llvm::Value* val =
        builder.CreateLoad(elem_llvm_type, source_elem_ptr, "ae.agg");
    detail::CommitPlainField(context, target_elem_ptr, val);
    return {};
  }

  if (elem_type.Kind() == TypeKind::kUnpackedArray) {
    if (NeedsFieldByField(elem_type_id, types)) {
      return AssignArrayFieldByFieldInternal(
          context, source_elem_ptr, target_elem_ptr, elem_type_id, policy);
    }
    llvm::Value* val =
        builder.CreateLoad(elem_llvm_type, source_elem_ptr, "ae.arr");
    detail::CommitPlainField(context, target_elem_ptr, val);
    return {};
  }

  llvm::Value* val =
      builder.CreateLoad(elem_llvm_type, source_elem_ptr, "ae.val");
  detail::CommitPlainField(context, target_elem_ptr, val);
  return {};
}

auto AssignStructField(
    Context& context, llvm::Value* source_field_ptr,
    llvm::Value* target_field_ptr, llvm::Type* field_llvm_type,
    TypeId field_type_id, OwnershipPolicy policy) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& field_type = types[field_type_id];

  if (field_type.Kind() == TypeKind::kString) {
    auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
    llvm::Value* new_val =
        builder.CreateLoad(ptr_ty, source_field_ptr, "sf.src");
    detail::CommitStringField(context, target_field_ptr, new_val, policy);
    return {};
  }

  if (field_type.Kind() == TypeKind::kUnpackedStruct) {
    if (NeedsFieldByField(field_type_id, types)) {
      return AssignStructFieldByFieldInternal(
          context, source_field_ptr, target_field_ptr, field_type_id, policy);
    }
    llvm::Value* val =
        builder.CreateLoad(field_llvm_type, source_field_ptr, "sf.agg");
    detail::CommitPlainField(context, target_field_ptr, val);
    return {};
  }

  if (field_type.Kind() == TypeKind::kUnpackedArray) {
    if (NeedsFieldByField(field_type_id, types)) {
      return AssignArrayFieldByFieldInternal(
          context, source_field_ptr, target_field_ptr, field_type_id, policy);
    }
    llvm::Value* val =
        builder.CreateLoad(field_llvm_type, source_field_ptr, "sf.arr");
    detail::CommitPlainField(context, target_field_ptr, val);
    return {};
  }

  llvm::Value* val =
      builder.CreateLoad(field_llvm_type, source_field_ptr, "sf.val");
  detail::CommitPlainField(context, target_field_ptr, val);
  return {};
}

auto AssignStructFieldByFieldInternal(
    Context& context, llvm::Value* source_ptr, llvm::Value* target_ptr,
    TypeId struct_type_id, OwnershipPolicy policy) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& struct_type = types[struct_type_id];
  const auto& struct_info = struct_type.AsUnpackedStruct();

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

    auto result = AssignStructField(
        context, src_field_ptr, tgt_field_ptr, field_llvm_type, field.type,
        policy);
    if (!result) return result;
  }
  return {};
}

auto AssignArrayFieldByFieldInternal(
    Context& context, llvm::Value* source_ptr, llvm::Value* target_ptr,
    TypeId array_type_id, OwnershipPolicy policy) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& type = types[array_type_id];
  const auto& arr_info = type.AsUnpackedArray();
  uint32_t count = arr_info.range.Size();
  TypeId elem_type_id = arr_info.element_type;

  auto llvm_type_result = BuildLlvmTypeForTypeId(context, array_type_id);
  if (!llvm_type_result) return std::unexpected(llvm_type_result.error());
  auto* llvm_array_type = llvm::cast<llvm::ArrayType>(*llvm_type_result);
  llvm::Type* elem_llvm_type = llvm_array_type->getElementType();

  auto& llvm_ctx = context.GetLlvmContext();
  auto* entry_block = builder.GetInsertBlock();
  auto* func = entry_block->getParent();

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* count_val = llvm::ConstantInt::get(i32_ty, count);

  auto* loop_header = llvm::BasicBlock::Create(llvm_ctx, "arr.asgn.hdr", func);
  auto* loop_body = llvm::BasicBlock::Create(llvm_ctx, "arr.asgn.body", func);
  auto* loop_exit = llvm::BasicBlock::Create(llvm_ctx, "arr.asgn.exit", func);

  builder.CreateBr(loop_header);

  builder.SetInsertPoint(loop_header);
  auto* phi = builder.CreatePHI(i32_ty, 2, "arr.asgn.idx");
  phi->addIncoming(llvm::ConstantInt::get(i32_ty, 0), entry_block);

  auto* cond = builder.CreateICmpULT(phi, count_val, "arr.asgn.cmp");
  builder.CreateCondBr(cond, loop_body, loop_exit);

  builder.SetInsertPoint(loop_body);
  auto* src_elem_ptr =
      builder.CreateGEP(elem_llvm_type, source_ptr, {phi}, "arr.asgn.src");
  auto* tgt_elem_ptr =
      builder.CreateGEP(elem_llvm_type, target_ptr, {phi}, "arr.asgn.tgt");

  auto result = AssignElement(
      context, src_elem_ptr, tgt_elem_ptr, elem_llvm_type, elem_type_id,
      policy);
  if (!result) return result;

  auto* back_edge_block = builder.GetInsertBlock();
  auto* next_idx = builder.CreateAdd(
      phi, llvm::ConstantInt::get(i32_ty, 1), "arr.asgn.next");

  phi->addIncoming(next_idx, back_edge_block);

  builder.CreateBr(loop_header);

  builder.SetInsertPoint(loop_exit);
  return {};
}

}  // namespace

auto CommitArrayFieldByField(
    Context& ctx, mir::PlaceId target, mir::PlaceId source,
    TypeId array_type_id, OwnershipPolicy policy) -> Result<void> {
  if (commit::Access::IsDesignSlot(ctx, target)) {
    return std::unexpected(ctx.GetDiagnosticContext().MakeUnsupported(
        ctx.GetCurrentOrigin(),
        "unpacked array assignment to design slot with managed elements "
        "not yet supported",
        UnsupportedCategory::kFeature));
  }

  auto target_ptr_or_err = ctx.GetPlacePointer(target);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  auto source_ptr_or_err = ctx.GetPlacePointer(source);
  if (!source_ptr_or_err) return std::unexpected(source_ptr_or_err.error());
  llvm::Value* source_ptr = *source_ptr_or_err;

  auto result = AssignArrayFieldByFieldInternal(
      ctx, source_ptr, target_ptr, array_type_id, policy);
  if (!result) return result;

  CommitMoveCleanupIfTemp(ctx, source, policy, array_type_id);

  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
