#include "lyra/llvm_backend/lifecycle/lifecycle_array.hpp"

#include <cstdint>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/lifecycle/detail.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"

namespace lyra::lowering::mir_to_llvm::detail {

void ForEachArrayElementPtr(
    Context& ctx, llvm::Value* array_ptr, TypeId array_type_id,
    ArrayElementCallback callback) {
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[array_type_id];
  const auto& arr_info = type.AsUnpackedArray();
  uint32_t count = arr_info.range.Size();
  TypeId elem_type_id = arr_info.element_type;

  auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, array_type_id);
  if (!llvm_type_result) {
    throw common::InternalError(
        "ForEachArrayElementPtr", "failed to get LLVM type for array");
  }
  auto* llvm_array_type = llvm::cast<llvm::ArrayType>(*llvm_type_result);

  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();
  auto* entry_block = builder.GetInsertBlock();
  auto* func = entry_block->getParent();

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* count_val = llvm::ConstantInt::get(i32_ty, count);

  auto* loop_header = llvm::BasicBlock::Create(llvm_ctx, "arr.loop", func);
  auto* loop_body = llvm::BasicBlock::Create(llvm_ctx, "arr.body", func);
  auto* loop_exit = llvm::BasicBlock::Create(llvm_ctx, "arr.exit", func);

  builder.CreateBr(loop_header);

  builder.SetInsertPoint(loop_header);
  auto* phi = builder.CreatePHI(i32_ty, 2, "arr.idx");
  phi->addIncoming(llvm::ConstantInt::get(i32_ty, 0), entry_block);

  auto* cond = builder.CreateICmpULT(phi, count_val, "arr.cmp");
  builder.CreateCondBr(cond, loop_body, loop_exit);

  builder.SetInsertPoint(loop_body);
  // GEP pattern for LLVM ArrayType: {0, idx} where 0 dereferences the array
  // pointer and idx indexes into the elements.
  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  auto* elem_ptr = builder.CreateGEP(
      llvm_array_type, array_ptr, {zero, phi}, "arr.elem.ptr");

  callback(elem_ptr, elem_type_id);

  auto* back_edge_block = builder.GetInsertBlock();
  auto* next_idx =
      builder.CreateAdd(phi, llvm::ConstantInt::get(i32_ty, 1), "arr.next");

  phi->addIncoming(next_idx, back_edge_block);

  builder.CreateBr(loop_header);

  builder.SetInsertPoint(loop_exit);
}

void DestroyArray(Context& ctx, llvm::Value* array_ptr, TypeId array_type_id) {
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[array_type_id];
  const auto& arr_info = type.AsUnpackedArray();
  TypeId elem_type_id = arr_info.element_type;

  if (!TypeContainsManaged(elem_type_id, types)) {
    return;
  }

  ForEachArrayElementPtr(
      ctx, array_ptr, array_type_id,
      [&](llvm::Value* elem_ptr, TypeId elem_type) {
        Destroy(ctx, elem_ptr, elem_type);
      });
}

void MoveCleanupArray(
    Context& ctx, llvm::Value* array_ptr, TypeId array_type_id) {
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[array_type_id];
  const auto& arr_info = type.AsUnpackedArray();
  TypeId elem_type_id = arr_info.element_type;

  if (!TypeContainsManaged(elem_type_id, types)) {
    return;
  }

  ForEachArrayElementPtr(
      ctx, array_ptr, array_type_id,
      [&](llvm::Value* elem_ptr, TypeId elem_type) {
        MoveCleanup(ctx, elem_ptr, elem_type);
      });
}

namespace {

using PairedArrayElementCallback =
    llvm::function_ref<void(llvm::Value*, llvm::Value*, TypeId)>;

// Iterate over pairs of elements from src and dst arrays.
// Generates an LLVM loop, calling callback(dst_elem, src_elem, elem_type).
void ForEachArrayElementPtrPaired(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr,
    TypeId array_type_id, PairedArrayElementCallback callback) {
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[array_type_id];
  const auto& arr_info = type.AsUnpackedArray();
  uint32_t count = arr_info.range.Size();
  TypeId elem_type_id = arr_info.element_type;

  auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, array_type_id);
  if (!llvm_type_result) {
    throw common::InternalError(
        "ForEachArrayElementPtrPaired", "failed to get LLVM type for array");
  }
  auto* llvm_array_type = llvm::cast<llvm::ArrayType>(*llvm_type_result);

  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();
  auto* entry_block = builder.GetInsertBlock();
  auto* func = entry_block->getParent();

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* count_val = llvm::ConstantInt::get(i32_ty, count);

  auto* loop_header = llvm::BasicBlock::Create(llvm_ctx, "copy.arr.loop", func);
  auto* loop_body = llvm::BasicBlock::Create(llvm_ctx, "copy.arr.body", func);
  auto* loop_exit = llvm::BasicBlock::Create(llvm_ctx, "copy.arr.exit", func);

  builder.CreateBr(loop_header);

  builder.SetInsertPoint(loop_header);
  auto* phi = builder.CreatePHI(i32_ty, 2, "copy.arr.idx");
  phi->addIncoming(llvm::ConstantInt::get(i32_ty, 0), entry_block);

  auto* cond = builder.CreateICmpULT(phi, count_val, "copy.arr.cmp");
  builder.CreateCondBr(cond, loop_body, loop_exit);

  builder.SetInsertPoint(loop_body);
  // GEP pattern for LLVM ArrayType: {0, idx} where 0 dereferences the array
  // pointer and idx indexes into the elements.
  auto* zero = llvm::ConstantInt::get(i32_ty, 0);
  auto* src_elem_ptr =
      builder.CreateGEP(llvm_array_type, src_ptr, {zero, phi}, "copy.arr.src");
  auto* dst_elem_ptr =
      builder.CreateGEP(llvm_array_type, dst_ptr, {zero, phi}, "copy.arr.dst");

  callback(dst_elem_ptr, src_elem_ptr, elem_type_id);

  auto* back_edge_block = builder.GetInsertBlock();
  auto* next_idx = builder.CreateAdd(
      phi, llvm::ConstantInt::get(i32_ty, 1), "copy.arr.next");

  phi->addIncoming(next_idx, back_edge_block);

  builder.CreateBr(loop_header);

  builder.SetInsertPoint(loop_exit);
}

}  // namespace

void CopyInitArray(
    Context& ctx, llvm::Value* dst_ptr, llvm::Value* src_ptr,
    TypeId array_type_id) {
  // Note: We always use element-by-element copy (even for POD elements)
  // because CopyInit handles the dispatch. For large POD arrays, this is
  // less efficient than memcpy, but keeps the code path uniform and correct.
  ForEachArrayElementPtrPaired(
      ctx, dst_ptr, src_ptr, array_type_id,
      [&](llvm::Value* dst_elem, llvm::Value* src_elem, TypeId elem_type) {
        CopyInit(ctx, dst_elem, src_elem, elem_type);
      });
}

}  // namespace lyra::lowering::mir_to_llvm::detail
