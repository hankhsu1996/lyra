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
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/llvm_backend/type_ops_managed.hpp"
#include "lyra/llvm_backend/union_storage.hpp"

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
  auto* llvm_elem_type = llvm_array_type->getElementType();

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
  auto* elem_ptr =
      builder.CreateGEP(llvm_elem_type, array_ptr, {phi}, "arr.elem.ptr");

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

}  // namespace lyra::lowering::mir_to_llvm::detail
