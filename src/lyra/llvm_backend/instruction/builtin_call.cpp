#include "lyra/llvm_backend/instruction/builtin_call.hpp"

#include <cstdint>
#include <expected>
#include <format>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/lifecycle.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

struct QueueTypeInfo {
  TypeId elem_type_id{};
  uint32_t max_bound = 0;
  Context::ElemOpsInfo elem_ops{};
};

auto GetQueueTypeInfo(Context& context, mir::PlaceId receiver)
    -> Result<QueueTypeInfo> {
  const auto& types = context.GetTypeArena();
  const auto& arena = context.GetMirArena();
  const auto& recv_place = arena[receiver];
  TypeId recv_type_id = mir::TypeOfPlace(types, recv_place);
  const auto& queue_type = types[recv_type_id];
  TypeId elem_type_id = queue_type.AsQueue().element_type;
  auto elem_ops = context.GetElemOpsForType(elem_type_id);
  if (!elem_ops) return std::unexpected(elem_ops.error());
  return QueueTypeInfo{
      .elem_type_id = elem_type_id,
      .max_bound = queue_type.AsQueue().max_bound,
      .elem_ops = *elem_ops,
  };
}

auto LowerArrayDelete(Context& context, const mir::BuiltinCall& call)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());

  auto recv_ptr_or_err = context.GetPlacePointer(call.receiver);
  if (!recv_ptr_or_err) return std::unexpected(recv_ptr_or_err.error());
  llvm::Value* recv_ptr = *recv_ptr_or_err;

  llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "da.del.h");

  // Delete: clear contents, handle stays valid
  builder.CreateCall(context.GetLyraDynArrayDelete(), {handle});

  // Notify if design slot (handle unchanged, content cleared)
  CommitNotifyMutationIfDesignSlot(context, call.receiver);
  return {};
}

auto LowerQueueDelete(Context& context, const mir::BuiltinCall& call)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());

  auto recv_ptr_or_err = context.GetPlacePointer(call.receiver);
  if (!recv_ptr_or_err) return std::unexpected(recv_ptr_or_err.error());
  llvm::Value* recv_ptr = *recv_ptr_or_err;

  // Get receiver TypeId for typed teardown
  const auto& arena = context.GetMirArena();
  const auto& recv_place = arena[call.receiver];
  TypeId recv_type_id = mir::TypeOfPlace(context.GetTypeArena(), recv_place);

  // Typed teardown via lifecycle (loads handle, calls release)
  Destroy(context, recv_ptr, recv_type_id);

  // Store null handle
  builder.CreateStore(llvm::Constant::getNullValue(ptr_ty), recv_ptr);
  CommitNotifyMutationIfDesignSlot(context, call.receiver);
  return {};
}

auto LowerQueueDeleteAt(Context& context, const mir::BuiltinCall& call)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
  auto* i64_ty = llvm::Type::getInt64Ty(context.GetLlvmContext());

  auto recv_ptr_or_err = context.GetPlacePointer(call.receiver);
  if (!recv_ptr_or_err) return std::unexpected(recv_ptr_or_err.error());
  llvm::Value* recv_ptr = *recv_ptr_or_err;

  llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "q.delat.h");

  auto index_or_err = LowerOperand(context, call.args[0]);
  if (!index_or_err) return std::unexpected(index_or_err.error());
  llvm::Value* index = *index_or_err;

  index = builder.CreateSExtOrTrunc(index, i64_ty, "q.delat.idx");
  builder.CreateCall(context.GetLyraQueueDeleteAt(), {handle, index});
  CommitNotifyMutationIfDesignSlot(context, call.receiver);
  return {};
}

auto LowerQueuePushBack(Context& context, const mir::BuiltinCall& call)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());

  auto recv_ptr_or_err = context.GetPlacePointer(call.receiver);
  if (!recv_ptr_or_err) return std::unexpected(recv_ptr_or_err.error());
  llvm::Value* recv_ptr = *recv_ptr_or_err;

  auto qi_result = GetQueueTypeInfo(context, call.receiver);
  if (!qi_result) return std::unexpected(qi_result.error());
  auto qi = *qi_result;

  auto val_or_err =
      LowerOperandAsStorage(context, call.args[0], qi.elem_ops.elem_llvm_type);
  if (!val_or_err) return std::unexpected(val_or_err.error());
  llvm::Value* val = *val_or_err;

  auto* temp =
      builder.CreateAlloca(qi.elem_ops.elem_llvm_type, nullptr, "q.pb.tmp");
  builder.CreateStore(val, temp);

  builder.CreateCall(
      context.GetLyraQueuePushBack(),
      {recv_ptr, temp, llvm::ConstantInt::get(i32_ty, qi.elem_ops.elem_size),
       llvm::ConstantInt::get(i32_ty, qi.max_bound), qi.elem_ops.clone_fn,
       qi.elem_ops.destroy_fn});

  CommitNotifyMutationIfDesignSlot(context, call.receiver);
  return {};
}

auto LowerQueuePushFront(Context& context, const mir::BuiltinCall& call)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());

  auto recv_ptr_or_err = context.GetPlacePointer(call.receiver);
  if (!recv_ptr_or_err) return std::unexpected(recv_ptr_or_err.error());
  llvm::Value* recv_ptr = *recv_ptr_or_err;

  auto qi_result = GetQueueTypeInfo(context, call.receiver);
  if (!qi_result) return std::unexpected(qi_result.error());
  auto qi = *qi_result;

  auto val_or_err =
      LowerOperandAsStorage(context, call.args[0], qi.elem_ops.elem_llvm_type);
  if (!val_or_err) return std::unexpected(val_or_err.error());
  llvm::Value* val = *val_or_err;

  auto* temp =
      builder.CreateAlloca(qi.elem_ops.elem_llvm_type, nullptr, "q.pf.tmp");
  builder.CreateStore(val, temp);

  builder.CreateCall(
      context.GetLyraQueuePushFront(),
      {recv_ptr, temp, llvm::ConstantInt::get(i32_ty, qi.elem_ops.elem_size),
       llvm::ConstantInt::get(i32_ty, qi.max_bound), qi.elem_ops.clone_fn,
       qi.elem_ops.destroy_fn});

  CommitNotifyMutationIfDesignSlot(context, call.receiver);
  return {};
}

auto LowerQueuePopBack(Context& context, const mir::BuiltinCall& call)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());

  auto recv_ptr_or_err = context.GetPlacePointer(call.receiver);
  if (!recv_ptr_or_err) return std::unexpected(recv_ptr_or_err.error());
  llvm::Value* recv_ptr = *recv_ptr_or_err;

  llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "q.popb.h");

  // Zero-init target (default if queue is empty)
  auto target_ptr_or_err = context.GetPlacePointer(*call.dest);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  auto target_type_or_err = context.GetPlaceLlvmType(*call.dest);
  if (!target_type_or_err) return std::unexpected(target_type_or_err.error());
  llvm::Type* target_type = *target_type_or_err;

  builder.CreateStore(llvm::Constant::getNullValue(target_type), target_ptr);

  builder.CreateCall(context.GetLyraQueuePopBack(), {handle, target_ptr});
  CommitNotifyMutationIfDesignSlot(context, call.receiver);
  return {};
}

auto LowerQueuePopFront(Context& context, const mir::BuiltinCall& call)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());

  auto recv_ptr_or_err = context.GetPlacePointer(call.receiver);
  if (!recv_ptr_or_err) return std::unexpected(recv_ptr_or_err.error());
  llvm::Value* recv_ptr = *recv_ptr_or_err;

  llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "q.popf.h");

  auto target_ptr_or_err = context.GetPlacePointer(*call.dest);
  if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
  llvm::Value* target_ptr = *target_ptr_or_err;

  auto target_type_or_err = context.GetPlaceLlvmType(*call.dest);
  if (!target_type_or_err) return std::unexpected(target_type_or_err.error());
  llvm::Type* target_type = *target_type_or_err;

  builder.CreateStore(llvm::Constant::getNullValue(target_type), target_ptr);

  builder.CreateCall(context.GetLyraQueuePopFront(), {handle, target_ptr});
  CommitNotifyMutationIfDesignSlot(context, call.receiver);
  return {};
}

auto LowerQueueInsert(Context& context, const mir::BuiltinCall& call)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
  auto* i64_ty = llvm::Type::getInt64Ty(context.GetLlvmContext());

  auto recv_ptr_or_err = context.GetPlacePointer(call.receiver);
  if (!recv_ptr_or_err) return std::unexpected(recv_ptr_or_err.error());
  llvm::Value* recv_ptr = *recv_ptr_or_err;

  auto qi_result = GetQueueTypeInfo(context, call.receiver);
  if (!qi_result) return std::unexpected(qi_result.error());
  auto qi = *qi_result;

  // args[0] = index, args[1] = value
  auto index_or_err = LowerOperand(context, call.args[0]);
  if (!index_or_err) return std::unexpected(index_or_err.error());
  llvm::Value* index = *index_or_err;
  index = builder.CreateSExtOrTrunc(index, i64_ty, "q.ins.idx");

  auto val_or_err =
      LowerOperandAsStorage(context, call.args[1], qi.elem_ops.elem_llvm_type);
  if (!val_or_err) return std::unexpected(val_or_err.error());
  llvm::Value* val = *val_or_err;

  auto* temp =
      builder.CreateAlloca(qi.elem_ops.elem_llvm_type, nullptr, "q.ins.tmp");
  builder.CreateStore(val, temp);

  builder.CreateCall(
      context.GetLyraQueueInsert(),
      {recv_ptr, index, temp,
       llvm::ConstantInt::get(i32_ty, qi.elem_ops.elem_size),
       llvm::ConstantInt::get(i32_ty, qi.max_bound), qi.elem_ops.clone_fn,
       qi.elem_ops.destroy_fn});

  CommitNotifyMutationIfDesignSlot(context, call.receiver);
  return {};
}

}  // namespace

auto LowerBuiltinCall(Context& context, const mir::BuiltinCall& call)
    -> Result<void> {
  switch (call.method) {
    case mir::BuiltinMethod::kArrayDelete:
      return LowerArrayDelete(context, call);
    case mir::BuiltinMethod::kQueueDelete:
      return LowerQueueDelete(context, call);
    case mir::BuiltinMethod::kQueueDeleteAt:
      return LowerQueueDeleteAt(context, call);
    case mir::BuiltinMethod::kQueuePushBack:
      return LowerQueuePushBack(context, call);
    case mir::BuiltinMethod::kQueuePushFront:
      return LowerQueuePushFront(context, call);
    case mir::BuiltinMethod::kQueuePopBack:
      return LowerQueuePopBack(context, call);
    case mir::BuiltinMethod::kQueuePopFront:
      return LowerQueuePopFront(context, call);
    case mir::BuiltinMethod::kQueueInsert:
      return LowerQueueInsert(context, call);

    default:
      throw common::InternalError(
          "LowerBuiltinCall",
          std::format(
              "unexpected builtin method in BuiltinCall: {}",
              static_cast<int>(call.method)));
  }
}

}  // namespace lyra::lowering::mir_to_llvm
