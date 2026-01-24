#include "lyra/llvm_backend/instruction_compute_builtin.hpp"

#include <cstdint>
#include <format>
#include <vector>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

void NotifyIfDesignSlot(Context& context, mir::PlaceId receiver) {
  const auto& arena = context.GetMirArena();
  const auto& place = arena[receiver];
  if (place.root.kind != mir::PlaceRoot::Kind::kDesign) {
    return;
  }
  auto& builder = context.GetBuilder();
  auto signal_id = static_cast<uint32_t>(place.root.id);
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
  llvm::Value* recv_ptr = context.GetPlacePointer(receiver);
  llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "q.notify.h");
  builder.CreateCall(
      context.GetLyraStoreDynArray(),
      {context.GetEnginePointer(), recv_ptr, handle,
       llvm::ConstantInt::get(i32_ty, signal_id)});
}

struct QueueTypeInfo {
  TypeId elem_type_id{};
  uint32_t max_bound = 0;
  Context::ElemOpsInfo elem_ops{};
};

auto GetQueueTypeInfo(Context& context, mir::PlaceId receiver)
    -> QueueTypeInfo {
  const auto& types = context.GetTypeArena();
  const auto& arena = context.GetMirArena();
  const auto& recv_place = arena[receiver];
  TypeId recv_type_id = mir::TypeOfPlace(types, recv_place);
  const auto& queue_type = types[recv_type_id];
  TypeId elem_type_id = queue_type.AsQueue().element_type;
  return {
      .elem_type_id = elem_type_id,
      .max_bound = queue_type.AsQueue().max_bound,
      .elem_ops = context.GetElemOpsForType(elem_type_id),
  };
}

}  // namespace

void LowerDynArrayBuiltin(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info) {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());

  switch (info.method) {
    case mir::BuiltinMethod::kNewArray: {
      // result_type is the dynamic array TypeId
      const Type& da_type = types[info.result_type];
      if (da_type.Kind() != TypeKind::kDynamicArray) {
        throw common::InternalError(
            "LowerDynArrayBuiltin",
            "kNewArray result_type is not a dynamic array");
      }
      TypeId elem_type_id = da_type.AsDynamicArray().element_type;
      auto elem_ops = context.GetElemOpsForType(elem_type_id);

      // Operand 0 = size
      llvm::Value* size = LowerOperand(context, compute.value.operands[0]);
      auto* i64_ty = llvm::Type::getInt64Ty(context.GetLlvmContext());
      auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
      size = builder.CreateSExtOrTrunc(size, i64_ty, "da.new.size");

      auto* elem_size_val = llvm::ConstantInt::get(i32_ty, elem_ops.elem_size);

      llvm::Value* handle = nullptr;
      if (compute.value.operands.size() >= 2) {
        // new[size](src): copy from existing array
        llvm::Value* src = LowerOperand(context, compute.value.operands[1]);
        handle = builder.CreateCall(
            context.GetLyraDynArrayNewCopy(),
            {size, elem_size_val, elem_ops.clone_fn, elem_ops.destroy_fn, src},
            "da.new_copy");
      } else {
        // new[size]: fresh allocation
        handle = builder.CreateCall(
            context.GetLyraDynArrayNew(),
            {size, elem_size_val, elem_ops.clone_fn, elem_ops.destroy_fn},
            "da.new");
      }

      // Release old handle at target before storing new (prevents leak on
      // reassignment or loop iteration)
      llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
      auto* old_handle = builder.CreateLoad(ptr_ty, target_ptr, "da.new.old");
      builder.CreateCall(context.GetLyraDynArrayRelease(), {old_handle});
      builder.CreateStore(handle, target_ptr);
      break;
    }

    case mir::BuiltinMethod::kArraySize: {
      // Operand 0 = array (Use of place, loaded as handle)
      llvm::Value* handle = LowerOperand(context, compute.value.operands[0]);
      llvm::Value* size = builder.CreateCall(
          context.GetLyraDynArraySize(), {handle}, "da.size");

      // Fit i64 result to target storage type
      llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
      llvm::Type* target_type = context.GetPlaceLlvmType(compute.target);
      size = builder.CreateZExtOrTrunc(size, target_type, "da.size.fit");
      builder.CreateStore(size, target_ptr);
      break;
    }

    case mir::BuiltinMethod::kArrayDelete: {
      // Load handle from receiver
      llvm::Value* recv_ptr = context.GetPlacePointer(*info.receiver);
      llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "da.del.h");

      // Delete: clear contents, handle stays valid
      builder.CreateCall(context.GetLyraDynArrayDelete(), {handle});

      // If receiver is a design slot, notify the engine
      const auto& arena = context.GetMirArena();
      const auto& recv_place = arena[*info.receiver];
      if (recv_place.root.kind == mir::PlaceRoot::Kind::kDesign) {
        auto signal_id = static_cast<uint32_t>(recv_place.root.id);
        auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
        builder.CreateCall(
            context.GetLyraStoreDynArray(),
            {context.GetEnginePointer(), recv_ptr, handle,
             llvm::ConstantInt::get(i32_ty, signal_id)});
      }
      break;
    }

    default:
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm,
          common::UnsupportedKind::kFeature, context.GetCurrentOrigin(),
          std::format(
              "unsupported builtin method: {}", static_cast<int>(info.method)));
  }
}

void LowerQueueBuiltin(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info) {
  auto& builder = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
  auto* i64_ty = llvm::Type::getInt64Ty(context.GetLlvmContext());

  switch (info.method) {
    case mir::BuiltinMethod::kQueueSize: {
      llvm::Value* handle = LowerOperand(context, compute.value.operands[0]);
      llvm::Value* size =
          builder.CreateCall(context.GetLyraDynArraySize(), {handle}, "q.size");
      llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
      llvm::Type* target_type = context.GetPlaceLlvmType(compute.target);
      size = builder.CreateZExtOrTrunc(size, target_type, "q.size.fit");
      builder.CreateStore(size, target_ptr);
      break;
    }

    case mir::BuiltinMethod::kQueueDelete: {
      llvm::Value* recv_ptr = context.GetPlacePointer(*info.receiver);
      llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "q.del.h");
      builder.CreateCall(context.GetLyraDynArrayRelease(), {handle});
      builder.CreateStore(llvm::Constant::getNullValue(ptr_ty), recv_ptr);
      NotifyIfDesignSlot(context, *info.receiver);
      break;
    }

    case mir::BuiltinMethod::kQueueDeleteAt: {
      llvm::Value* recv_ptr = context.GetPlacePointer(*info.receiver);
      llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "q.delat.h");
      llvm::Value* index = LowerOperand(context, compute.value.operands[0]);
      index = builder.CreateSExtOrTrunc(index, i64_ty, "q.delat.idx");
      builder.CreateCall(context.GetLyraQueueDeleteAt(), {handle, index});
      NotifyIfDesignSlot(context, *info.receiver);
      break;
    }

    case mir::BuiltinMethod::kQueuePushBack: {
      llvm::Value* recv_ptr = context.GetPlacePointer(*info.receiver);
      auto qi = GetQueueTypeInfo(context, *info.receiver);

      llvm::Value* val = LowerOperandAsStorage(
          context, compute.value.operands[0], qi.elem_ops.elem_llvm_type);
      auto* temp =
          builder.CreateAlloca(qi.elem_ops.elem_llvm_type, nullptr, "q.pb.tmp");
      builder.CreateStore(val, temp);

      builder.CreateCall(
          context.GetLyraQueuePushBack(),
          {recv_ptr, temp,
           llvm::ConstantInt::get(i32_ty, qi.elem_ops.elem_size),
           llvm::ConstantInt::get(i32_ty, qi.max_bound), qi.elem_ops.clone_fn,
           qi.elem_ops.destroy_fn});

      NotifyIfDesignSlot(context, *info.receiver);
      break;
    }

    case mir::BuiltinMethod::kQueuePushFront: {
      llvm::Value* recv_ptr = context.GetPlacePointer(*info.receiver);
      auto qi = GetQueueTypeInfo(context, *info.receiver);

      llvm::Value* val = LowerOperandAsStorage(
          context, compute.value.operands[0], qi.elem_ops.elem_llvm_type);
      auto* temp =
          builder.CreateAlloca(qi.elem_ops.elem_llvm_type, nullptr, "q.pf.tmp");
      builder.CreateStore(val, temp);

      builder.CreateCall(
          context.GetLyraQueuePushFront(),
          {recv_ptr, temp,
           llvm::ConstantInt::get(i32_ty, qi.elem_ops.elem_size),
           llvm::ConstantInt::get(i32_ty, qi.max_bound), qi.elem_ops.clone_fn,
           qi.elem_ops.destroy_fn});

      NotifyIfDesignSlot(context, *info.receiver);
      break;
    }

    case mir::BuiltinMethod::kQueuePopBack: {
      llvm::Value* recv_ptr = context.GetPlacePointer(*info.receiver);
      llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "q.popb.h");

      // Zero-init target (default if queue is empty)
      llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
      llvm::Type* target_type = context.GetPlaceLlvmType(compute.target);
      builder.CreateStore(
          llvm::Constant::getNullValue(target_type), target_ptr);

      builder.CreateCall(context.GetLyraQueuePopBack(), {handle, target_ptr});
      NotifyIfDesignSlot(context, *info.receiver);
      break;
    }

    case mir::BuiltinMethod::kQueuePopFront: {
      llvm::Value* recv_ptr = context.GetPlacePointer(*info.receiver);
      llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "q.popf.h");

      llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
      llvm::Type* target_type = context.GetPlaceLlvmType(compute.target);
      builder.CreateStore(
          llvm::Constant::getNullValue(target_type), target_ptr);

      builder.CreateCall(context.GetLyraQueuePopFront(), {handle, target_ptr});
      NotifyIfDesignSlot(context, *info.receiver);
      break;
    }

    case mir::BuiltinMethod::kQueueInsert: {
      llvm::Value* recv_ptr = context.GetPlacePointer(*info.receiver);
      auto qi = GetQueueTypeInfo(context, *info.receiver);

      // operand[0] = index, operand[1] = value
      llvm::Value* index = LowerOperand(context, compute.value.operands[0]);
      index = builder.CreateSExtOrTrunc(index, i64_ty, "q.ins.idx");

      llvm::Value* val = LowerOperandAsStorage(
          context, compute.value.operands[1], qi.elem_ops.elem_llvm_type);
      auto* temp = builder.CreateAlloca(
          qi.elem_ops.elem_llvm_type, nullptr, "q.ins.tmp");
      builder.CreateStore(val, temp);

      builder.CreateCall(
          context.GetLyraQueueInsert(),
          {recv_ptr, index, temp,
           llvm::ConstantInt::get(i32_ty, qi.elem_ops.elem_size),
           llvm::ConstantInt::get(i32_ty, qi.max_bound), qi.elem_ops.clone_fn,
           qi.elem_ops.destroy_fn});

      NotifyIfDesignSlot(context, *info.receiver);
      break;
    }

    default:
      throw common::InternalError(
          "LowerQueueBuiltin",
          std::format(
              "unexpected queue builtin: {}", static_cast<int>(info.method)));
  }
}

}  // namespace lyra::lowering::mir_to_llvm
