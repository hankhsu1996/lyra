#include "lyra/llvm_backend/instruction_compute_builtin.hpp"

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <format>
#include <vector>

#include "llvm/ADT/APInt.h"
#include "llvm/IR/BasicBlock.h"
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

namespace {

void LowerEnumNextPrev(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info) {
  assert(info.enum_type.has_value());
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  TypeId enum_type_id = *info.enum_type;
  const Type& enum_type = types[enum_type_id];
  const auto& enum_info = enum_type.AsEnum();
  auto member_count = static_cast<uint32_t>(enum_info.members.size());
  assert(member_count > 0);

  uint32_t bit_width = PackedBitWidth(enum_type, types);
  assert(!IsPackedFourState(enum_type, types));

  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
  llvm::Type* value_type = context.GetPlaceLlvmType(compute.target);

  // Load current enum value
  llvm::Value* current_val = LowerOperand(context, compute.value.operands[0]);
  current_val = builder.CreateZExtOrTrunc(current_val, value_type, "enum.val");

  // Build select chain to find current index (first match wins for duplicates)
  llvm::Value* idx = llvm::ConstantInt::get(i32_ty, member_count);  // sentinel
  uint32_t val_bits = value_type->getIntegerBitWidth();
  for (int i = static_cast<int>(member_count) - 1; i >= 0; --i) {
    const auto& member = enum_info.members[i];
    uint32_t build_width = std::max(
        val_bits, static_cast<uint32_t>(member.value.value.size() * 64));
    llvm::APInt member_ap(build_width, 0);
    for (size_t w = 0; w < member.value.value.size(); ++w) {
      member_ap.insertBits(llvm::APInt(64, member.value.value[w]), w * 64);
    }
    member_ap = member_ap.trunc(val_bits);
    auto* member_val = llvm::ConstantInt::get(value_type, member_ap);
    auto* eq = builder.CreateICmpEQ(current_val, member_val, "enum.eq");
    idx = builder.CreateSelect(
        eq, llvm::ConstantInt::get(i32_ty, i), idx, "enum.idx");
  }

  // Handle not-found: default to 0 (next) or member_count-1 (prev)
  uint32_t default_idx =
      (info.method == mir::BuiltinMethod::kEnumNext) ? 0 : member_count - 1;
  auto* valid = builder.CreateICmpULT(
      idx, llvm::ConstantInt::get(i32_ty, member_count), "enum.valid");
  auto* safe_idx = builder.CreateSelect(
      valid, idx, llvm::ConstantInt::get(i32_ty, default_idx), "enum.safe");

  // Get step (operands[1], default 1), truncate to i32
  llvm::Value* step = nullptr;
  if (compute.value.operands.size() > 1) {
    step = LowerOperand(context, compute.value.operands[1]);
    step = builder.CreateIntCast(step, i32_ty, false, "enum.step");
  } else {
    step = llvm::ConstantInt::get(i32_ty, 1);
  }

  // Compute step mod N
  auto* n_const = llvm::ConstantInt::get(i32_ty, member_count);
  auto* step_mod = builder.CreateURem(step, n_const, "enum.step_mod");

  // next: offset = step_mod; prev: offset = N - step_mod
  llvm::Value* offset = nullptr;
  if (info.method == mir::BuiltinMethod::kEnumNext) {
    offset = step_mod;
  } else {
    offset = builder.CreateSub(n_const, step_mod, "enum.prev_off");
  }

  // result_idx = (safe_idx + offset) % N
  auto* sum = builder.CreateAdd(safe_idx, offset, "enum.sum");
  auto* result_idx = builder.CreateURem(sum, n_const, "enum.result_idx");

  // Load from global values array
  auto* global = context.GetOrCreateEnumValuesGlobal(enum_type_id);
  auto* arr_type = llvm::ArrayType::get(value_type, member_count);
  auto* gep = builder.CreateGEP(
      arr_type, global, {builder.getInt32(0), result_idx}, "enum.gep");
  llvm::Value* result = builder.CreateLoad(value_type, gep, "enum.result");

  // Apply width mask
  if (bit_width < value_type->getIntegerBitWidth()) {
    auto mask =
        llvm::APInt::getLowBitsSet(value_type->getIntegerBitWidth(), bit_width);
    result = builder.CreateAnd(
        result, llvm::ConstantInt::get(value_type, mask), "enum.masked");
  }

  // Store to target
  llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
  builder.CreateStore(result, target_ptr);
}

void LowerEnumName(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info) {
  assert(info.enum_type.has_value());
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();

  TypeId enum_type_id = *info.enum_type;
  const Type& enum_type = types[enum_type_id];
  const auto& enum_info = enum_type.AsEnum();
  auto member_count = static_cast<uint32_t>(enum_info.members.size());
  assert(!IsPackedFourState(enum_type, types));

  // enum_name target is string (ptr), but we need the enum's integral type
  // for comparisons. Round up to power-of-2 storage size.
  uint32_t bit_width = PackedBitWidth(enum_type, types);
  uint32_t storage_bits = bit_width;
  if (bit_width <= 8) {
    storage_bits = 8;
  } else if (bit_width <= 16) {
    storage_bits = 16;
  } else if (bit_width <= 32) {
    storage_bits = 32;
  } else if (bit_width <= 64) {
    storage_bits = 64;
  }
  auto* enum_val_type =
      llvm::Type::getIntNTy(context.GetLlvmContext(), storage_bits);

  // Load current enum value
  llvm::Value* current_val = LowerOperand(context, compute.value.operands[0]);
  current_val =
      builder.CreateZExtOrTrunc(current_val, enum_val_type, "enum.name.val");

  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
  auto* i64_ty = llvm::Type::getInt64Ty(context.GetLlvmContext());
  llvm::Function* current_fn = builder.GetInsertBlock()->getParent();

  // Create basic blocks for compare-branch ladder
  std::vector<llvm::BasicBlock*> check_blocks;
  std::vector<llvm::BasicBlock*> match_blocks;
  for (uint32_t i = 0; i < member_count; ++i) {
    check_blocks.push_back(
        llvm::BasicBlock::Create(
            context.GetLlvmContext(), "enum.check", current_fn));
    match_blocks.push_back(
        llvm::BasicBlock::Create(
            context.GetLlvmContext(), "enum.match", current_fn));
  }
  auto* no_match_bb = llvm::BasicBlock::Create(
      context.GetLlvmContext(), "enum.nomatch", current_fn);
  auto* merge_bb = llvm::BasicBlock::Create(
      context.GetLlvmContext(), "enum.merge", current_fn);

  // Branch to first check
  builder.CreateBr(check_blocks[0]);

  // Emit compare-branch ladder
  for (uint32_t i = 0; i < member_count; ++i) {
    builder.SetInsertPoint(check_blocks[i]);
    const auto& member = enum_info.members[i];
    uint32_t build_width = std::max(
        storage_bits, static_cast<uint32_t>(member.value.value.size() * 64));
    llvm::APInt member_ap(build_width, 0);
    for (size_t w = 0; w < member.value.value.size(); ++w) {
      member_ap.insertBits(llvm::APInt(64, member.value.value[w]), w * 64);
    }
    member_ap = member_ap.trunc(storage_bits);
    auto* member_val = llvm::ConstantInt::get(enum_val_type, member_ap);
    auto* eq = builder.CreateICmpEQ(current_val, member_val, "enum.name.eq");
    llvm::BasicBlock* next =
        (i + 1 < member_count) ? check_blocks[i + 1] : no_match_bb;
    builder.CreateCondBr(eq, match_blocks[i], next);
  }

  // Emit match blocks: each creates a string from the member name
  std::vector<llvm::Value*> match_handles;
  match_handles.reserve(member_count);
  for (uint32_t i = 0; i < member_count; ++i) {
    builder.SetInsertPoint(match_blocks[i]);
    const auto& name = enum_info.members[i].name;
    auto* str_data = builder.CreateGlobalStringPtr(name, "enum.str");
    auto* str_len = llvm::ConstantInt::get(i64_ty, name.size());
    match_handles.push_back(builder.CreateCall(
        context.GetLyraStringFromLiteral(), {str_data, str_len},
        "enum.name.handle"));
    builder.CreateBr(merge_bb);
    match_blocks[i] = builder.GetInsertBlock();
  }

  // No-match block: empty string
  builder.SetInsertPoint(no_match_bb);
  auto* empty_data = builder.CreateGlobalStringPtr("", "enum.str.empty");
  auto* empty_len = llvm::ConstantInt::get(i64_ty, 0);
  auto* empty_handle = builder.CreateCall(
      context.GetLyraStringFromLiteral(), {empty_data, empty_len},
      "enum.name.empty");
  builder.CreateBr(merge_bb);
  auto* no_match_end = builder.GetInsertBlock();

  // Merge block: PHI for result
  builder.SetInsertPoint(merge_bb);
  auto* phi = builder.CreatePHI(ptr_ty, member_count + 1, "enum.name.result");
  for (uint32_t i = 0; i < member_count; ++i) {
    phi->addIncoming(match_handles[i], match_blocks[i]);
  }
  phi->addIncoming(empty_handle, no_match_end);

  // String ownership: release old, store new (Pattern B)
  llvm::Value* target_ptr = context.GetPlacePointer(compute.target);
  auto* old_handle = builder.CreateLoad(ptr_ty, target_ptr, "enum.name.old");
  builder.CreateCall(context.GetLyraStringRelease(), {old_handle});
  builder.CreateStore(phi, target_ptr);
}

}  // namespace

void LowerEnumBuiltin(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info) {
  if (info.method == mir::BuiltinMethod::kEnumNext ||
      info.method == mir::BuiltinMethod::kEnumPrev) {
    LowerEnumNextPrev(context, compute, info);
  } else {
    LowerEnumName(context, compute, info);
  }
}

}  // namespace lyra::lowering::mir_to_llvm
