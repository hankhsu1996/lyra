#include "lyra/llvm_backend/instruction_compute_builtin.hpp"

#include <algorithm>
#include <cassert>
#include <cstdint>
#include <expected>
#include <format>
#include <vector>

#include "llvm/ADT/APInt.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

auto NotifyIfDesignSlot(Context& context, mir::PlaceId receiver)
    -> Result<void> {
  const auto& arena = context.GetMirArena();
  const auto& place = arena[receiver];
  if (place.root.kind != mir::PlaceRoot::Kind::kDesign) {
    return {};
  }
  auto& builder = context.GetBuilder();
  auto signal_id = static_cast<uint32_t>(place.root.id);
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());

  auto recv_ptr_or_err = context.GetPlacePointer(receiver);
  if (!recv_ptr_or_err) {
    return std::unexpected(recv_ptr_or_err.error());
  }
  llvm::Value* recv_ptr = *recv_ptr_or_err;

  llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "q.notify.h");
  builder.CreateCall(
      context.GetLyraStoreDynArray(),
      {context.GetEnginePointer(), recv_ptr, handle,
       llvm::ConstantInt::get(i32_ty, signal_id)});
  return {};
}

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
  if (!elem_ops) {
    return std::unexpected(elem_ops.error());
  }
  return QueueTypeInfo{
      .elem_type_id = elem_type_id,
      .max_bound = queue_type.AsQueue().max_bound,
      .elem_ops = *elem_ops,
  };
}

}  // namespace

auto LowerDynArrayBuiltin(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info) -> Result<void> {
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
      auto elem_ops_result = context.GetElemOpsForType(elem_type_id);
      if (!elem_ops_result) {
        return std::unexpected(elem_ops_result.error());
      }
      auto elem_ops = *elem_ops_result;

      // Operand 0 = size
      auto size_or_err = LowerOperand(context, compute.value.operands[0]);
      if (!size_or_err) {
        return std::unexpected(size_or_err.error());
      }
      llvm::Value* size = *size_or_err;
      auto* i64_ty = llvm::Type::getInt64Ty(context.GetLlvmContext());
      auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
      size = builder.CreateSExtOrTrunc(size, i64_ty, "da.new.size");

      auto* elem_size_val = llvm::ConstantInt::get(i32_ty, elem_ops.elem_size);

      llvm::Value* handle = nullptr;
      if (compute.value.operands.size() >= 2) {
        // new[size](src): copy from existing array
        auto src_or_err = LowerOperand(context, compute.value.operands[1]);
        if (!src_or_err) {
          return std::unexpected(src_or_err.error());
        }
        llvm::Value* src = *src_or_err;
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
      auto target_ptr_or_err = context.GetPlacePointer(compute.target);
      if (!target_ptr_or_err) {
        return std::unexpected(target_ptr_or_err.error());
      }
      llvm::Value* target_ptr = *target_ptr_or_err;

      auto* old_handle = builder.CreateLoad(ptr_ty, target_ptr, "da.new.old");
      builder.CreateCall(context.GetLyraDynArrayRelease(), {old_handle});
      builder.CreateStore(handle, target_ptr);
      return {};
    }

    case mir::BuiltinMethod::kArraySize: {
      // Operand 0 = array (Use of place, loaded as handle)
      auto handle_or_err = LowerOperand(context, compute.value.operands[0]);
      if (!handle_or_err) {
        return std::unexpected(handle_or_err.error());
      }
      llvm::Value* handle = *handle_or_err;

      llvm::Value* size = builder.CreateCall(
          context.GetLyraDynArraySize(), {handle}, "da.size");

      // Fit i64 result to target storage type
      auto target_ptr_or_err = context.GetPlacePointer(compute.target);
      if (!target_ptr_or_err) {
        return std::unexpected(target_ptr_or_err.error());
      }
      llvm::Value* target_ptr = *target_ptr_or_err;

      auto target_type_or_err = context.GetPlaceLlvmType(compute.target);
      if (!target_type_or_err) {
        return std::unexpected(target_type_or_err.error());
      }
      llvm::Type* target_type = *target_type_or_err;

      size = builder.CreateZExtOrTrunc(size, target_type, "da.size.fit");
      builder.CreateStore(size, target_ptr);
      return {};
    }

    case mir::BuiltinMethod::kArrayDelete: {
      // Load handle from receiver
      auto recv_ptr_or_err = context.GetPlacePointer(*info.receiver);
      if (!recv_ptr_or_err) {
        return std::unexpected(recv_ptr_or_err.error());
      }
      llvm::Value* recv_ptr = *recv_ptr_or_err;

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
      return {};
    }

    default:
      return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
          context.GetCurrentOrigin(),
          std::format(
              "unsupported builtin method: {}", static_cast<int>(info.method)),
          UnsupportedCategory::kFeature));
  }
}

auto LowerQueueBuiltin(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
  auto* i64_ty = llvm::Type::getInt64Ty(context.GetLlvmContext());

  switch (info.method) {
    case mir::BuiltinMethod::kQueueSize: {
      auto handle_or_err = LowerOperand(context, compute.value.operands[0]);
      if (!handle_or_err) {
        return std::unexpected(handle_or_err.error());
      }
      llvm::Value* handle = *handle_or_err;

      llvm::Value* size =
          builder.CreateCall(context.GetLyraDynArraySize(), {handle}, "q.size");

      auto target_ptr_or_err = context.GetPlacePointer(compute.target);
      if (!target_ptr_or_err) {
        return std::unexpected(target_ptr_or_err.error());
      }
      llvm::Value* target_ptr = *target_ptr_or_err;

      auto target_type_or_err = context.GetPlaceLlvmType(compute.target);
      if (!target_type_or_err) {
        return std::unexpected(target_type_or_err.error());
      }
      llvm::Type* target_type = *target_type_or_err;

      size = builder.CreateZExtOrTrunc(size, target_type, "q.size.fit");
      builder.CreateStore(size, target_ptr);
      return {};
    }

    case mir::BuiltinMethod::kQueueDelete: {
      auto recv_ptr_or_err = context.GetPlacePointer(*info.receiver);
      if (!recv_ptr_or_err) {
        return std::unexpected(recv_ptr_or_err.error());
      }
      llvm::Value* recv_ptr = *recv_ptr_or_err;

      llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "q.del.h");
      builder.CreateCall(context.GetLyraDynArrayRelease(), {handle});
      builder.CreateStore(llvm::Constant::getNullValue(ptr_ty), recv_ptr);
      return NotifyIfDesignSlot(context, *info.receiver);
    }

    case mir::BuiltinMethod::kQueueDeleteAt: {
      auto recv_ptr_or_err = context.GetPlacePointer(*info.receiver);
      if (!recv_ptr_or_err) {
        return std::unexpected(recv_ptr_or_err.error());
      }
      llvm::Value* recv_ptr = *recv_ptr_or_err;

      llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "q.delat.h");

      auto index_or_err = LowerOperand(context, compute.value.operands[0]);
      if (!index_or_err) {
        return std::unexpected(index_or_err.error());
      }
      llvm::Value* index = *index_or_err;

      index = builder.CreateSExtOrTrunc(index, i64_ty, "q.delat.idx");
      builder.CreateCall(context.GetLyraQueueDeleteAt(), {handle, index});
      return NotifyIfDesignSlot(context, *info.receiver);
    }

    case mir::BuiltinMethod::kQueuePushBack: {
      auto recv_ptr_or_err = context.GetPlacePointer(*info.receiver);
      if (!recv_ptr_or_err) {
        return std::unexpected(recv_ptr_or_err.error());
      }
      llvm::Value* recv_ptr = *recv_ptr_or_err;

      auto qi_result = GetQueueTypeInfo(context, *info.receiver);
      if (!qi_result) {
        return std::unexpected(qi_result.error());
      }
      auto qi = *qi_result;

      auto val_or_err = LowerOperandAsStorage(
          context, compute.value.operands[0], qi.elem_ops.elem_llvm_type);
      if (!val_or_err) {
        return std::unexpected(val_or_err.error());
      }
      llvm::Value* val = *val_or_err;

      auto* temp =
          builder.CreateAlloca(qi.elem_ops.elem_llvm_type, nullptr, "q.pb.tmp");
      builder.CreateStore(val, temp);

      builder.CreateCall(
          context.GetLyraQueuePushBack(),
          {recv_ptr, temp,
           llvm::ConstantInt::get(i32_ty, qi.elem_ops.elem_size),
           llvm::ConstantInt::get(i32_ty, qi.max_bound), qi.elem_ops.clone_fn,
           qi.elem_ops.destroy_fn});

      return NotifyIfDesignSlot(context, *info.receiver);
    }

    case mir::BuiltinMethod::kQueuePushFront: {
      auto recv_ptr_or_err = context.GetPlacePointer(*info.receiver);
      if (!recv_ptr_or_err) {
        return std::unexpected(recv_ptr_or_err.error());
      }
      llvm::Value* recv_ptr = *recv_ptr_or_err;

      auto qi_result = GetQueueTypeInfo(context, *info.receiver);
      if (!qi_result) {
        return std::unexpected(qi_result.error());
      }
      auto qi = *qi_result;

      auto val_or_err = LowerOperandAsStorage(
          context, compute.value.operands[0], qi.elem_ops.elem_llvm_type);
      if (!val_or_err) {
        return std::unexpected(val_or_err.error());
      }
      llvm::Value* val = *val_or_err;

      auto* temp =
          builder.CreateAlloca(qi.elem_ops.elem_llvm_type, nullptr, "q.pf.tmp");
      builder.CreateStore(val, temp);

      builder.CreateCall(
          context.GetLyraQueuePushFront(),
          {recv_ptr, temp,
           llvm::ConstantInt::get(i32_ty, qi.elem_ops.elem_size),
           llvm::ConstantInt::get(i32_ty, qi.max_bound), qi.elem_ops.clone_fn,
           qi.elem_ops.destroy_fn});

      return NotifyIfDesignSlot(context, *info.receiver);
    }

    case mir::BuiltinMethod::kQueuePopBack: {
      auto recv_ptr_or_err = context.GetPlacePointer(*info.receiver);
      if (!recv_ptr_or_err) {
        return std::unexpected(recv_ptr_or_err.error());
      }
      llvm::Value* recv_ptr = *recv_ptr_or_err;

      llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "q.popb.h");

      // Zero-init target (default if queue is empty)
      auto target_ptr_or_err = context.GetPlacePointer(compute.target);
      if (!target_ptr_or_err) {
        return std::unexpected(target_ptr_or_err.error());
      }
      llvm::Value* target_ptr = *target_ptr_or_err;

      auto target_type_or_err = context.GetPlaceLlvmType(compute.target);
      if (!target_type_or_err) {
        return std::unexpected(target_type_or_err.error());
      }
      llvm::Type* target_type = *target_type_or_err;

      builder.CreateStore(
          llvm::Constant::getNullValue(target_type), target_ptr);

      builder.CreateCall(context.GetLyraQueuePopBack(), {handle, target_ptr});
      return NotifyIfDesignSlot(context, *info.receiver);
    }

    case mir::BuiltinMethod::kQueuePopFront: {
      auto recv_ptr_or_err = context.GetPlacePointer(*info.receiver);
      if (!recv_ptr_or_err) {
        return std::unexpected(recv_ptr_or_err.error());
      }
      llvm::Value* recv_ptr = *recv_ptr_or_err;

      llvm::Value* handle = builder.CreateLoad(ptr_ty, recv_ptr, "q.popf.h");

      auto target_ptr_or_err = context.GetPlacePointer(compute.target);
      if (!target_ptr_or_err) {
        return std::unexpected(target_ptr_or_err.error());
      }
      llvm::Value* target_ptr = *target_ptr_or_err;

      auto target_type_or_err = context.GetPlaceLlvmType(compute.target);
      if (!target_type_or_err) {
        return std::unexpected(target_type_or_err.error());
      }
      llvm::Type* target_type = *target_type_or_err;

      builder.CreateStore(
          llvm::Constant::getNullValue(target_type), target_ptr);

      builder.CreateCall(context.GetLyraQueuePopFront(), {handle, target_ptr});
      return NotifyIfDesignSlot(context, *info.receiver);
    }

    case mir::BuiltinMethod::kQueueInsert: {
      auto recv_ptr_or_err = context.GetPlacePointer(*info.receiver);
      if (!recv_ptr_or_err) {
        return std::unexpected(recv_ptr_or_err.error());
      }
      llvm::Value* recv_ptr = *recv_ptr_or_err;

      auto qi_result = GetQueueTypeInfo(context, *info.receiver);
      if (!qi_result) {
        return std::unexpected(qi_result.error());
      }
      auto qi = *qi_result;

      // operand[0] = index, operand[1] = value
      auto index_or_err = LowerOperand(context, compute.value.operands[0]);
      if (!index_or_err) {
        return std::unexpected(index_or_err.error());
      }
      llvm::Value* index = *index_or_err;
      index = builder.CreateSExtOrTrunc(index, i64_ty, "q.ins.idx");

      auto val_or_err = LowerOperandAsStorage(
          context, compute.value.operands[1], qi.elem_ops.elem_llvm_type);
      if (!val_or_err) {
        return std::unexpected(val_or_err.error());
      }
      llvm::Value* val = *val_or_err;

      auto* temp = builder.CreateAlloca(
          qi.elem_ops.elem_llvm_type, nullptr, "q.ins.tmp");
      builder.CreateStore(val, temp);

      builder.CreateCall(
          context.GetLyraQueueInsert(),
          {recv_ptr, index, temp,
           llvm::ConstantInt::get(i32_ty, qi.elem_ops.elem_size),
           llvm::ConstantInt::get(i32_ty, qi.max_bound), qi.elem_ops.clone_fn,
           qi.elem_ops.destroy_fn});

      return NotifyIfDesignSlot(context, *info.receiver);
    }

    default:
      throw common::InternalError(
          "LowerQueueBuiltin",
          std::format(
              "unexpected queue builtin: {}", static_cast<int>(info.method)));
  }
}

namespace {

auto LowerEnumNextPrev(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info) -> Result<void> {
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

  auto value_type_or_err = context.GetPlaceLlvmType(compute.target);
  if (!value_type_or_err) {
    return std::unexpected(value_type_or_err.error());
  }
  llvm::Type* value_type = *value_type_or_err;

  // Load current enum value
  auto current_val_or_err = LowerOperand(context, compute.value.operands[0]);
  if (!current_val_or_err) {
    return std::unexpected(current_val_or_err.error());
  }
  llvm::Value* current_val = *current_val_or_err;
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
    auto step_or_err = LowerOperand(context, compute.value.operands[1]);
    if (!step_or_err) {
      return std::unexpected(step_or_err.error());
    }
    step = *step_or_err;
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
  auto target_ptr_or_err = context.GetPlacePointer(compute.target);
  if (!target_ptr_or_err) {
    return std::unexpected(target_ptr_or_err.error());
  }
  llvm::Value* target_ptr = *target_ptr_or_err;

  builder.CreateStore(result, target_ptr);
  return {};
}

auto LowerEnumName(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info) -> Result<void> {
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
  auto current_val_or_err = LowerOperand(context, compute.value.operands[0]);
  if (!current_val_or_err) {
    return std::unexpected(current_val_or_err.error());
  }
  llvm::Value* current_val = *current_val_or_err;
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
  auto target_ptr_or_err = context.GetPlacePointer(compute.target);
  if (!target_ptr_or_err) {
    return std::unexpected(target_ptr_or_err.error());
  }
  llvm::Value* target_ptr = *target_ptr_or_err;

  auto* old_handle = builder.CreateLoad(ptr_ty, target_ptr, "enum.name.old");
  builder.CreateCall(context.GetLyraStringRelease(), {old_handle});
  builder.CreateStore(phi, target_ptr);
  return {};
}

}  // namespace

auto LowerEnumBuiltin(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info) -> Result<void> {
  if (info.method == mir::BuiltinMethod::kEnumNext ||
      info.method == mir::BuiltinMethod::kEnumPrev) {
    return LowerEnumNextPrev(context, compute, info);
  }
  return LowerEnumName(context, compute, info);
}

auto LowerBuiltin(
    Context& context, const mir::Compute& compute,
    const mir::BuiltinCallRvalueInfo& info) -> Result<void> {
  switch (info.method) {
    case mir::BuiltinMethod::kNewArray:
    case mir::BuiltinMethod::kArraySize:
    case mir::BuiltinMethod::kArrayDelete:
      return LowerDynArrayBuiltin(context, compute, info);
    case mir::BuiltinMethod::kQueueSize:
    case mir::BuiltinMethod::kQueueDelete:
    case mir::BuiltinMethod::kQueueDeleteAt:
    case mir::BuiltinMethod::kQueuePushBack:
    case mir::BuiltinMethod::kQueuePushFront:
    case mir::BuiltinMethod::kQueuePopBack:
    case mir::BuiltinMethod::kQueuePopFront:
    case mir::BuiltinMethod::kQueueInsert:
      return LowerQueueBuiltin(context, compute, info);
    case mir::BuiltinMethod::kEnumNext:
    case mir::BuiltinMethod::kEnumPrev:
    case mir::BuiltinMethod::kEnumName:
      return LowerEnumBuiltin(context, compute, info);
  }
  return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
      context.GetCurrentOrigin(),
      std::format(
          "unsupported builtin method: {}", static_cast<int>(info.method)),
      UnsupportedCategory::kFeature));
}

}  // namespace lyra::lowering::mir_to_llvm
