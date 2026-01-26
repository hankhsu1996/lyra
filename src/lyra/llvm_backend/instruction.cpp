#include "lyra/llvm_backend/instruction.hpp"

#include <cstdint>
#include <expected>
#include <variant>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction_compute.hpp"
#include "lyra/llvm_backend/instruction_display.hpp"
#include "lyra/llvm_backend/instruction_system_tf.hpp"
#include "lyra/llvm_backend/operand.hpp"
#include "lyra/llvm_backend/type_ops.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/effect.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/instruction.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Check if a target place is a design slot (needs notify on write)
auto IsDesignPlace(Context& context, mir::PlaceId place_id) -> bool {
  const auto& place = context.GetMirArena()[place_id];
  return place.root.kind == mir::PlaceRoot::Kind::kDesign;
}

// Get the signal_id for a design place (slot index)
auto GetSignalId(Context& context, mir::PlaceId place_id) -> uint32_t {
  const auto& place = context.GetMirArena()[place_id];
  return static_cast<uint32_t>(place.root.id);
}

// Store a non-string value to a design slot with change notification.
// new_value: the LLVM value to store (integer or 4-state struct)
// target_ptr: pointer to the design slot
// storage_type: LLVM type of the stored value
void StoreDesignWithNotify(
    Context& context, llvm::Value* new_value, llvm::Value* target_ptr,
    llvm::Type* storage_type, mir::PlaceId target_place) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  // Compute byte size from storage type
  auto byte_size = static_cast<uint32_t>(
      context.GetModule().getDataLayout().getTypeAllocSize(storage_type));
  auto signal_id = GetSignalId(context, target_place);

  // Store new value to a temp alloca (notify helper reads from pointer)
  auto* temp = builder.CreateAlloca(storage_type, nullptr, "notify_tmp");
  builder.CreateStore(new_value, temp);

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  builder.CreateCall(
      context.GetLyraStorePacked(),
      {context.GetEnginePointer(), target_ptr, temp,
       llvm::ConstantInt::get(i32_ty, byte_size),
       llvm::ConstantInt::get(i32_ty, signal_id)});
}

// Read-modify-write for BitRangeProjection assignment.
// Clears the target bit range in the base value, then OR's in the new value.
auto StoreBitRange(
    Context& context, mir::PlaceId target, llvm::Value* source_raw)
    -> Result<void> {
  // Check for unsupported case: part-select on local/temp in user functions.
  // User functions don't have frame_ptr_ set, and locals use alloca storage.
  const auto& arena = context.GetMirArena();
  const auto& place = arena[target];
  if ((place.root.kind == mir::PlaceRoot::Kind::kLocal ||
       place.root.kind == mir::PlaceRoot::Kind::kTemp) &&
      context.GetFramePointer() == nullptr) {
    // Find the BitRangeProjection for error origin
    for (const auto& proj : place.projections) {
      if (std::holds_alternative<mir::BitRangeProjection>(proj.info)) {
        common::OriginId blame =
            proj.origin.IsValid() ? proj.origin : context.GetCurrentOrigin();
        return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
            blame,
            "part-select as assignment target in function not yet supported",
            UnsupportedCategory::kFeature));
      }
    }
  }

  auto& builder = context.GetBuilder();
  auto br_result = context.ComposeBitRange(target);
  if (!br_result) return std::unexpected(br_result.error());
  auto [offset, width] = *br_result;

  auto ptr_or_err = context.GetPlacePointer(target);
  if (!ptr_or_err) return std::unexpected(ptr_or_err.error());
  llvm::Value* ptr = *ptr_or_err;
  auto base_type_result = context.GetPlaceBaseType(target);
  if (!base_type_result) return std::unexpected(base_type_result.error());
  llvm::Type* base_type = *base_type_result;

  if (base_type->isStructTy()) {
    // 4-state base: RMW both planes independently
    auto* base_struct = llvm::cast<llvm::StructType>(base_type);
    auto* plane_type = base_struct->getElementType(0);
    uint32_t plane_width = plane_type->getIntegerBitWidth();

    llvm::Value* old_packed = builder.CreateLoad(base_type, ptr, "rmw.old");
    llvm::Value* old_val =
        builder.CreateExtractValue(old_packed, 0, "rmw.old.val");
    llvm::Value* old_unk =
        builder.CreateExtractValue(old_packed, 1, "rmw.old.unk");

    auto* shift_amt =
        builder.CreateZExtOrTrunc(offset, plane_type, "rmw.offset");

    auto mask_ap = llvm::APInt::getLowBitsSet(plane_width, width);
    auto* mask = llvm::ConstantInt::get(plane_type, mask_ap);
    auto* mask_shifted = builder.CreateShl(mask, shift_amt, "rmw.mask");
    auto* not_mask = builder.CreateNot(mask_shifted, "rmw.notmask");

    // Extract source value/unknown planes
    llvm::Value* src_val = nullptr;
    llvm::Value* src_unk = nullptr;
    if (source_raw->getType()->isStructTy()) {
      src_val = builder.CreateExtractValue(source_raw, 0, "rmw.src.val");
      src_unk = builder.CreateExtractValue(source_raw, 1, "rmw.src.unk");
    } else {
      // 2-state source into 4-state target: unknown = 0 in the range
      src_val = source_raw;
      src_unk = llvm::ConstantInt::get(plane_type, 0);
    }

    // Extend source to plane width and shift into position
    src_val = builder.CreateZExtOrTrunc(src_val, plane_type, "rmw.src.val.ext");
    src_unk = builder.CreateZExtOrTrunc(src_unk, plane_type, "rmw.src.unk.ext");
    auto* new_val_shifted =
        builder.CreateShl(src_val, shift_amt, "rmw.val.shl");
    auto* new_unk_shifted =
        builder.CreateShl(src_unk, shift_amt, "rmw.unk.shl");

    // Clear + OR for both planes
    auto* cleared_val = builder.CreateAnd(old_val, not_mask, "rmw.val.clear");
    auto* result_val =
        builder.CreateOr(cleared_val, new_val_shifted, "rmw.val");
    auto* cleared_unk = builder.CreateAnd(old_unk, not_mask, "rmw.unk.clear");
    auto* result_unk =
        builder.CreateOr(cleared_unk, new_unk_shifted, "rmw.unk");

    // Pack and store (with notify if design place)
    llvm::Value* packed = llvm::UndefValue::get(base_struct);
    packed = builder.CreateInsertValue(packed, result_val, 0);
    packed = builder.CreateInsertValue(packed, result_unk, 1);
    if (IsDesignPlace(context, target)) {
      StoreDesignWithNotify(context, packed, ptr, base_type, target);
    } else {
      builder.CreateStore(packed, ptr);
    }
    return {};
  }

  // 2-state base: simple RMW
  uint32_t base_width = base_type->getIntegerBitWidth();
  auto* shift_amt = builder.CreateZExtOrTrunc(offset, base_type, "rmw.offset");

  auto mask_ap = llvm::APInt::getLowBitsSet(base_width, width);
  auto* mask = llvm::ConstantInt::get(base_type, mask_ap);
  auto* mask_shifted = builder.CreateShl(mask, shift_amt, "rmw.mask");
  auto* not_mask = builder.CreateNot(mask_shifted, "rmw.notmask");

  llvm::Value* old_val = builder.CreateLoad(base_type, ptr, "rmw.old");
  auto* cleared = builder.CreateAnd(old_val, not_mask, "rmw.clear");

  // Extend source to base width and shift into position
  llvm::Value* src = source_raw;
  if (src->getType()->isStructTy()) {
    // Coerce 4-state source to 2-state
    auto* val = builder.CreateExtractValue(src, 0, "rmw.src.val");
    auto* unk = builder.CreateExtractValue(src, 1, "rmw.src.unk");
    auto* not_unk = builder.CreateNot(unk);
    src = builder.CreateAnd(val, not_unk);
  }
  src = builder.CreateZExtOrTrunc(src, base_type, "rmw.src.ext");
  auto* new_shifted = builder.CreateShl(src, shift_amt, "rmw.src.shl");
  auto* result = builder.CreateOr(cleared, new_shifted, "rmw.result");
  if (IsDesignPlace(context, target)) {
    StoreDesignWithNotify(context, result, ptr, base_type, target);
  } else {
    builder.CreateStore(result, ptr);
  }
  return {};
}

auto LowerAssign(Context& context, const mir::Assign& assign) -> Result<void> {
  // BitRangeProjection targets use read-modify-write
  if (context.HasBitRangeProjection(assign.target)) {
    auto source_raw_or_err = LowerOperandRaw(context, assign.source);
    if (!source_raw_or_err) return std::unexpected(source_raw_or_err.error());
    return StoreBitRange(context, assign.target, *source_raw_or_err);
  }

  // Delegate all value semantics (ownership, refcounting, field recursion)
  // to the TypeOps layer
  return AssignPlace(context, assign.target, assign.source);
}

auto LowerGuardedAssign(Context& context, const mir::GuardedAssign& guarded)
    -> Result<void> {
  auto& builder = context.GetBuilder();

  // Source is always evaluated unconditionally (per spec)
  auto source_raw_or_err = LowerOperandRaw(context, guarded.source);
  if (!source_raw_or_err) return std::unexpected(source_raw_or_err.error());
  llvm::Value* source_raw = *source_raw_or_err;

  // Lower validity predicate to i1
  auto valid_or_err = LowerOperand(context, guarded.validity);
  if (!valid_or_err) return std::unexpected(valid_or_err.error());
  llvm::Value* valid = *valid_or_err;
  if (valid->getType()->getIntegerBitWidth() > 1) {
    auto* zero = llvm::ConstantInt::get(valid->getType(), 0);
    valid = builder.CreateICmpNE(valid, zero, "ga.tobool");
  }

  // Branch: only write if valid
  auto* func = builder.GetInsertBlock()->getParent();
  auto* do_write_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "ga.write", func);
  auto* skip_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "ga.skip", func);

  builder.CreateCondBr(valid, do_write_bb, skip_bb);

  // Valid path: perform the assignment
  builder.SetInsertPoint(do_write_bb);
  if (context.HasBitRangeProjection(guarded.target)) {
    if (auto result = StoreBitRange(context, guarded.target, source_raw);
        !result) {
      return result;
    }
  } else {
    // Direct store (same as LowerAssign but with pre-computed source_raw)
    auto target_ptr_or_err = context.GetPlacePointer(guarded.target);
    if (!target_ptr_or_err) return std::unexpected(target_ptr_or_err.error());
    llvm::Value* target_ptr = *target_ptr_or_err;
    auto storage_type_or_err = context.GetPlaceLlvmType(guarded.target);
    if (!storage_type_or_err)
      return std::unexpected(storage_type_or_err.error());
    llvm::Type* storage_type = *storage_type_or_err;

    const auto& types = context.GetTypeArena();
    const auto& ga_place = context.GetMirArena()[guarded.target];
    const Type& ga_type = types[mir::TypeOfPlace(types, ga_place)];

    if (ga_type.Kind() == TypeKind::kDynamicArray ||
        ga_type.Kind() == TypeKind::kQueue) {
      // Dynamic array / queue: clone source, store new, release old
      llvm::Value* new_handle = source_raw;
      new_handle = builder.CreateCall(
          context.GetLyraDynArrayClone(), {new_handle}, "ga.da.clone");

      auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
      auto* old_handle = builder.CreateLoad(ptr_ty, target_ptr, "ga.da.old");

      if (IsDesignPlace(context, guarded.target)) {
        auto signal_id = GetSignalId(context, guarded.target);
        auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
        builder.CreateCall(
            context.GetLyraStoreDynArray(),
            {context.GetEnginePointer(), target_ptr, new_handle,
             llvm::ConstantInt::get(i32_ty, signal_id)});
      } else {
        builder.CreateStore(new_handle, target_ptr);
      }

      builder.CreateCall(context.GetLyraDynArrayRelease(), {old_handle});
    } else if (storage_type->isStructTy()) {
      // 4-state target
      auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
      auto* elem_type = struct_type->getElementType(0);

      llvm::Value* val = nullptr;
      llvm::Value* unk = nullptr;
      if (source_raw->getType()->isStructTy()) {
        val = builder.CreateExtractValue(source_raw, 0, "ga.val");
        unk = builder.CreateExtractValue(source_raw, 1, "ga.unk");
      } else {
        val = builder.CreateZExtOrTrunc(source_raw, elem_type, "ga.val");
        unk = llvm::ConstantInt::get(elem_type, 0);
      }
      val = builder.CreateZExtOrTrunc(val, elem_type, "ga.val.fit");
      unk = builder.CreateZExtOrTrunc(unk, elem_type, "ga.unk.fit");

      llvm::Value* packed = llvm::UndefValue::get(struct_type);
      packed = builder.CreateInsertValue(packed, val, 0);
      packed = builder.CreateInsertValue(packed, unk, 1);
      if (IsDesignPlace(context, guarded.target)) {
        StoreDesignWithNotify(
            context, packed, target_ptr, storage_type, guarded.target);
      } else {
        builder.CreateStore(packed, target_ptr);
      }
    } else {
      // 2-state target
      llvm::Value* src = source_raw;
      if (src->getType()->isStructTy()) {
        auto* v = builder.CreateExtractValue(src, 0, "ga.val");
        auto* u = builder.CreateExtractValue(src, 1, "ga.unk");
        auto* not_u = builder.CreateNot(u);
        src = builder.CreateAnd(v, not_u);
      }
      if (src->getType() != storage_type && src->getType()->isIntegerTy() &&
          storage_type->isIntegerTy()) {
        src = builder.CreateZExtOrTrunc(src, storage_type, "ga.fit");
      }
      if (IsDesignPlace(context, guarded.target)) {
        StoreDesignWithNotify(
            context, src, target_ptr, storage_type, guarded.target);
      } else {
        builder.CreateStore(src, target_ptr);
      }
    }
  }
  builder.CreateBr(skip_bb);

  // Continue
  builder.SetInsertPoint(skip_bb);
  return {};
}

// Get the root design slot pointer (before any projections).
auto GetDesignRootPointer(Context& context, mir::PlaceId place_id)
    -> llvm::Value* {
  const auto& place = context.GetMirArena()[place_id];
  auto slot_id = mir::SlotId{static_cast<uint32_t>(place.root.id)};
  uint32_t field_index = context.GetDesignFieldIndex(slot_id);
  return context.GetBuilder().CreateStructGEP(
      context.GetDesignStateType(), context.GetDesignPointer(), field_index,
      "nba.base");
}

// Emit the LyraScheduleNba call with value/mask stored in allocas.
void EmitScheduleNbaCall(
    Context& context, llvm::Value* write_ptr, llvm::Value* notify_base_ptr,
    llvm::Value* value, llvm::Value* mask, llvm::Type* storage_type,
    uint32_t signal_id) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  auto byte_size = static_cast<uint32_t>(
      context.GetModule().getDataLayout().getTypeAllocSize(storage_type));

  auto* val_alloca = builder.CreateAlloca(storage_type, nullptr, "nba.val");
  builder.CreateStore(value, val_alloca);
  auto* mask_alloca = builder.CreateAlloca(storage_type, nullptr, "nba.mask");
  builder.CreateStore(mask, mask_alloca);

  builder.CreateCall(
      context.GetLyraScheduleNba(),
      {context.GetEnginePointer(), write_ptr, notify_base_ptr, val_alloca,
       mask_alloca, llvm::ConstantInt::get(i32_ty, byte_size),
       llvm::ConstantInt::get(i32_ty, signal_id)});
}

auto LowerNonBlockingAssign(Context& context, const mir::NonBlockingAssign& nba)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  const auto& place = arena[nba.target];

  uint32_t signal_id = GetSignalId(context, nba.target);

  // Case 1: BitRangeProjection — shifted value and mask
  if (context.HasBitRangeProjection(nba.target)) {
    auto br_result = context.ComposeBitRange(nba.target);
    if (!br_result) return std::unexpected(br_result.error());
    auto [offset, width] = *br_result;
    auto ptr_or_err = context.GetPlacePointer(nba.target);
    if (!ptr_or_err) return std::unexpected(ptr_or_err.error());
    llvm::Value* ptr = *ptr_or_err;
    auto base_type_result = context.GetPlaceBaseType(nba.target);
    if (!base_type_result) return std::unexpected(base_type_result.error());
    llvm::Type* base_type = *base_type_result;
    llvm::Value* notify_base_ptr = ptr;  // BitRange shares root pointer

    // If there's an IndexProjection before the BitRange, notify_base is root
    for (const auto& proj : place.projections) {
      if (std::holds_alternative<mir::IndexProjection>(proj.info)) {
        notify_base_ptr = GetDesignRootPointer(context, nba.target);
        break;
      }
    }

    if (base_type->isStructTy()) {
      // 4-state base: mask both planes
      auto* base_struct = llvm::cast<llvm::StructType>(base_type);
      auto* plane_type = base_struct->getElementType(0);
      uint32_t plane_width = plane_type->getIntegerBitWidth();

      auto* shift_amt =
          builder.CreateZExtOrTrunc(offset, plane_type, "nba.offset");
      auto mask_ap = llvm::APInt::getLowBitsSet(plane_width, width);
      auto* mask_val = llvm::ConstantInt::get(plane_type, mask_ap);
      auto* mask_shifted = builder.CreateShl(mask_val, shift_amt, "nba.mask");

      // Evaluate source
      auto source_raw_or_err = LowerOperandRaw(context, nba.source);
      if (!source_raw_or_err) return std::unexpected(source_raw_or_err.error());
      llvm::Value* source_raw = *source_raw_or_err;
      llvm::Value* src_val = nullptr;
      llvm::Value* src_unk = nullptr;
      if (source_raw->getType()->isStructTy()) {
        src_val = builder.CreateExtractValue(source_raw, 0, "nba.src.val");
        src_unk = builder.CreateExtractValue(source_raw, 1, "nba.src.unk");
      } else {
        src_val = source_raw;
        src_unk = llvm::ConstantInt::get(plane_type, 0);
      }
      src_val =
          builder.CreateZExtOrTrunc(src_val, plane_type, "nba.src.val.ext");
      src_unk =
          builder.CreateZExtOrTrunc(src_unk, plane_type, "nba.src.unk.ext");
      auto* val_shifted = builder.CreateShl(src_val, shift_amt, "nba.val.shl");
      auto* unk_shifted = builder.CreateShl(src_unk, shift_amt, "nba.unk.shl");

      // Pack into struct for value and mask
      llvm::Value* packed_val = llvm::UndefValue::get(base_struct);
      packed_val = builder.CreateInsertValue(packed_val, val_shifted, 0);
      packed_val = builder.CreateInsertValue(packed_val, unk_shifted, 1);

      llvm::Value* packed_mask = llvm::UndefValue::get(base_struct);
      packed_mask = builder.CreateInsertValue(packed_mask, mask_shifted, 0);
      packed_mask = builder.CreateInsertValue(packed_mask, mask_shifted, 1);

      EmitScheduleNbaCall(
          context, ptr, notify_base_ptr, packed_val, packed_mask, base_type,
          signal_id);
    } else {
      // 2-state base
      uint32_t base_width = base_type->getIntegerBitWidth();
      auto* shift_amt =
          builder.CreateZExtOrTrunc(offset, base_type, "nba.offset");

      auto mask_ap = llvm::APInt::getLowBitsSet(base_width, width);
      auto* mask_val = llvm::ConstantInt::get(base_type, mask_ap);
      auto* mask_shifted = builder.CreateShl(mask_val, shift_amt, "nba.mask");

      // Evaluate source, coerce to base width, shift into position
      auto src_or_err = LowerOperand(context, nba.source);
      if (!src_or_err) return std::unexpected(src_or_err.error());
      llvm::Value* src = *src_or_err;
      src = builder.CreateZExtOrTrunc(src, base_type, "nba.src.ext");
      auto* val_shifted = builder.CreateShl(src, shift_amt, "nba.val.shl");

      EmitScheduleNbaCall(
          context, ptr, notify_base_ptr, val_shifted, mask_shifted, base_type,
          signal_id);
    }
    return {};
  }

  // Case 2: IndexProjection — array element write with OOB guard
  bool has_index_projection = false;
  for (const auto& proj : place.projections) {
    if (std::holds_alternative<mir::IndexProjection>(proj.info)) {
      has_index_projection = true;
      break;
    }
  }

  if (has_index_projection) {
    // Get the index operand and array bounds for OOB check
    const mir::IndexProjection* idx_proj = nullptr;
    TypeId parent_type = place.root.type;
    for (const auto& proj : place.projections) {
      if (const auto* idx = std::get_if<mir::IndexProjection>(&proj.info)) {
        idx_proj = idx;
        break;
      }
    }

    const Type& arr_type = types[parent_type];
    auto arr_size = arr_type.AsUnpackedArray().range.Size();

    // Compute bounds check
    auto index_or_err = LowerOperand(context, idx_proj->index);
    if (!index_or_err) return std::unexpected(index_or_err.error());
    llvm::Value* index = *index_or_err;
    auto* arr_size_val = llvm::ConstantInt::get(index->getType(), arr_size);
    auto* in_bounds =
        builder.CreateICmpULT(index, arr_size_val, "nba.inbounds");

    // Create conditional branch
    auto* func = builder.GetInsertBlock()->getParent();
    auto* schedule_bb =
        llvm::BasicBlock::Create(llvm_ctx, "nba.schedule", func);
    auto* skip_bb = llvm::BasicBlock::Create(llvm_ctx, "nba.skip", func);
    builder.CreateCondBr(in_bounds, schedule_bb, skip_bb);

    // Schedule block: compute pointers, value, mask, call
    builder.SetInsertPoint(schedule_bb);
    auto write_ptr_or_err = context.GetPlacePointer(nba.target);
    if (!write_ptr_or_err) return std::unexpected(write_ptr_or_err.error());
    llvm::Value* write_ptr = *write_ptr_or_err;
    llvm::Value* notify_base_ptr = GetDesignRootPointer(context, nba.target);
    auto storage_type_or_err = context.GetPlaceLlvmType(nba.target);
    if (!storage_type_or_err)
      return std::unexpected(storage_type_or_err.error());
    llvm::Type* storage_type = *storage_type_or_err;

    // Evaluate source and coerce to storage type
    llvm::Value* source_value = nullptr;
    llvm::Value* mask_value = nullptr;
    if (storage_type->isStructTy()) {
      // 4-state element
      auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
      auto* elem_type = struct_type->getElementType(0);
      auto raw_or_err = LowerOperandRaw(context, nba.source);
      if (!raw_or_err) return std::unexpected(raw_or_err.error());
      llvm::Value* raw = *raw_or_err;
      llvm::Value* val = nullptr;
      llvm::Value* unk = nullptr;
      if (raw->getType()->isStructTy()) {
        val = builder.CreateExtractValue(raw, 0);
        unk = builder.CreateExtractValue(raw, 1);
      } else {
        val = builder.CreateZExtOrTrunc(raw, elem_type);
        unk = llvm::ConstantInt::get(elem_type, 0);
      }
      val = builder.CreateZExtOrTrunc(val, elem_type);
      unk = builder.CreateZExtOrTrunc(unk, elem_type);
      source_value = llvm::UndefValue::get(struct_type);
      source_value = builder.CreateInsertValue(source_value, val, 0);
      source_value = builder.CreateInsertValue(source_value, unk, 1);
      // Mask: all-ones for both planes
      auto* all_ones = llvm::ConstantInt::get(
          elem_type, llvm::APInt::getAllOnes(elem_type->getIntegerBitWidth()));
      mask_value = llvm::UndefValue::get(struct_type);
      mask_value = builder.CreateInsertValue(mask_value, all_ones, 0);
      mask_value = builder.CreateInsertValue(mask_value, all_ones, 1);
    } else {
      // 2-state element
      auto source_value_or_err = LowerOperand(context, nba.source);
      if (!source_value_or_err)
        return std::unexpected(source_value_or_err.error());
      source_value = *source_value_or_err;
      if (source_value->getType() != storage_type) {
        source_value = builder.CreateZExtOrTrunc(source_value, storage_type);
      }
      mask_value = llvm::ConstantInt::get(
          storage_type,
          llvm::APInt::getAllOnes(storage_type->getIntegerBitWidth()));
    }

    EmitScheduleNbaCall(
        context, write_ptr, notify_base_ptr, source_value, mask_value,
        storage_type, signal_id);
    builder.CreateBr(skip_bb);

    builder.SetInsertPoint(skip_bb);
    return {};
  }

  // Case 3: Simple full-width write (no projections)
  auto write_ptr_or_err = context.GetPlacePointer(nba.target);
  if (!write_ptr_or_err) return std::unexpected(write_ptr_or_err.error());
  llvm::Value* write_ptr = *write_ptr_or_err;
  llvm::Value* notify_base_ptr = write_ptr;
  auto storage_type_or_err = context.GetPlaceLlvmType(nba.target);
  if (!storage_type_or_err) return std::unexpected(storage_type_or_err.error());
  llvm::Type* storage_type = *storage_type_or_err;

  llvm::Value* source_value = nullptr;
  llvm::Value* mask_value = nullptr;
  if (storage_type->isStructTy()) {
    // 4-state target
    auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
    auto* elem_type = struct_type->getElementType(0);
    auto raw_or_err = LowerOperandRaw(context, nba.source);
    if (!raw_or_err) return std::unexpected(raw_or_err.error());
    llvm::Value* raw = *raw_or_err;
    llvm::Value* val = nullptr;
    llvm::Value* unk = nullptr;
    if (raw->getType()->isStructTy()) {
      val = builder.CreateExtractValue(raw, 0, "nba.val");
      unk = builder.CreateExtractValue(raw, 1, "nba.unk");
    } else {
      val = builder.CreateZExtOrTrunc(raw, elem_type, "nba.val");
      unk = llvm::ConstantInt::get(elem_type, 0);
    }
    val = builder.CreateZExtOrTrunc(val, elem_type, "nba.val.fit");
    unk = builder.CreateZExtOrTrunc(unk, elem_type, "nba.unk.fit");
    source_value = llvm::UndefValue::get(struct_type);
    source_value = builder.CreateInsertValue(source_value, val, 0);
    source_value = builder.CreateInsertValue(source_value, unk, 1);
    // Mask: all-ones for both planes
    auto* all_ones = llvm::ConstantInt::get(
        elem_type, llvm::APInt::getAllOnes(elem_type->getIntegerBitWidth()));
    mask_value = llvm::UndefValue::get(struct_type);
    mask_value = builder.CreateInsertValue(mask_value, all_ones, 0);
    mask_value = builder.CreateInsertValue(mask_value, all_ones, 1);
  } else {
    // 2-state target
    const Type& type = types[mir::TypeOfPlace(types, place)];
    auto source_value_or_err = LowerOperand(context, nba.source);
    if (!source_value_or_err)
      return std::unexpected(source_value_or_err.error());
    source_value = *source_value_or_err;
    if (source_value->getType() != storage_type &&
        source_value->getType()->isIntegerTy() && storage_type->isIntegerTy()) {
      if (type.Kind() == TypeKind::kIntegral && type.AsIntegral().is_signed) {
        source_value = builder.CreateSExtOrTrunc(source_value, storage_type);
      } else {
        source_value = builder.CreateZExtOrTrunc(source_value, storage_type);
      }
    }
    mask_value = llvm::ConstantInt::get(
        storage_type,
        llvm::APInt::getAllOnes(storage_type->getIntegerBitWidth()));
  }

  EmitScheduleNbaCall(
      context, write_ptr, notify_base_ptr, source_value, mask_value,
      storage_type, signal_id);
  return {};
}

auto LowerTimeFormatEffect(Context& context, const mir::TimeFormatEffect& tf)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  // Get engine pointer
  auto* engine_ptr = context.GetEnginePointer();

  // Create global constant for suffix string
  auto* suffix_global =
      builder.CreateGlobalStringPtr(tf.suffix, "timeformat_suffix");

  // Call LyraSetTimeFormat(engine, units, precision, suffix, min_width)
  builder.CreateCall(
      context.GetLyraSetTimeFormat(),
      {engine_ptr,
       llvm::ConstantInt::get(llvm::Type::getInt8Ty(llvm_ctx), tf.units),
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm_ctx), tf.precision),
       suffix_global,
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(llvm_ctx), tf.min_width)});

  return {};
}

auto LowerEffectOp(Context& context, const mir::EffectOp& effect_op)
    -> Result<void> {
  return std::visit(
      common::Overloaded{
          [&context](const mir::DisplayEffect& display) -> Result<void> {
            return LowerDisplayEffect(context, display);
          },
          [](const mir::SeverityEffect& /*severity*/) -> Result<void> {
            // TODO(hankhsu): Handle severity effects
            return {};
          },
          [](const mir::MemIOEffect& /*mem_io*/) -> Result<void> {
            // TODO(hankhsu): Handle mem IO effects
            return {};
          },
          [&context](const mir::TimeFormatEffect& tf) -> Result<void> {
            return LowerTimeFormatEffect(context, tf);
          },
          [&context](const mir::SystemTfEffect& effect) -> Result<void> {
            return LowerSystemTfEffect(context, effect);
          },
      },
      effect_op);
}

}  // namespace

auto LowerInstruction(Context& context, const mir::Instruction& instruction)
    -> Result<void> {
  // Set origin for error reporting.
  // OriginScope preserves outer origin if instruction.origin is Invalid.
  OriginScope origin_scope(context, instruction.origin);

  // RAII guard for statement-scoped cleanup of owned string temps
  StatementScope scope(context);

  return std::visit(
      common::Overloaded{
          [&context](const mir::Assign& assign) -> Result<void> {
            return LowerAssign(context, assign);
          },
          [&context](const mir::Compute& compute) -> Result<void> {
            return LowerCompute(context, compute);
          },
          [&context](const mir::GuardedAssign& guarded) -> Result<void> {
            return LowerGuardedAssign(context, guarded);
          },
          [&context](const mir::Effect& effect) -> Result<void> {
            return LowerEffectOp(context, effect.op);
          },
          [&context](const mir::NonBlockingAssign& nba) -> Result<void> {
            return LowerNonBlockingAssign(context, nba);
          },
      },
      instruction.data);
}

}  // namespace lyra::lowering::mir_to_llvm
