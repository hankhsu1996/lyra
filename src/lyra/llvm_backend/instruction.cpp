#include "lyra/llvm_backend/instruction.hpp"

#include <cstdint>
#include <expected>
#include <variant>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/commit.hpp"
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

  // Get WriteTarget for unified pointer + signal_id
  auto wt_or_err = context.GetWriteTarget(target);
  if (!wt_or_err) return std::unexpected(wt_or_err.error());
  const WriteTarget& wt = *wt_or_err;

  auto base_type_result = context.GetPlaceBaseType(target);
  if (!base_type_result) return std::unexpected(base_type_result.error());
  llvm::Type* base_type = *base_type_result;

  if (base_type->isStructTy()) {
    // 4-state base: RMW both planes independently
    auto* base_struct = llvm::cast<llvm::StructType>(base_type);
    auto* plane_type = base_struct->getElementType(0);
    uint32_t plane_width = plane_type->getIntegerBitWidth();

    llvm::Value* old_packed = builder.CreateLoad(base_type, wt.ptr, "rmw.old");
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
    detail::StorePackedToWriteTarget(context, packed, wt);
    return {};
  }

  // 2-state base: simple RMW
  uint32_t base_width = base_type->getIntegerBitWidth();
  auto* shift_amt = builder.CreateZExtOrTrunc(offset, base_type, "rmw.offset");

  auto mask_ap = llvm::APInt::getLowBitsSet(base_width, width);
  auto* mask = llvm::ConstantInt::get(base_type, mask_ap);
  auto* mask_shifted = builder.CreateShl(mask, shift_amt, "rmw.mask");
  auto* not_mask = builder.CreateNot(mask_shifted, "rmw.notmask");

  llvm::Value* old_val = builder.CreateLoad(base_type, wt.ptr, "rmw.old");
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
  detail::StorePackedToWriteTarget(context, result, wt);
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
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  // Source evaluated BEFORE branch (per SystemVerilog spec)
  auto source_raw_or_err = LowerOperandRaw(context, guarded.source);
  if (!source_raw_or_err) return std::unexpected(source_raw_or_err.error());
  llvm::Value* source_raw = *source_raw_or_err;

  // Validity check (coerce to i1)
  auto valid_or_err = LowerOperand(context, guarded.validity);
  if (!valid_or_err) return std::unexpected(valid_or_err.error());
  llvm::Value* valid = *valid_or_err;
  if (valid->getType()->getIntegerBitWidth() > 1) {
    auto* zero = llvm::ConstantInt::get(valid->getType(), 0);
    valid = builder.CreateICmpNE(valid, zero, "ga.tobool");
  }

  // Branch
  auto* func = builder.GetInsertBlock()->getParent();
  auto* do_write_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "ga.write", func);
  auto* skip_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "ga.skip", func);
  builder.CreateCondBr(valid, do_write_bb, skip_bb);

  // Write path
  builder.SetInsertPoint(do_write_bb);

  // BitRangeProjection: intentional bypass (see invariant in plan)
  if (context.HasBitRangeProjection(guarded.target)) {
    auto result = StoreBitRange(context, guarded.target, source_raw);
    if (!result) return result;
  } else {
    const auto& place = arena[guarded.target];
    TypeId type_id = mir::TypeOfPlace(types, place);

    // INVARIANT: GuardedAssign always uses kClone, never kMove.
    // Reason: We must not mutate/clear the source *place* when the assign is
    // conditional. The source was evaluated BEFORE the validity check (per SV
    // spec), and the source place may still be observable by other code.
    // kClone ensures no "consume source" side effects (no null-out, no
    // release). Do NOT "optimize" this to kMove.
    auto result = CommitValue(
        context, guarded.target, source_raw, type_id, OwnershipPolicy::kClone);
    if (!result) return result;
  }
  builder.CreateBr(skip_bb);

  builder.SetInsertPoint(skip_bb);
  return {};
}

// Get the root design slot pointer (before any projections) after alias
// resolution.
auto GetDesignRootPointer(Context& context, mir::PlaceId place_id)
    -> llvm::Value* {
  // Use resolved signal_id to ensure we get the canonical root
  auto signal_id_opt = context.GetCanonicalRootSignalId(place_id);
  if (!signal_id_opt.has_value()) {
    throw common::InternalError(
        "GetDesignRootPointer", "called on non-design place after resolution");
  }
  auto slot_id = mir::SlotId{*signal_id_opt};
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

  // Use canonical signal_id (after alias resolution) for notification
  // NBA is only valid for design places
  auto signal_id_opt = context.GetCanonicalRootSignalId(nba.target);
  if (!signal_id_opt.has_value()) {
    throw common::InternalError(
        "LowerNonBlockingAssign", "NBA target must resolve to a design place");
  }
  uint32_t signal_id = *signal_id_opt;

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

auto LowerStrobeEffect(Context& context, const mir::StrobeEffect& strobe)
    -> Result<void> {
  auto& builder = context.GetBuilder();

  // Get the thunk function (already declared via DeclareUserFunction)
  llvm::Function* thunk_fn = context.GetUserFunction(strobe.thunk);
  if (thunk_fn == nullptr) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(), "$strobe thunk function not found",
        UnsupportedCategory::kFeature));
  }

  // Get engine and design state pointers from process state header
  llvm::Value* engine_ptr = context.GetEnginePointer();
  llvm::Value* design_ptr = context.GetDesignPointer();

  // Schedule the thunk for the Postponed region
  // LyraSchedulePostponed(engine, callback, design_state)
  builder.CreateCall(
      context.GetLyraSchedulePostponed(), {engine_ptr, thunk_fn, design_ptr});

  return {};
}

auto LowerMonitorEffect(Context& context, const mir::MonitorEffect& monitor)
    -> Result<void> {
  auto& builder = context.GetBuilder();

  // Get the setup thunk function (already declared via DeclareUserFunction).
  // The setup thunk handles: initial print, serialization, and registration.
  llvm::Function* setup_fn = context.GetUserFunction(monitor.setup_thunk);
  if (setup_fn == nullptr) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(), "$monitor setup thunk function not found",
        UnsupportedCategory::kFeature));
  }

  // Get design and engine pointers
  llvm::Value* design_ptr = context.GetDesignPointer();
  llvm::Value* engine_ptr = context.GetEnginePointer();

  // Call setup thunk - it handles initial print + serialization + registration
  // Setup thunk signature: void (DesignState*, Engine*)
  builder.CreateCall(setup_fn, {design_ptr, engine_ptr});

  return {};
}

auto LowerMonitorControlEffect(
    Context& context, const mir::MonitorControlEffect& control)
    -> Result<void> {
  auto& builder = context.GetBuilder();

  // Get engine pointer
  llvm::Value* engine_ptr = context.GetEnginePointer();

  // Call LyraMonitorSetEnabled(engine, enable)
  auto& llvm_ctx = context.GetLlvmContext();
  builder.CreateCall(
      context.GetLyraMonitorSetEnabled(),
      {engine_ptr,
       llvm::ConstantInt::get(
           llvm::Type::getInt1Ty(llvm_ctx), control.enable ? 1 : 0)});

  return {};
}

auto LowerMemIOEffect(Context& context, const mir::MemIOEffect& mem_io)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();

  // Get array type info from target_type
  const Type& arr_type = types[mem_io.target_type];
  if (arr_type.Kind() != TypeKind::kUnpackedArray) {
    throw common::InternalError(
        "LowerMemIOEffect", "target must be unpacked array");
  }
  const auto& arr_info = arr_type.AsUnpackedArray();
  TypeId elem_type_id = arr_info.element_type;
  const Type& elem_type = types[elem_type_id];

  // Restriction: packed integrals only
  if (!IsPacked(elem_type)) {
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        "$readmem/$writemem: only packed integral elements supported",
        UnsupportedCategory::kType));
  }

  // Determine if 4-state
  bool is_four_state = IsPackedFourState(elem_type, types);

  // Extract type info
  auto element_width = static_cast<int32_t>(PackedBitWidth(elem_type, types));
  auto element_count = static_cast<int32_t>(arr_info.range.Size());
  auto min_addr = static_cast<int64_t>(arr_info.range.Lower());

  // Compute stride_bytes and value_size_bytes from DataLayout (authoritative)
  auto elem_ops_result = context.GetElemOpsForType(elem_type_id);
  if (!elem_ops_result) return std::unexpected(elem_ops_result.error());
  llvm::Type* elem_llvm_type = elem_ops_result->elem_llvm_type;
  int32_t stride_bytes = elem_ops_result->elem_size;

  int32_t value_size_bytes = stride_bytes;
  if (is_four_state) {
    // For 4-state struct {value, x_mask}, value_size is half the stride
    auto* struct_ty = llvm::cast<llvm::StructType>(elem_llvm_type);
    const auto& dl = context.GetModule().getDataLayout();
    value_size_bytes =
        static_cast<int32_t>(dl.getTypeAllocSize(struct_ty->getElementType(0)));
  }

  int32_t element_kind = is_four_state ? 1 : 0;

  // Get target pointer
  auto target_ptr_result = context.GetPlacePointer(mem_io.target);
  if (!target_ptr_result) return std::unexpected(target_ptr_result.error());
  llvm::Value* target_ptr = *target_ptr_result;

  // Lower filename operand (string handle)
  auto filename_result = LowerOperand(context, mem_io.filename);
  if (!filename_result) return std::unexpected(filename_result.error());
  llvm::Value* filename_handle = *filename_result;

  auto* i64_ty = llvm::Type::getInt64Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* i1_ty = llvm::Type::getInt1Ty(llvm_ctx);

  // Determine step from array direction
  int64_t step = arr_info.range.IsDescending() ? -1 : 1;
  int64_t left = arr_info.range.left;
  int64_t right = arr_info.range.right;

  // Default: left_bound -> right_bound (IEEE LRM semantics)
  llvm::Value* eff_start = llvm::ConstantInt::get(i64_ty, left);
  if (mem_io.start_addr) {
    auto start_result = LowerOperand(context, *mem_io.start_addr);
    if (!start_result) return std::unexpected(start_result.error());
    eff_start = builder.CreateSExtOrTrunc(*start_result, i64_ty, "eff_start");
  }

  llvm::Value* eff_end = llvm::ConstantInt::get(i64_ty, right);
  if (mem_io.end_addr) {
    auto end_result = LowerOperand(context, *mem_io.end_addr);
    if (!end_result) return std::unexpected(end_result.error());
    eff_end = builder.CreateSExtOrTrunc(*end_result, i64_ty, "eff_end");
  }

  // Compute current/final consistent with step direction
  // step > 0: current = min(start,end), final = max(start,end)
  // step < 0: current = max(start,end), final = min(start,end)
  llvm::Value* cmp = builder.CreateICmpSLT(eff_start, eff_end, "start_lt_end");
  llvm::Value* low = builder.CreateSelect(cmp, eff_start, eff_end, "eff_low");
  llvm::Value* high = builder.CreateSelect(cmp, eff_end, eff_start, "eff_high");

  llvm::Value* current_addr = (step > 0) ? low : high;
  llvm::Value* final_addr = (step > 0) ? high : low;

  // Emit runtime call
  std::vector<llvm::Value*> args = {
      filename_handle,
      target_ptr,
      llvm::ConstantInt::get(i32_ty, element_width),
      llvm::ConstantInt::get(i32_ty, stride_bytes),
      llvm::ConstantInt::get(i32_ty, value_size_bytes),
      llvm::ConstantInt::get(i32_ty, element_count),
      llvm::ConstantInt::get(i64_ty, min_addr),
      current_addr,
      final_addr,
      llvm::ConstantInt::get(i64_ty, step),
      llvm::ConstantInt::get(i1_ty, mem_io.is_hex ? 1 : 0),
      llvm::ConstantInt::get(i32_ty, element_kind),
  };

  if (mem_io.is_read) {
    builder.CreateCall(context.GetLyraReadmem(), args);
  } else {
    builder.CreateCall(context.GetLyraWritemem(), args);
  }

  return {};
}

auto LowerEffectOp(Context& context, const mir::EffectOp& effect_op)
    -> Result<void> {
  return std::visit(
      common::Overloaded{
          [&context](const mir::DisplayEffect& display) -> Result<void> {
            return LowerDisplayEffect(context, display);
          },
          [&context](const mir::SeverityEffect& severity) -> Result<void> {
            return LowerSeverityEffect(context, severity);
          },
          [&context](const mir::MemIOEffect& mem_io) -> Result<void> {
            return LowerMemIOEffect(context, mem_io);
          },
          [&context](const mir::TimeFormatEffect& tf) -> Result<void> {
            return LowerTimeFormatEffect(context, tf);
          },
          [&context](const mir::SystemTfEffect& effect) -> Result<void> {
            return LowerSystemTfEffect(context, effect);
          },
          [&context](const mir::StrobeEffect& strobe) -> Result<void> {
            return LowerStrobeEffect(context, strobe);
          },
          [&context](const mir::MonitorEffect& monitor) -> Result<void> {
            return LowerMonitorEffect(context, monitor);
          },
          [&context](const mir::MonitorControlEffect& control) -> Result<void> {
            return LowerMonitorControlEffect(context, control);
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
