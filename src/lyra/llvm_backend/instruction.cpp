#include "lyra/llvm_backend/instruction.hpp"

#include <cstdint>
#include <variant>

#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/Casting.h"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/common/unsupported_error.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction_compute.hpp"
#include "lyra/llvm_backend/instruction_display.hpp"
#include "lyra/llvm_backend/operand.hpp"
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
void StoreBitRange(
    Context& context, mir::PlaceId target, llvm::Value* source_raw) {
  auto& builder = context.GetBuilder();
  auto [offset, width] = context.ComposeBitRange(target);

  llvm::Value* ptr = context.GetPlacePointer(target);
  llvm::Type* base_type = context.GetPlaceBaseType(target);

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
    return;
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
}

void LowerAssign(Context& context, const mir::Assign& assign) {
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  // BitRangeProjection targets use read-modify-write
  if (context.HasBitRangeProjection(assign.target)) {
    llvm::Value* source_raw = LowerOperandRaw(context, assign.source);
    StoreBitRange(context, assign.target, source_raw);
    return;
  }

  // Get pointer to target place storage
  llvm::Value* target_ptr = context.GetPlacePointer(assign.target);
  llvm::Type* storage_type = context.GetPlaceLlvmType(assign.target);

  // Get the effective type (element type if projected, root type otherwise)
  const auto& place = arena[assign.target];
  const Type& type = types[mir::TypeOfPlace(types, place)];

  // Handle string assignment with reference counting
  if (type.Kind() == TypeKind::kString) {
    // 1. Get new value
    llvm::Value* new_val = LowerOperand(context, assign.source);

    // 2. If source is a place reference (borrowed), retain to get owned
    if (std::holds_alternative<mir::PlaceId>(assign.source.payload)) {
      new_val = builder.CreateCall(context.GetLyraStringRetain(), {new_val});
    }

    if (IsDesignPlace(context, assign.target)) {
      // 3. Load old value before helper overwrites it
      auto* old_val = builder.CreateLoad(storage_type, target_ptr);
      // 4. Store + notify via helper
      auto signal_id = GetSignalId(context, assign.target);
      auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
      builder.CreateCall(
          context.GetLyraStoreString(),
          {context.GetEnginePointer(), target_ptr, new_val,
           llvm::ConstantInt::get(i32_ty, signal_id)});
      // 5. Release old value
      builder.CreateCall(context.GetLyraStringRelease(), {old_val});
    } else {
      // 3. Release old value in slot
      auto* old_val = builder.CreateLoad(storage_type, target_ptr);
      builder.CreateCall(context.GetLyraStringRelease(), {old_val});
      // 4. Store owned value (slot now owns)
      builder.CreateStore(new_val, target_ptr);
    }
    return;
  }

  // Dynamic array / queue: clone/move semantics with ownership tracking
  if (type.Kind() == TypeKind::kDynamicArray ||
      type.Kind() == TypeKind::kQueue) {
    auto* ptr_ty = llvm::PointerType::getUnqual(context.GetLlvmContext());
    llvm::Value* new_handle = LowerOperand(context, assign.source);

    // Determine clone vs move based on source kind.
    // MIR invariant: temps are single-owner; Use(temp) is a consuming move
    // and the temp will not be read again in this statement.
    if (std::holds_alternative<mir::PlaceId>(assign.source.payload)) {
      auto src_place_id = std::get<mir::PlaceId>(assign.source.payload);
      const auto& src_place = arena[src_place_id];
      if (src_place.root.kind == mir::PlaceRoot::Kind::kTemp) {
        // Move: null-out the temp to transfer ownership
        auto* src_ptr = context.GetPlacePointer(src_place_id);
        auto* null_val = llvm::ConstantPointerNull::get(
            llvm::cast<llvm::PointerType>(ptr_ty));
        builder.CreateStore(null_val, src_ptr);
      } else {
        // Clone: persistent place, need independent copy
        new_handle = builder.CreateCall(
            context.GetLyraDynArrayClone(), {new_handle}, "da.clone");
      }
    }
    // else: source is a Const (nullptr literal) — use as-is

    // Load old handle before store overwrites it
    auto* old_handle = builder.CreateLoad(ptr_ty, target_ptr, "da.old");

    // Store new handle (with design notify if needed)
    if (IsDesignPlace(context, assign.target)) {
      auto signal_id = GetSignalId(context, assign.target);
      auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());
      builder.CreateCall(
          context.GetLyraStoreDynArray(),
          {context.GetEnginePointer(), target_ptr, new_handle,
           llvm::ConstantInt::get(i32_ty, signal_id)});
    } else {
      builder.CreateStore(new_handle, target_ptr);
    }

    // Release old handle after store
    builder.CreateCall(context.GetLyraDynArrayRelease(), {old_handle});
    return;
  }

  // Unpacked array: aggregate store (only valid for POD element types)
  if (type.Kind() == TypeKind::kUnpackedArray) {
    const auto& arr_info = type.AsUnpackedArray();
    const Type& elem_type = types[arr_info.element_type];
    if (elem_type.Kind() == TypeKind::kDynamicArray ||
        elem_type.Kind() == TypeKind::kQueue ||
        elem_type.Kind() == TypeKind::kString) {
      throw common::UnsupportedErrorException(
          common::UnsupportedLayer::kMirToLlvm,
          common::UnsupportedKind::kFeature, context.GetCurrentOrigin(),
          "unpacked array assignment with owned-handle elements "
          "(dynamic array or string) not yet supported");
    }

    llvm::Value* val = LowerOperandRaw(context, assign.source);
    if (IsDesignPlace(context, assign.target)) {
      StoreDesignWithNotify(
          context, val, target_ptr, storage_type, assign.target);
    } else {
      builder.CreateStore(val, target_ptr);
    }
    return;
  }

  // 4-state target: construct {value, unknown=0} struct
  if (storage_type->isStructTy()) {
    auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
    auto* elem_type = struct_type->getElementType(0);

    // Load source as raw value (struct if 4-state, integer if 2-state)
    llvm::Value* raw = LowerOperandRaw(context, assign.source);

    llvm::Value* val = nullptr;
    llvm::Value* unk = nullptr;
    if (raw->getType()->isStructTy()) {
      // Source is 4-state: pass through
      val = builder.CreateExtractValue(raw, 0, "assign.val");
      unk = builder.CreateExtractValue(raw, 1, "assign.unk");
    } else {
      // Source is 2-state: wrap as {value, unknown=0}
      val = builder.CreateZExtOrTrunc(raw, elem_type, "assign.val");
      unk = llvm::ConstantInt::get(elem_type, 0);
    }

    // Coerce to elem_type if width differs
    val = builder.CreateZExtOrTrunc(val, elem_type, "assign.val.fit");
    unk = builder.CreateZExtOrTrunc(unk, elem_type, "assign.unk.fit");

    llvm::Value* packed = llvm::UndefValue::get(struct_type);
    packed = builder.CreateInsertValue(packed, val, 0);
    packed = builder.CreateInsertValue(packed, unk, 1);
    if (IsDesignPlace(context, assign.target)) {
      StoreDesignWithNotify(
          context, packed, target_ptr, storage_type, assign.target);
    } else {
      builder.CreateStore(packed, target_ptr);
    }
    return;
  }

  // 2-state target: lower source (coerces 4-state to integer)
  llvm::Value* source_value = LowerOperand(context, assign.source);

  // Adjust the value to match storage type if needed (only for integrals)
  if (source_value->getType() != storage_type &&
      source_value->getType()->isIntegerTy() && storage_type->isIntegerTy()) {
    if (type.Kind() == TypeKind::kIntegral && type.AsIntegral().is_signed) {
      source_value = builder.CreateSExtOrTrunc(source_value, storage_type);
    } else {
      source_value = builder.CreateZExtOrTrunc(source_value, storage_type);
    }
  }

  // Store to the place (with notify if design)
  if (IsDesignPlace(context, assign.target)) {
    StoreDesignWithNotify(
        context, source_value, target_ptr, storage_type, assign.target);
  } else {
    builder.CreateStore(source_value, target_ptr);
  }
}

void LowerGuardedAssign(Context& context, const mir::GuardedAssign& guarded) {
  auto& builder = context.GetBuilder();

  // Source is always evaluated unconditionally (per spec)
  llvm::Value* source_raw = LowerOperandRaw(context, guarded.source);

  // Lower validity predicate to i1
  llvm::Value* valid = LowerOperand(context, guarded.validity);
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
    StoreBitRange(context, guarded.target, source_raw);
  } else {
    // Direct store (same as LowerAssign but with pre-computed source_raw)
    llvm::Value* target_ptr = context.GetPlacePointer(guarded.target);
    llvm::Type* storage_type = context.GetPlaceLlvmType(guarded.target);

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

void LowerNonBlockingAssign(
    Context& context, const mir::NonBlockingAssign& nba) {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  const auto& place = arena[nba.target];

  uint32_t signal_id = GetSignalId(context, nba.target);

  // Case 1: BitRangeProjection — shifted value and mask
  if (context.HasBitRangeProjection(nba.target)) {
    auto [offset, width] = context.ComposeBitRange(nba.target);
    llvm::Value* ptr = context.GetPlacePointer(nba.target);
    llvm::Type* base_type = context.GetPlaceBaseType(nba.target);
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
      llvm::Value* source_raw = LowerOperandRaw(context, nba.source);
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
      llvm::Value* src = LowerOperand(context, nba.source);
      src = builder.CreateZExtOrTrunc(src, base_type, "nba.src.ext");
      auto* val_shifted = builder.CreateShl(src, shift_amt, "nba.val.shl");

      EmitScheduleNbaCall(
          context, ptr, notify_base_ptr, val_shifted, mask_shifted, base_type,
          signal_id);
    }
    return;
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
    llvm::Value* index = LowerOperand(context, idx_proj->index);
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
    llvm::Value* write_ptr = context.GetPlacePointer(nba.target);
    llvm::Value* notify_base_ptr = GetDesignRootPointer(context, nba.target);
    llvm::Type* storage_type = context.GetPlaceLlvmType(nba.target);

    // Evaluate source and coerce to storage type
    llvm::Value* source_value = nullptr;
    llvm::Value* mask_value = nullptr;
    if (storage_type->isStructTy()) {
      // 4-state element
      auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
      auto* elem_type = struct_type->getElementType(0);
      llvm::Value* raw = LowerOperandRaw(context, nba.source);
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
      source_value = LowerOperand(context, nba.source);
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
    return;
  }

  // Case 3: Simple full-width write (no projections)
  llvm::Value* write_ptr = context.GetPlacePointer(nba.target);
  llvm::Value* notify_base_ptr = write_ptr;
  llvm::Type* storage_type = context.GetPlaceLlvmType(nba.target);

  llvm::Value* source_value = nullptr;
  llvm::Value* mask_value = nullptr;
  if (storage_type->isStructTy()) {
    // 4-state target
    auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
    auto* elem_type = struct_type->getElementType(0);
    llvm::Value* raw = LowerOperandRaw(context, nba.source);
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
    source_value = LowerOperand(context, nba.source);
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
}

void LowerEffectOp(Context& context, const mir::EffectOp& effect_op) {
  std::visit(
      Overloaded{
          [&context](const mir::DisplayEffect& display) {
            LowerDisplayEffect(context, display);
          },
          [](const mir::SeverityEffect& /*severity*/) {
            // TODO(hankhsu): Handle severity effects
          },
          [](const mir::MemIOEffect& /*mem_io*/) {
            // TODO(hankhsu): Handle mem IO effects
          },
          [](const mir::FcloseEffect& /*fclose*/) {
            // TODO(hankhsu): Handle fclose effects
          },
      },
      effect_op);
}

}  // namespace

void LowerInstruction(Context& context, const mir::Instruction& instruction) {
  // Set origin for error reporting
  context.SetCurrentOrigin(instruction.origin);

  // RAII guard for statement-scoped cleanup of owned string temps
  StatementScope scope(context);

  std::visit(
      Overloaded{
          [&context](const mir::Assign& assign) {
            LowerAssign(context, assign);
          },
          [&context](const mir::Compute& compute) {
            LowerCompute(context, compute);
          },
          [&context](const mir::GuardedAssign& guarded) {
            LowerGuardedAssign(context, guarded);
          },
          [&context](const mir::Effect& effect) {
            LowerEffectOp(context, effect.op);
          },
          [&context](const mir::NonBlockingAssign& nba) {
            LowerNonBlockingAssign(context, nba);
          },
      },
      instruction.data);
}

}  // namespace lyra::lowering::mir_to_llvm
