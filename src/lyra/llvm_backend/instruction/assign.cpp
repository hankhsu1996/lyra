#include "lyra/llvm_backend/instruction/assign.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <variant>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/type_ops/dispatch.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Read-modify-write for BitRangeProjection assignment.
// Clears the target bit range in the base value, then OR's in the new value.
auto StoreBitRange(
    Context& context, mir::PlaceId target, llvm::Value* source_raw)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto br_result = context.ComposeBitRange(target);
  if (!br_result) return std::unexpected(br_result.error());
  auto [offset, width] = *br_result;

  // Get pointer for RMW load
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
    CommitPackedValueRaw(context, target, packed);
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
  CommitPackedValueRaw(context, target, result);
  return {};
}

// Get the root design slot pointer (before any projections) after alias
// resolution. Uses GetSignalIdForNba from commit module.
auto GetDesignRootPointer(Context& context, mir::PlaceId place_id)
    -> llvm::Value* {
  uint32_t signal_id = GetSignalIdForNba(context, place_id);
  auto slot_id = mir::SlotId{signal_id};
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

// Forward declaration for mutual recursion
auto StoreManagedStructLiteralToPtr(
    Context& context, llvm::Value* target_ptr, TypeId struct_type_id,
    const std::vector<mir::Operand>& operands) -> Result<void>;

// Store a single field from an operand, handling managed types recursively.
auto StoreFieldFromOperand(
    Context& context, llvm::Value* field_ptr, TypeId field_type_id,
    const mir::Operand& operand) -> Result<void> {
  const auto& types = context.GetTypeArena();
  const Type& field_type = types[field_type_id];

  if (field_type.Kind() == TypeKind::kString) {
    // String field: compute operand -> store with lifecycle (no notify)
    auto val_or_err = LowerOperand(context, operand);
    if (!val_or_err) return std::unexpected(val_or_err.error());

    // For constants, the operand produces a freshly-owned handle (from
    // LyraStringFromLiteral). For places, we need to determine ownership.
    OwnershipPolicy policy = std::holds_alternative<Constant>(operand.payload)
                                 ? OwnershipPolicy::kMove
                                 : OwnershipPolicy::kClone;
    detail::CommitStringField(context, field_ptr, *val_or_err, policy);
    return {};
  }

  if (field_type.Kind() == TypeKind::kUnpackedStruct &&
      NeedsFieldByField(field_type_id, types)) {
    // Nested managed struct: currently not supported
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(),
        "nested struct with managed fields in aggregate literal",
        UnsupportedCategory::kType));
  }

  // Plain field: compute operand -> store (no notify)
  auto val_or_err = LowerOperand(context, operand);
  if (!val_or_err) return std::unexpected(val_or_err.error());
  detail::CommitPlainField(context, field_ptr, *val_or_err);
  return {};
}

// Store struct literal fields to target pointer.
auto StoreManagedStructLiteralToPtr(
    Context& context, llvm::Value* target_ptr, TypeId struct_type_id,
    const std::vector<mir::Operand>& operands) -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = context.GetTypeArena();
  const Type& struct_type = types[struct_type_id];
  const auto& struct_info = struct_type.AsUnpackedStruct();

  auto llvm_struct_type_result =
      BuildLlvmTypeForTypeId(context, struct_type_id);
  if (!llvm_struct_type_result)
    return std::unexpected(llvm_struct_type_result.error());
  auto* llvm_struct = llvm::cast<llvm::StructType>(*llvm_struct_type_result);

  for (size_t i = 0; i < struct_info.fields.size(); ++i) {
    const auto& field = struct_info.fields[i];
    const auto& operand = operands[i];
    auto field_idx = static_cast<unsigned>(i);

    llvm::Value* field_ptr =
        builder.CreateStructGEP(llvm_struct, target_ptr, field_idx);

    auto result =
        StoreFieldFromOperand(context, field_ptr, field.type, operand);
    if (!result) return result;
  }

  return {};
}

// Lower a managed struct literal aggregate assignment.
auto LowerManagedStructLiteral(
    Context& context, mir::PlaceId target, const mir::Rvalue& rvalue,
    TypeId struct_type_id) -> Result<void> {
  auto target_ptr_result = context.GetPlacePointer(target);
  if (!target_ptr_result) return std::unexpected(target_ptr_result.error());
  llvm::Value* target_ptr = *target_ptr_result;

  auto result = StoreManagedStructLiteralToPtr(
      context, target_ptr, struct_type_id, rvalue.operands);
  if (!result) return result;

  CommitNotifyAggregateIfDesignSlot(context, target);
  return {};
}

// Lower an Rvalue source assignment.
auto LowerRvalueAssign(
    Context& context, mir::PlaceId target, const mir::Rvalue& rvalue)
    -> Result<void> {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  TypeId result_type = mir::TypeOfPlace(types, arena[target]);
  const Type& type = types[result_type];

  // Check for managed aggregate literal: handle field-by-field
  if (std::holds_alternative<mir::AggregateRvalueInfo>(rvalue.info) &&
      NeedsFieldByField(result_type, types)) {
    if (type.Kind() == TypeKind::kUnpackedStruct) {
      return LowerManagedStructLiteral(context, target, rvalue, result_type);
    }
    return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
        context.GetCurrentOrigin(), "managed array literal not yet supported",
        UnsupportedCategory::kType));
  }

  // Standard path: evaluate rvalue, store via commit
  auto rv_result = LowerRvalue(context, rvalue, result_type);
  if (!rv_result) return std::unexpected(rv_result.error());
  llvm::Value* value = rv_result->value;
  llvm::Value* unknown = rv_result->unknown;

  // For packed 4-state, pack value + unknown into struct before storing
  if (IsPacked(type) && IsPackedFourState(type, types)) {
    auto storage_type_or_err = context.GetPlaceLlvmType(target);
    if (!storage_type_or_err)
      return std::unexpected(storage_type_or_err.error());
    auto* struct_type = llvm::cast<llvm::StructType>(*storage_type_or_err);
    auto* elem_type = struct_type->getElementType(0);
    // Coerce value and unknown to struct element type
    auto& builder = context.GetBuilder();
    value = builder.CreateZExtOrTrunc(value, elem_type, "rv.val.fit");
    if (unknown == nullptr) {
      unknown = llvm::ConstantInt::get(elem_type, 0);
    } else {
      unknown = builder.CreateZExtOrTrunc(unknown, elem_type, "rv.unk.fit");
    }
    value = PackFourState(builder, struct_type, value, unknown);
  }

  // For packed types, use raw store (no ownership semantics)
  if (IsPacked(type)) {
    CommitPackedValueRaw(context, target, value);
    return {};
  }

  // For POD unpacked aggregates (no managed fields), use simple store.
  if ((type.Kind() == TypeKind::kUnpackedStruct ||
       type.Kind() == TypeKind::kUnpackedArray) &&
      !NeedsFieldByField(result_type, types)) {
    auto target_ptr_result = context.GetPlacePointer(target);
    if (!target_ptr_result) return std::unexpected(target_ptr_result.error());
    context.GetBuilder().CreateStore(value, *target_ptr_result);
    CommitNotifyAggregateIfDesignSlot(context, target);
    return {};
  }

  // For managed types, use CommitValue which handles Destroy internally
  return CommitValue(
      context, target, value, result_type, OwnershipPolicy::kMove);
}

// Evaluate RightHandSide to a raw LLVM value (for bit-range RMW).
auto LowerRhsRaw(
    Context& context, const mir::RightHandSide& rhs, mir::PlaceId target)
    -> Result<llvm::Value*> {
  return std::visit(
      common::Overloaded{
          [&](const mir::Operand& operand) -> Result<llvm::Value*> {
            return LowerOperandRaw(context, operand);
          },
          [&](const mir::Rvalue& rvalue) -> Result<llvm::Value*> {
            const auto& arena = context.GetMirArena();
            const auto& types = context.GetTypeArena();
            TypeId result_type = mir::TypeOfPlace(types, arena[target]);
            auto rv_result = LowerRvalue(context, rvalue, result_type);
            if (!rv_result) return std::unexpected(rv_result.error());
            // For 4-state, pack into struct
            if (IsPacked(types[result_type]) &&
                IsPackedFourState(types[result_type], types)) {
              auto storage_type_or_err = context.GetPlaceLlvmType(target);
              if (!storage_type_or_err)
                return std::unexpected(storage_type_or_err.error());
              auto* struct_type =
                  llvm::cast<llvm::StructType>(*storage_type_or_err);
              auto* elem_type = struct_type->getElementType(0);
              auto& builder = context.GetBuilder();
              llvm::Value* value =
                  builder.CreateZExtOrTrunc(rv_result->value, elem_type);
              llvm::Value* unknown = rv_result->unknown;
              if (unknown == nullptr) {
                unknown = llvm::ConstantInt::get(elem_type, 0);
              } else {
                unknown = builder.CreateZExtOrTrunc(unknown, elem_type);
              }
              return PackFourState(builder, struct_type, value, unknown);
            }
            return rv_result->value;
          },
      },
      rhs);
}

}  // namespace

auto LowerAssign(Context& context, const mir::Assign& assign) -> Result<void> {
  // BitRangeProjection targets use read-modify-write
  if (context.HasBitRangeProjection(assign.dest)) {
    auto source_raw_or_err = LowerRhsRaw(context, assign.rhs, assign.dest);
    if (!source_raw_or_err) return std::unexpected(source_raw_or_err.error());
    return StoreBitRange(context, assign.dest, *source_raw_or_err);
  }

  // Dispatch on RightHandSide: Operand or Rvalue
  return std::visit(
      common::Overloaded{
          [&](const mir::Operand& operand) -> Result<void> {
            // Delegate all value semantics to the TypeOps layer
            return AssignPlace(context, assign.dest, operand);
          },
          [&](const mir::Rvalue& rvalue) -> Result<void> {
            return LowerRvalueAssign(context, assign.dest, rvalue);
          },
      },
      assign.rhs);
}

auto LowerGuardedAssign(Context& context, const mir::GuardedAssign& guarded)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();

  // rhs evaluated BEFORE branch (per SystemVerilog spec)
  auto rhs_raw_or_err = LowerRhsRaw(context, guarded.rhs, guarded.dest);
  if (!rhs_raw_or_err) return std::unexpected(rhs_raw_or_err.error());
  llvm::Value* rhs_raw = *rhs_raw_or_err;

  // Guard check (coerce to i1)
  auto guard_or_err = LowerOperand(context, guarded.guard);
  if (!guard_or_err) return std::unexpected(guard_or_err.error());
  llvm::Value* guard = *guard_or_err;
  if (guard->getType()->getIntegerBitWidth() > 1) {
    auto* zero = llvm::ConstantInt::get(guard->getType(), 0);
    guard = builder.CreateICmpNE(guard, zero, "ga.tobool");
  }

  // Branch
  auto* func = builder.GetInsertBlock()->getParent();
  auto* do_write_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "ga.write", func);
  auto* skip_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "ga.skip", func);
  builder.CreateCondBr(guard, do_write_bb, skip_bb);

  // Write path
  builder.SetInsertPoint(do_write_bb);

  // BitRangeProjection: intentional bypass
  if (context.HasBitRangeProjection(guarded.dest)) {
    auto result = StoreBitRange(context, guarded.dest, rhs_raw);
    if (!result) return result;
  } else {
    const auto& place = arena[guarded.dest];
    TypeId type_id = mir::TypeOfPlace(types, place);

    // INVARIANT: GuardedAssign always uses kClone, never kMove.
    // Reason: We must not mutate/clear the rhs *place* when the assign is
    // conditional. The rhs was evaluated BEFORE the guard check (per SV
    // spec), and the rhs place may still be observable by other code.
    // kClone ensures no "consume source" side effects (no null-out, no
    // release). Do NOT "optimize" this to kMove.
    auto result = CommitValue(
        context, guarded.dest, rhs_raw, type_id, OwnershipPolicy::kClone);
    if (!result) return result;
  }
  builder.CreateBr(skip_bb);

  builder.SetInsertPoint(skip_bb);
  return {};
}

auto LowerDeferredAssign(Context& context, const mir::DeferredAssign& deferred)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  const auto& place = arena[deferred.dest];

  // Use canonical signal_id (after alias resolution) for notification
  // NBA is only valid for design places (GetSignalIdForNba throws if not)
  uint32_t signal_id = GetSignalIdForNba(context, deferred.dest);

  // Case 1: BitRangeProjection — shifted value and mask
  if (context.HasBitRangeProjection(deferred.dest)) {
    auto br_result = context.ComposeBitRange(deferred.dest);
    if (!br_result) return std::unexpected(br_result.error());
    auto [offset, width] = *br_result;
    auto ptr_or_err = context.GetPlacePointer(deferred.dest);
    if (!ptr_or_err) return std::unexpected(ptr_or_err.error());
    llvm::Value* ptr = *ptr_or_err;
    auto base_type_result = context.GetPlaceBaseType(deferred.dest);
    if (!base_type_result) return std::unexpected(base_type_result.error());
    llvm::Type* base_type = *base_type_result;
    llvm::Value* notify_base_ptr = ptr;  // BitRange shares root pointer

    // If there's an IndexProjection before the BitRange, notify_base is root
    for (const auto& proj : place.projections) {
      if (std::holds_alternative<mir::IndexProjection>(proj.info)) {
        notify_base_ptr = GetDesignRootPointer(context, deferred.dest);
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

      // Evaluate rhs
      auto rhs_raw_or_err = LowerRhsRaw(context, deferred.rhs, deferred.dest);
      if (!rhs_raw_or_err) return std::unexpected(rhs_raw_or_err.error());
      llvm::Value* source_raw = *rhs_raw_or_err;
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

      // Evaluate rhs, coerce to base width, shift into position
      auto rhs_or_err = LowerRhsRaw(context, deferred.rhs, deferred.dest);
      if (!rhs_or_err) return std::unexpected(rhs_or_err.error());
      llvm::Value* src = *rhs_or_err;
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
    auto write_ptr_or_err = context.GetPlacePointer(deferred.dest);
    if (!write_ptr_or_err) return std::unexpected(write_ptr_or_err.error());
    llvm::Value* write_ptr = *write_ptr_or_err;
    llvm::Value* notify_base_ptr = GetDesignRootPointer(context, deferred.dest);
    auto storage_type_or_err = context.GetPlaceLlvmType(deferred.dest);
    if (!storage_type_or_err)
      return std::unexpected(storage_type_or_err.error());
    llvm::Type* storage_type = *storage_type_or_err;

    // Evaluate rhs and coerce to storage type
    llvm::Value* source_value = nullptr;
    llvm::Value* mask_value = nullptr;
    if (storage_type->isStructTy()) {
      // 4-state element
      auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
      auto* elem_type = struct_type->getElementType(0);
      auto raw_or_err = LowerRhsRaw(context, deferred.rhs, deferred.dest);
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
      auto rhs_or_err = LowerRhsRaw(context, deferred.rhs, deferred.dest);
      if (!rhs_or_err) return std::unexpected(rhs_or_err.error());
      source_value = *rhs_or_err;
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
  auto write_ptr_or_err = context.GetPlacePointer(deferred.dest);
  if (!write_ptr_or_err) return std::unexpected(write_ptr_or_err.error());
  llvm::Value* write_ptr = *write_ptr_or_err;
  llvm::Value* notify_base_ptr = write_ptr;
  auto storage_type_or_err = context.GetPlaceLlvmType(deferred.dest);
  if (!storage_type_or_err) return std::unexpected(storage_type_or_err.error());
  llvm::Type* storage_type = *storage_type_or_err;

  llvm::Value* source_value = nullptr;
  llvm::Value* mask_value = nullptr;
  if (storage_type->isStructTy()) {
    // 4-state target
    auto* struct_type = llvm::cast<llvm::StructType>(storage_type);
    auto* elem_type = struct_type->getElementType(0);
    auto raw_or_err = LowerRhsRaw(context, deferred.rhs, deferred.dest);
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
    auto rhs_or_err = LowerRhsRaw(context, deferred.rhs, deferred.dest);
    if (!rhs_or_err) return std::unexpected(rhs_or_err.error());
    source_value = *rhs_or_err;
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

}  // namespace lyra::lowering::mir_to_llvm
