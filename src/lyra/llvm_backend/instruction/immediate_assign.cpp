#include "lyra/llvm_backend/instruction/immediate_assign.hpp"

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
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction/assign_core.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/type_ops/dispatch.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

using detail::IsFourStateScalarStruct;
using detail::LowerRhsRaw;

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

  if (IsFourStateScalarStruct(base_type)) {
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

}  // namespace lyra::lowering::mir_to_llvm
