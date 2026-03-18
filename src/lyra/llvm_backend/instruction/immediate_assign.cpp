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
#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/compute.hpp"
#include "lyra/llvm_backend/compute/four_state_ops.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/instruction/assign_core.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/packed_storage_view.hpp"
#include "lyra/llvm_backend/type_ops/dispatch.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

using detail::LowerRhsRaw;

namespace {

// Convert a legacy llvm::Value* (iN or {iN, iN}) to a PackedRValue.
// This is caller-side representation adapting, not storage logic.
auto ConvertRawToPackedRValue(
    Context& ctx, llvm::Value* raw, uint32_t semantic_bits, bool is_four_state)
    -> PackedRValue {
  auto& builder = ctx.GetBuilder();
  PackedRValue result;
  result.semantic_bits = semantic_bits;
  result.is_four_state = is_four_state;

  if (raw->getType()->isStructTy()) {
    result.val = builder.CreateExtractValue(raw, 0, "rhs.val");
    result.unk = builder.CreateExtractValue(raw, 1, "rhs.unk");
  } else {
    result.val = raw;
    if (is_four_state) {
      // 2-state source into 4-state target: unknown = 0
      result.unk = llvm::ConstantInt::get(raw->getType(), 0);
    }
  }
  return result;
}

// Build a PackedStorePolicy from context state and target place.
// Reads context execution contract and resolves signal ID.
auto BuildStorePolicy(Context& ctx, mir::PlaceId target) -> PackedStorePolicy {
  PackedStorePolicy policy;

  auto signal_id = GetDesignSignalId(ctx, target);
  if (!signal_id.has_value()) {
    // Non-design target (process-local): direct store, no notification.
    policy.store_mode = PackedStoreMode::kDirectInit;
    return policy;
  }

  policy.signal_id = *signal_id;
  policy.engine_ptr = ctx.GetEnginePointer();
  policy.first_dirty_seen = ctx.GetFirstDirtySeenPtr();
  policy.notification_deferred =
      ctx.GetNotificationPolicy() == NotificationPolicy::kDeferred;

  switch (ctx.GetDesignStoreMode()) {
    case DesignStoreMode::kDirectInit:
      policy.store_mode = PackedStoreMode::kDirectInit;
      break;
    case DesignStoreMode::kNotifySimulation:
      policy.store_mode = PackedStoreMode::kNotifySimulation;
      break;
    case DesignStoreMode::kNotifyCrossContext:
      policy.store_mode = PackedStoreMode::kNotifyCrossContext;
      break;
  }

  // Validate: notify modes require a valid engine_ptr LLVM value.
  if (policy.store_mode != PackedStoreMode::kDirectInit &&
      policy.engine_ptr == nullptr) {
    throw common::InternalError(
        "BuildStorePolicy", "notify store mode requires engine_ptr on Context");
  }

  return policy;
}

// Packed subview write via the packed storage view module.
auto StoreBitRange(Context& ctx, mir::PlaceId target, llvm::Value* source_raw)
    -> Result<void> {
  auto path = ExtractPackedAccessPath(ctx, target);
  if (!path) return std::unexpected(path.error());

  auto subview = ResolvePackedSubview(ctx, *path);
  if (!subview) return std::unexpected(subview.error());

  auto rvalue = ConvertRawToPackedRValue(
      ctx, source_raw, subview->semantic_bit_width,
      subview->storage.is_four_state);
  auto policy = BuildStorePolicy(ctx, target);

  return EmitStoreToPackedSubview(ctx, *subview, rvalue, policy);
}

// Forward declaration for mutual recursion
auto StoreManagedStructLiteralToPtr(
    Context& context, llvm::Value* target_ptr, TypeId struct_type_id,
    const std::vector<mir::Operand>& operands) -> Result<void>;

// Determine ownership policy for a field assignment from an operand.
// Constants produce freshly-owned values (kMove); place/temp references
// alias existing storage and need cloning (kClone).
auto OwnershipForFieldOperand(const mir::Operand& operand) -> OwnershipPolicy {
  return std::holds_alternative<Constant>(operand.payload)
             ? OwnershipPolicy::kMove
             : OwnershipPolicy::kClone;
}

// Materialize an operand as a pointer suitable for field-by-field transfer.
// Uses LowerOperandRaw (not LowerOperand) to preserve aggregate struct types
// that would otherwise be misinterpreted as 4-state {value, unknown} pairs.
// The returned pointer is a temporary alloca valid for the current function.
auto MaterializeOperandAsPtr(
    Context& context, const mir::Operand& operand, TypeId type_id)
    -> Result<llvm::Value*> {
  auto val_or_err = LowerOperandRaw(context, operand);
  if (!val_or_err) return std::unexpected(val_or_err.error());

  auto llvm_type_result = BuildLlvmTypeForTypeId(context, type_id);
  if (!llvm_type_result) return std::unexpected(llvm_type_result.error());

  auto& builder = context.GetBuilder();
  auto* alloca =
      builder.CreateAlloca(*llvm_type_result, nullptr, "operand.spill");
  builder.CreateStore(*val_or_err, alloca);
  return alloca;
}

// Store a single field from an operand, handling managed types recursively.
auto StoreFieldFromOperand(
    Context& context, llvm::Value* field_ptr, TypeId field_type_id,
    const mir::Operand& operand) -> Result<void> {
  const auto& types = context.GetTypeArena();
  const Type& field_type = types[field_type_id];

  if (field_type.Kind() == TypeKind::kString) {
    auto val_or_err = LowerOperand(context, operand);
    if (!val_or_err) return std::unexpected(val_or_err.error());
    detail::CommitStringField(
        context, field_ptr, *val_or_err, OwnershipForFieldOperand(operand));
    return {};
  }

  if (field_type.Kind() == TypeKind::kUnpackedStruct &&
      NeedsFieldByField(field_type_id, types)) {
    // Nested managed struct: materialize the operand to a pointer, then
    // delegate to the canonical lifecycle-aware struct transfer path.
    auto src_ptr = MaterializeOperandAsPtr(context, operand, field_type_id);
    if (!src_ptr) return std::unexpected(src_ptr.error());
    return detail::TransferManagedStructFields(
        context, *src_ptr, field_ptr, field_type_id,
        OwnershipForFieldOperand(operand));
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
  if (IsPacked(type) && context.IsPackedFourState(type)) {
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
    CommitPackedValueRaw(context, target, value, result_type);
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
