#include "lyra/llvm_backend/instruction/immediate_assign.hpp"

#include <cstddef>
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
#include "lyra/llvm_backend/slot_access.hpp"
#include "lyra/llvm_backend/type_ops/dispatch.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"
#include "lyra/llvm_backend/write_plan.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place_type.hpp"
#include "lyra/mir/rvalue.hpp"
#include "lyra/mir/statement.hpp"

namespace lyra::lowering::mir_to_llvm {

using detail::LowerRhsRaw;

namespace {

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
// Rvalue adapter: determines result type, handles rvalue-specific
// preprocessing (aggregate literals, 4-state packing), then routes
// through DispatchWrite. Does not own semantic write-shape decisions.
auto LowerRvalueAssign(
    Context& context, mir::PlaceId target, const mir::Rvalue& rvalue)
    -> Result<void> {
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  TypeId result_type = mir::TypeOfPlace(types, arena[target]);
  const Type& type = types[result_type];

  // Aggregate literal special case: some write shapes cannot be represented
  // as a single raw value. Route explicitly by plan before falling through
  // to the generic raw-value path.
  if (std::holds_alternative<mir::AggregateRvalueInfo>(rvalue.info)) {
    auto plan = BuildWritePlan(result_type, types);

    if (plan.op == WriteOp::kCommitFieldByFieldStruct) {
      return LowerManagedStructLiteral(context, target, rvalue, result_type);
    }
    if (plan.op == WriteOp::kCommitFieldByFieldArray) {
      return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
          context.GetCurrentOrigin(), "managed array literal not yet supported",
          UnsupportedCategory::kType));
    }
    if (plan.op == WriteOp::kRejectUnsupported) {
      return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
          context.GetCurrentOrigin(),
          "unpacked aggregate with container fields "
          "(dynamic array/queue) not yet supported",
          UnsupportedCategory::kFeature));
    }
    if (plan.op == WriteOp::kCommitUnionMemcpy) {
      return std::unexpected(context.GetDiagnosticContext().MakeUnsupported(
          context.GetCurrentOrigin(),
          "union aggregate rvalue assignment not yet supported",
          UnsupportedCategory::kFeature));
    }
    // Other plans (kStorePlainAggregate, kCommitPackedOrFloatScalar,
    // kCommitManagedScalar) can proceed to the generic raw-value path.
  }

  // Standard path: evaluate rvalue to raw LLVM value(s)
  auto rv_result = LowerRvalue(context, rvalue, result_type);
  if (!rv_result) return std::unexpected(rv_result.error());
  llvm::Value* value = rv_result->value;
  llvm::Value* unknown = rv_result->unknown;

  // Rvalue-specific 4-state packing: LowerRvalue produces value+unknown
  // separately; pack into {val, unk} struct for the commit path.
  if (IsPacked(type) && context.IsPackedFourState(type)) {
    auto storage_type_or_err = context.GetPlaceLlvmType(target);
    if (!storage_type_or_err)
      return std::unexpected(storage_type_or_err.error());
    auto* struct_type = llvm::cast<llvm::StructType>(*storage_type_or_err);
    auto* elem_type = struct_type->getElementType(0);
    auto& builder = context.GetBuilder();
    value = builder.CreateZExtOrTrunc(value, elem_type, "rv.val.fit");
    if (unknown == nullptr) {
      unknown = llvm::ConstantInt::get(elem_type, 0);
    } else {
      unknown = builder.CreateZExtOrTrunc(unknown, elem_type, "rv.unk.fit");
    }
    value = PackFourState(builder, struct_type, value, unknown);
  }

  // Dispatch through shared write orchestrator
  return DispatchWrite(
      context, target, RawValueSource{value}, result_type,
      OwnershipPolicy::kMove);
}

}  // namespace

auto LowerAssign(Context& context, const mir::Assign& assign) -> Result<void> {
  CanonicalSlotAccess canonical(context);
  return LowerAssign(context, canonical, assign);
}

auto LowerGuardedAssign(Context& context, const mir::GuardedAssign& guarded)
    -> Result<void> {
  CanonicalSlotAccess canonical(context);
  return LowerGuardedAssign(context, canonical, guarded);
}
// Single resolver-aware write path for managed immediate assignments.
//
// Commits a raw value to a managed slot through the resolver. This is
// the only write path for managed destinations. It handles:
//
//   1. Projected writes (bit-range) -> canonical StoreBitRange
//      (projected writes are canonical-only in v1 and never managed)
//   2. Managed whole-slot module slots -> resolver.CommitSlotValue
//
// Precondition: resolver.ManagesPlace(dest) is true, or dest has a
// bit-range projection (which is canonical-only).
//
// Both LowerAssign and LowerGuardedAssign use this for managed
// destinations. Non-managed destinations use the canonical assignment
// path (AssignPlace / LowerRvalueAssign / CommitValue) which handles
// full type-dispatch semantics (field-by-field structs, union memcpy,
// string lifecycle, etc.).
auto CommitManagedImmediate(
    Context& context, SlotAccessResolver& resolver, mir::PlaceId dest,
    llvm::Value* value, OwnershipPolicy policy) -> Result<void> {
  if (context.HasBitRangeProjection(dest)) {
    return StoreBitRange(context, dest, value);
  }
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  TypeId type_id = mir::TypeOfPlace(types, arena[dest]);
  return resolver.CommitSlotValue(dest, value, type_id, policy);
}

auto LowerAssign(
    Context& context, SlotAccessResolver& resolver, const mir::Assign& assign)
    -> Result<void> {
  // Managed destination: evaluate RHS via resolver-aware path, commit
  // through the single managed write path.
  if (resolver.ManagesPlace(assign.dest)) {
    auto rhs_raw_or_err =
        LowerRhsRaw(context, resolver, assign.rhs, assign.dest);
    if (!rhs_raw_or_err) return std::unexpected(rhs_raw_or_err.error());
    return CommitManagedImmediate(
        context, resolver, assign.dest, *rhs_raw_or_err,
        OwnershipPolicy::kMove);
  }

  // Non-managed destination: evaluate RHS with resolver-aware reads,
  // then commit through canonical write path.
  //
  // Projected writes (bit-range) are canonical-only in v1.
  if (context.HasBitRangeProjection(assign.dest)) {
    auto source_raw_or_err =
        LowerRhsRaw(context, resolver, assign.rhs, assign.dest);
    if (!source_raw_or_err) return std::unexpected(source_raw_or_err.error());
    return StoreBitRange(context, assign.dest, *source_raw_or_err);
  }

  // Check write plan: field-by-field struct/array and union memcpy
  // require OperandSource (they need the source PlaceId for memcpy/
  // field transfer). For these types, use AssignPlace which preserves
  // operand identity. AssignPlace reads operands via canonical
  // LowerOperandRaw, which is correct because these complex types
  // are never v1-eligible managed slots (only plain scalars are managed).
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  TypeId type_id = mir::TypeOfPlace(types, arena[assign.dest]);
  auto plan = BuildWritePlan(type_id, types);
  if (plan.op == WriteOp::kCommitFieldByFieldStruct ||
      plan.op == WriteOp::kCommitFieldByFieldArray ||
      plan.op == WriteOp::kCommitUnionMemcpy) {
    return std::visit(
        common::Overloaded{
            [&](const mir::Operand& operand) -> Result<void> {
              return AssignPlace(context, assign.dest, operand);
            },
            [&](const mir::Rvalue& rvalue) -> Result<void> {
              return LowerRvalueAssign(context, assign.dest, rvalue);
            },
        },
        assign.rhs);
  }

  // All other types: evaluate RHS with resolver-aware operand reads,
  // commit as raw value through canonical write path.
  auto rhs_raw_or_err = LowerRhsRaw(context, resolver, assign.rhs, assign.dest);
  if (!rhs_raw_or_err) return std::unexpected(rhs_raw_or_err.error());
  // Determine ownership: Rvalue sources produce temporaries (kMove).
  // Operand sources from temps are kMove, all others are kClone.
  auto policy = std::visit(
      common::Overloaded{
          [&](const mir::Operand& op) -> OwnershipPolicy {
            return DetermineOwnership(context, op);
          },
          [](const mir::Rvalue&) -> OwnershipPolicy {
            return OwnershipPolicy::kMove;
          },
      },
      assign.rhs);
  return CommitValue(context, assign.dest, *rhs_raw_or_err, type_id, policy);
}

auto LowerGuardedAssign(
    Context& context, SlotAccessResolver& resolver,
    const mir::GuardedAssign& guarded) -> Result<void> {
  auto& builder = context.GetBuilder();

  // RHS evaluated BEFORE branch (per SystemVerilog spec).
  auto rhs_raw_or_err =
      LowerRhsRaw(context, resolver, guarded.rhs, guarded.dest);
  if (!rhs_raw_or_err) return std::unexpected(rhs_raw_or_err.error());
  llvm::Value* rhs_raw = *rhs_raw_or_err;

  // Guard check (coerce to i1).
  auto guard_or_err = LowerOperand(context, resolver, guarded.guard);
  if (!guard_or_err) return std::unexpected(guard_or_err.error());
  llvm::Value* guard = *guard_or_err;
  if (guard->getType()->getIntegerBitWidth() > 1) {
    auto* zero = llvm::ConstantInt::get(guard->getType(), 0);
    guard = builder.CreateICmpNE(guard, zero, "ga.tobool");
  }

  // Branch.
  auto* func = builder.GetInsertBlock()->getParent();
  auto* do_write_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "ga.write", func);
  auto* skip_bb =
      llvm::BasicBlock::Create(context.GetLlvmContext(), "ga.skip", func);
  builder.CreateCondBr(guard, do_write_bb, skip_bb);

  // Write path.
  builder.SetInsertPoint(do_write_bb);

  if (resolver.ManagesPlace(guarded.dest)) {
    // Managed destination: single managed write path.
    // INVARIANT: GuardedAssign always uses kClone, never kMove.
    auto result = CommitManagedImmediate(
        context, resolver, guarded.dest, rhs_raw, OwnershipPolicy::kClone);
    if (!result) return result;
  } else if (context.HasBitRangeProjection(guarded.dest)) {
    auto result = StoreBitRange(context, guarded.dest, rhs_raw);
    if (!result) return result;
  } else {
    // Non-managed: canonical CommitValue(raw).
    // This uses CommitValue with a pre-evaluated raw value, while
    // plain assign uses AssignPlace/LowerRvalueAssign with operand
    // identity. This asymmetry is intentional: guarded assignment
    // evaluates RHS before the guard branch (SV spec), so only a
    // raw value is available at the write point. Both paths produce
    // identical results for v1-eligible types (plain scalars).
    // INVARIANT: GuardedAssign always uses kClone (see above).
    const auto& arena = context.GetMirArena();
    const auto& types = context.GetTypeArena();
    TypeId type_id = mir::TypeOfPlace(types, arena[guarded.dest]);
    auto result = CommitValue(
        context, guarded.dest, rhs_raw, type_id, OwnershipPolicy::kClone);
    if (!result) return result;
  }
  builder.CreateBr(skip_bb);

  builder.SetInsertPoint(skip_bb);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
