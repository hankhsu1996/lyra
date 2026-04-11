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
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
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
using detail::LowerRhsToPackedRValue;

namespace {

// Build a PackedStorePolicy from context state and target place.
// Reads context execution contract and resolves signal ID.
auto BuildStorePolicy(Context& ctx, mir::PlaceId target) -> PackedStorePolicy {
  PackedStorePolicy policy;

  auto signal_id = GetDesignSignalCoord(ctx, target);
  if (!signal_id.has_value()) {
    // Non-design target (process-local): direct store, no notification.
    policy.store_mode = PackedStoreMode::kDirectInit;
    return policy;
  }

  policy.signal_id = *signal_id;
  policy.engine_ptr = ctx.GetEnginePointer();
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
auto StoreBitRange(
    Context& ctx, mir::PlaceId target, const PackedRValue& rvalue)
    -> Result<void> {
  auto path = ExtractPackedAccessPath(ctx, target);
  if (!path) return std::unexpected(path.error());

  auto subview = ResolvePackedSubview(ctx, *path);
  if (!subview) return std::unexpected(subview.error());

  auto policy = BuildStorePolicy(ctx, target);

  return EmitStoreToPackedSubview(ctx, *subview, rvalue, policy);
}

// Forward declaration for mutual recursion
auto StoreManagedStructLiteralToPtr(
    Context& context, const CuFacts& facts, llvm::Value* target_ptr,
    TypeId struct_type_id, const std::vector<mir::Operand>& operands)
    -> Result<void>;

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
    Context& context, const CuFacts& facts, llvm::Value* field_ptr,
    TypeId field_type_id, const mir::Operand& operand) -> Result<void> {
  const auto& types = *facts.types;
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
    Context& context, const CuFacts& facts, llvm::Value* target_ptr,
    TypeId struct_type_id, const std::vector<mir::Operand>& operands)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& types = *facts.types;
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
        StoreFieldFromOperand(context, facts, field_ptr, field.type, operand);
    if (!result) return result;
  }

  return {};
}

// Lower a managed struct literal aggregate assignment.
auto LowerManagedStructLiteral(
    Context& context, const CuFacts& facts, const mir::WriteTarget& target,
    const mir::Rvalue& rvalue, TypeId struct_type_id) -> Result<void> {
  auto target_ptr = context.GetWriteDestPointer(target);
  if (!target_ptr) return std::unexpected(target_ptr.error());

  auto result = StoreManagedStructLiteralToPtr(
      context, facts, *target_ptr, struct_type_id, rvalue.operands);
  if (!result) return result;

  CommitNotifyAggregateIfDesignSlot(context, target);
  return {};
}

// Lower an Rvalue source assignment.
// Rvalue adapter: determines result type, handles rvalue-specific
// preprocessing (aggregate literals, 4-state packing), then routes
// through DispatchWrite. Does not own semantic write-shape decisions.
auto LowerRvalueAssign(
    Context& context, const CuFacts& facts, const mir::WriteTarget& target,
    const mir::Rvalue& rvalue) -> Result<void> {
  const auto& types = *facts.types;
  TypeId result_type = detail::ResolveDestType(context, facts, target);
  const Type& type = types[result_type];

  // Aggregate literal special case: some write shapes cannot be represented
  // as a single raw value. Route explicitly by plan before falling through
  // to the generic raw-value path.
  if (std::holds_alternative<mir::AggregateRvalueInfo>(rvalue.info)) {
    auto plan = BuildWritePlan(result_type, types);

    if (plan.op == WriteOp::kCommitFieldByFieldStruct) {
      return LowerManagedStructLiteral(
          context, facts, target, rvalue, result_type);
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
  auto rv_result = LowerRvalue(context, facts, rvalue, result_type);
  if (!rv_result) return std::unexpected(rv_result.error());

  // Packed types: non-lossy PackedRValue transport via PackedRValueSource.
  // Preserves unk == nullptr for provably 2-state RHS.
  if (IsPacked(type)) {
    uint32_t semantic_bits = PackedBitWidth(type, types);
    PackedRValue packed;
    packed.val = rv_result->value;
    packed.unk = rv_result->unknown;
    packed.semantic_bits = semantic_bits;
    return DispatchWrite(
        context, target,
        PackedRValueSource{.rvalue = packed, .type_id = result_type},
        result_type, OwnershipPolicy::kMove);
  }

  // Non-packed types: raw value transport.
  return DispatchWrite(
      context, target, RawValueSource{rv_result->value}, result_type,
      OwnershipPolicy::kMove);
}

}  // namespace

auto LowerAssign(
    Context& context, const CuFacts& facts, const mir::Assign& assign)
    -> Result<void> {
  CanonicalSlotAccess canonical(context);
  return LowerAssign(context, facts, canonical, assign);
}

auto LowerGuardedAssign(
    Context& context, const CuFacts& facts, const mir::GuardedAssign& guarded)
    -> Result<void> {
  CanonicalSlotAccess canonical(context);
  return LowerGuardedAssign(context, facts, canonical, guarded);
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
// Managed non-projected writes only. Bit-range projections are handled
// directly by callers using non-lossy PackedRValue transport.
auto CommitManagedImmediate(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    mir::PlaceId dest, llvm::Value* value, OwnershipPolicy policy)
    -> Result<void> {
  const auto& types = *facts.types;
  TypeId type_id = mir::TypeOfPlace(types, context.LookupPlace(dest));
  return resolver.CommitSlotValue(dest, value, type_id, policy);
}

auto LowerAssign(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::Assign& assign) -> Result<void> {
  const auto& dest = assign.dest;
  auto* dest_place = std::get_if<mir::PlaceId>(&dest);

  // PlaceId-only paths: managed slots and bit-range projections.
  // External refs are design-global with no projections, so these
  // branches are only reachable for PlaceId destinations.
  if (dest_place != nullptr && resolver.ManagesPlace(*dest_place)) {
    if (context.HasBitRangeProjection(*dest_place)) {
      auto path = ExtractPackedAccessPath(context, *dest_place);
      if (!path) return std::unexpected(path.error());
      auto subview = ResolvePackedSubview(context, *path);
      if (!subview) return std::unexpected(subview.error());
      auto packed = LowerRhsToPackedRValue(
          context, facts, resolver, assign.rhs, subview->semantic_bit_width,
          subview->result_type);
      if (!packed) return std::unexpected(packed.error());
      return StoreBitRange(context, *dest_place, *packed);
    }
    auto rhs_raw_or_err =
        LowerRhsRaw(context, facts, resolver, assign.rhs, *dest_place);
    if (!rhs_raw_or_err) return std::unexpected(rhs_raw_or_err.error());
    return CommitManagedImmediate(
        context, facts, resolver, *dest_place, *rhs_raw_or_err,
        OwnershipPolicy::kMove);
  }

  if (dest_place != nullptr && context.HasBitRangeProjection(*dest_place)) {
    auto path = ExtractPackedAccessPath(context, *dest_place);
    if (!path) return std::unexpected(path.error());
    auto subview = ResolvePackedSubview(context, *path);
    if (!subview) return std::unexpected(subview.error());
    auto packed = LowerRhsToPackedRValue(
        context, facts, resolver, assign.rhs, subview->semantic_bit_width,
        subview->result_type);
    if (!packed) return std::unexpected(packed.error());
    return StoreBitRange(context, *dest_place, *packed);
  }

  // Shared path: resolve destination type and build write plan.
  // Works for both PlaceId and ExternalRefId destinations.
  const auto& types = *facts.types;
  TypeId type_id = detail::ResolveDestType(context, facts, dest);
  auto plan = BuildWritePlan(type_id, types);

  // Field-by-field and union memcpy: route through DispatchWrite (operand
  // source) or LowerRvalueAssign (rvalue source). Both accept mir::WriteTarget.
  if (plan.op == WriteOp::kCommitFieldByFieldStruct ||
      plan.op == WriteOp::kCommitFieldByFieldArray ||
      plan.op == WriteOp::kCommitUnionMemcpy) {
    return std::visit(
        common::Overloaded{
            [&](const mir::Operand& operand) -> Result<void> {
              return DispatchWrite(
                  context, dest, OperandSource{&operand}, type_id,
                  DetermineOwnership(context, operand));
            },
            [&](const mir::Rvalue& rvalue) -> Result<void> {
              return LowerRvalueAssign(context, facts, dest, rvalue);
            },
        },
        assign.rhs);
  }

  // Packed scalar: non-lossy PackedRValue transport through DispatchWrite.
  if (plan.op == WriteOp::kCommitPackedOrFloatScalar &&
      IsPacked(types[type_id])) {
    uint32_t semantic_bits = PackedBitWidth(types[type_id], types);
    auto packed = LowerRhsToPackedRValue(
        context, facts, resolver, assign.rhs, semantic_bits, type_id);
    if (!packed) return std::unexpected(packed.error());
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
    return DispatchWrite(
        context, dest,
        PackedRValueSource{.rvalue = *packed, .type_id = type_id}, type_id,
        policy);
  }

  // Default: evaluate RHS as raw value, commit through canonical path.
  auto rhs_raw_or_err =
      LowerRhsRaw(context, facts, resolver, assign.rhs, type_id);
  if (!rhs_raw_or_err) return std::unexpected(rhs_raw_or_err.error());
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
  return DispatchWrite(
      context, dest, RawValueSource{*rhs_raw_or_err}, type_id, policy);
}

auto LowerGuardedAssign(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::GuardedAssign& guarded) -> Result<void> {
  const auto& dest = guarded.dest;
  auto* dest_place = std::get_if<mir::PlaceId>(&dest);
  auto& builder = context.GetBuilder();
  const auto& types = *facts.types;
  TypeId dest_type_id = detail::ResolveDestType(context, facts, dest);

  // Bit-range is PlaceId-only (external refs have no projections).
  bool is_bit_range =
      dest_place != nullptr && context.HasBitRangeProjection(*dest_place);
  bool is_packed_dest = is_bit_range || IsPacked(types[dest_type_id]);
  std::optional<PackedRValue> packed_rhs;
  llvm::Value* rhs_raw = nullptr;

  if (is_bit_range) {
    auto path = ExtractPackedAccessPath(context, *dest_place);
    if (!path) return std::unexpected(path.error());
    auto subview = ResolvePackedSubview(context, *path);
    if (!subview) return std::unexpected(subview.error());
    auto packed = LowerRhsToPackedRValue(
        context, facts, resolver, guarded.rhs, subview->semantic_bit_width,
        subview->result_type);
    if (!packed) return std::unexpected(packed.error());
    packed_rhs = *packed;
  } else if (is_packed_dest) {
    uint32_t semantic_bits = PackedBitWidth(types[dest_type_id], types);
    auto packed = LowerRhsToPackedRValue(
        context, facts, resolver, guarded.rhs, semantic_bits, dest_type_id);
    if (!packed) return std::unexpected(packed.error());
    packed_rhs = *packed;
  } else {
    auto rhs_raw_or_err =
        LowerRhsRaw(context, facts, resolver, guarded.rhs, dest_type_id);
    if (!rhs_raw_or_err) return std::unexpected(rhs_raw_or_err.error());
    rhs_raw = *rhs_raw_or_err;
  }

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

  if (is_bit_range) {
    auto result = StoreBitRange(context, *dest_place, *packed_rhs);
    if (!result) return result;
  } else if (is_packed_dest) {
    auto result = DispatchWrite(
        context, dest,
        PackedRValueSource{.rvalue = *packed_rhs, .type_id = dest_type_id},
        dest_type_id, OwnershipPolicy::kClone);
    if (!result) return result;
  } else if (dest_place != nullptr && resolver.ManagesPlace(*dest_place)) {
    auto result = CommitManagedImmediate(
        context, facts, resolver, *dest_place, rhs_raw,
        OwnershipPolicy::kClone);
    if (!result) return result;
  } else {
    auto result = DispatchWrite(
        context, dest, RawValueSource{rhs_raw}, dest_type_id,
        OwnershipPolicy::kClone);
    if (!result) return result;
  }
  builder.CreateBr(skip_bb);

  builder.SetInsertPoint(skip_bb);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
