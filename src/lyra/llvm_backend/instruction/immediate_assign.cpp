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
#include "lyra/common/type_queries.hpp"
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
#include "lyra/llvm_backend/type_ops/managed.hpp"
#include "lyra/llvm_backend/write_plan.hpp"
#include "lyra/llvm_backend/write_route.hpp"
#include "lyra/mir/handle.hpp"
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
    Context& ctx, const CuFacts& facts, mir::PlaceId target,
    const PackedRValue& rvalue) -> Result<void> {
  auto path = ExtractPackedAccessPath(ctx, facts, target);
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
    Context& context, const CuFacts& facts, const mir::Operand& operand,
    TypeId type_id) -> Result<llvm::Value*> {
  auto val_or_err = LowerOperandRaw(context, facts, operand);
  if (!val_or_err) return std::unexpected(val_or_err.error());

  auto llvm_type_result = BuildLlvmTypeForTypeId(context, facts, type_id);
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
    auto val_or_err = LowerOperand(context, facts, operand);
    if (!val_or_err) return std::unexpected(val_or_err.error());
    detail::CommitStringField(
        context.GetBuilder(), context.GetLyraStringRetain(),
        context.GetLyraStringRelease(), field_ptr, *val_or_err,
        OwnershipForFieldOperand(operand));
    return {};
  }

  if (field_type.Kind() == TypeKind::kUnpackedStruct &&
      NeedsFieldByField(field_type_id, types)) {
    // Nested managed struct: materialize the operand to a pointer, then
    // delegate to the canonical lifecycle-aware struct transfer path.
    auto src_ptr =
        MaterializeOperandAsPtr(context, facts, operand, field_type_id);
    if (!src_ptr) return std::unexpected(src_ptr.error());
    return detail::TransferManagedStructFields(
        context, facts, *src_ptr, field_ptr, field_type_id,
        OwnershipForFieldOperand(operand));
  }

  // Plain field: compute operand -> store (no notify)
  auto val_or_err = LowerOperand(context, facts, operand);
  if (!val_or_err) return std::unexpected(val_or_err.error());
  detail::CommitPlainField(context.GetBuilder(), field_ptr, *val_or_err);
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
      BuildLlvmTypeForTypeId(context, facts, struct_type_id);
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
        context, facts, target,
        PackedRValueSource{.rvalue = packed, .type_id = result_type},
        result_type, OwnershipPolicy::kMove);
  }

  // Non-packed types: raw value transport.
  return DispatchWrite(
      context, facts, target, RawValueSource{rv_result->value}, result_type,
      OwnershipPolicy::kMove);
}

}  // namespace

auto LowerGuardedAssign(
    Context& context, const CuFacts& facts, const mir::GuardedAssign& guarded)
    -> Result<void> {
  CanonicalSlotAccess canonical(context, facts);
  return LowerGuardedAssign(context, facts, canonical, guarded);
}

// Commit a value through the resolver (shadow alloca protocol).
// For resolver-routed destinations only (non-projected whole-slot writes).
auto CommitResolverRoutedValue(
    SlotAccessResolver& resolver, mir::PlaceId dest, llvm::Value* value,
    TypeId dest_type, OwnershipPolicy policy) -> Result<void> {
  return resolver.CommitSlotValue(dest, value, dest_type, policy);
}

// Compute write route once. Does not inspect OwnershipPolicy.
auto RouteWriteTarget(
    Context& ctx, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::WriteTarget& dest) -> RoutedWriteTarget {
  TypeId dest_type = detail::ResolveDestType(ctx, facts, dest);
  const auto* dest_place = std::get_if<mir::PlaceId>(&dest);

  // Bit-range takes priority: always goes to StoreBitRange regardless of
  // resolver status.
  if (dest_place != nullptr && ctx.HasBitRangeProjection(*dest_place)) {
    return {
        .kind = CommitRouteKind::kBitRange,
        .dest_type = dest_type,
        .dest = dest};
  }
  // Resolver-routed: shadow alloca protocol for activation-local slots.
  if (dest_place != nullptr && resolver.RequiresResolverCommit(*dest_place)) {
    return {
        .kind = CommitRouteKind::kResolverRouted,
        .dest_type = dest_type,
        .dest = dest};
  }
  // Direct: local alloca, temp, canonical design slot, external ref.
  return {
      .kind = CommitRouteKind::kDirect, .dest_type = dest_type, .dest = dest};
}

// Execute the bit-range (packed sub-field RMW) write path.
// Shared by plain, lifecycle, and guarded writes.
static auto ExecuteBitRangeWrite(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const RoutedWriteTarget& route, const mir::RightHandSide& rhs)
    -> Result<void> {
  auto dest_place = mir::RequireLocalDest(route.dest, "ExecuteBitRangeWrite");
  auto path = ExtractPackedAccessPath(context, facts, dest_place);
  if (!path) return std::unexpected(path.error());
  auto subview = ResolvePackedSubview(context, *path);
  if (!subview) return std::unexpected(subview.error());
  auto packed = LowerRhsToPackedRValue(
      context, facts, resolver, rhs, subview->semantic_bit_width,
      subview->result_type);
  if (!packed) return std::unexpected(packed.error());
  return StoreBitRange(context, facts, dest_place, *packed);
}

// Execute the direct write path with lifecycle policy (CopyAssign /
// MoveAssign).
static auto ExecuteDirectLifecycleWrite(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const RoutedWriteTarget& route, const mir::RightHandSide& rhs,
    OwnershipPolicy policy) -> Result<void> {
  const auto& types = *facts.types;
  auto plan = BuildWritePlan(route.dest_type, types);

  // Field-by-field and union memcpy.
  if (plan.op == WriteOp::kCommitFieldByFieldStruct ||
      plan.op == WriteOp::kCommitFieldByFieldArray ||
      plan.op == WriteOp::kCommitUnionMemcpy ||
      plan.op == WriteOp::kStorePlainAggregate) {
    return std::visit(
        common::Overloaded{
            [&](const mir::Operand& operand) -> Result<void> {
              return DispatchWrite(
                  context, facts, route.dest, OperandSource{&operand},
                  route.dest_type, policy);
            },
            [&](const mir::Rvalue& rvalue) -> Result<void> {
              return LowerRvalueAssign(context, facts, route.dest, rvalue);
            },
        },
        rhs);
  }

  // Packed scalar: non-lossy PackedRValue transport.
  if (plan.op == WriteOp::kCommitPackedOrFloatScalar &&
      IsPacked(types[route.dest_type])) {
    uint32_t semantic_bits = PackedBitWidth(types[route.dest_type], types);
    auto packed = LowerRhsToPackedRValue(
        context, facts, resolver, rhs, semantic_bits, route.dest_type);
    if (!packed) return std::unexpected(packed.error());
    return DispatchWrite(
        context, facts, route.dest,
        PackedRValueSource{.rvalue = *packed, .type_id = route.dest_type},
        route.dest_type, policy);
  }

  // Default: raw value.
  auto rhs_raw_or_err =
      LowerRhsRaw(context, facts, resolver, rhs, route.dest_type);
  if (!rhs_raw_or_err) return std::unexpected(rhs_raw_or_err.error());
  return DispatchWrite(
      context, facts, route.dest, RawValueSource{*rhs_raw_or_err},
      route.dest_type, policy);
}

// Execute a direct plain write via DispatchPlainWrite (no OwnershipPolicy).
static auto ExecuteDirectPlainWrite(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const RoutedWriteTarget& route, const mir::RightHandSide& rhs)
    -> Result<void> {
  const auto& types = *facts.types;
  auto plan = BuildWritePlan(route.dest_type, types);

  // Aggregates and union memcpy.
  if (plan.op == WriteOp::kStorePlainAggregate ||
      plan.op == WriteOp::kCommitUnionMemcpy) {
    return std::visit(
        common::Overloaded{
            [&](const mir::Operand& operand) -> Result<void> {
              return DispatchPlainWrite(
                  context, facts, route.dest, OperandSource{&operand},
                  route.dest_type);
            },
            [&](const mir::Rvalue& rvalue) -> Result<void> {
              return LowerRvalueAssign(context, facts, route.dest, rvalue);
            },
        },
        rhs);
  }

  // Packed scalar: non-lossy PackedRValue transport.
  if (plan.op == WriteOp::kCommitPackedOrFloatScalar &&
      IsPacked(types[route.dest_type])) {
    uint32_t semantic_bits = PackedBitWidth(types[route.dest_type], types);
    auto packed = LowerRhsToPackedRValue(
        context, facts, resolver, rhs, semantic_bits, route.dest_type);
    if (!packed) return std::unexpected(packed.error());
    return DispatchPlainWrite(
        context, facts, route.dest,
        PackedRValueSource{.rvalue = *packed, .type_id = route.dest_type},
        route.dest_type);
  }

  // Default: raw value.
  auto rhs_raw_or_err =
      LowerRhsRaw(context, facts, resolver, rhs, route.dest_type);
  if (!rhs_raw_or_err) return std::unexpected(rhs_raw_or_err.error());
  return DispatchPlainWrite(
      context, facts, route.dest, RawValueSource{*rhs_raw_or_err},
      route.dest_type);
}

// Execute a plain write (no lifecycle, no OwnershipPolicy).
// Routes based on pre-computed route.
static auto ExecutePlainWrite(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const RoutedWriteTarget& route, const mir::RightHandSide& rhs)
    -> Result<void> {
  switch (route.kind) {
    case CommitRouteKind::kBitRange:
      return ExecuteBitRangeWrite(context, facts, resolver, route, rhs);
    case CommitRouteKind::kResolverRouted: {
      auto place = mir::RequireLocalDest(route.dest, "ExecutePlainWrite");
      auto rhs_raw = LowerRhsRaw(context, facts, resolver, rhs, place);
      if (!rhs_raw) return std::unexpected(rhs_raw.error());
      return resolver.CommitPlainSlotValue(place, *rhs_raw, route.dest_type);
    }
    case CommitRouteKind::kDirect:
      return ExecuteDirectPlainWrite(context, facts, resolver, route, rhs);
  }
  throw common::InternalError("ExecutePlainWrite", "unhandled CommitRouteKind");
}

// Execute a lifecycle write (copy or move). Routes based on pre-computed route.
static auto ExecuteLifecycleWrite(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const RoutedWriteTarget& route, const mir::RightHandSide& rhs,
    OwnershipPolicy policy) -> Result<void> {
  switch (route.kind) {
    case CommitRouteKind::kBitRange:
      return ExecuteBitRangeWrite(context, facts, resolver, route, rhs);
    case CommitRouteKind::kResolverRouted: {
      auto place = mir::RequireLocalDest(route.dest, "ExecuteLifecycleWrite");
      auto rhs_raw = LowerRhsRaw(context, facts, resolver, rhs, place);
      if (!rhs_raw) return std::unexpected(rhs_raw.error());
      return CommitResolverRoutedValue(
          resolver, place, *rhs_raw, route.dest_type, policy);
    }
    case CommitRouteKind::kDirect:
      return ExecuteDirectLifecycleWrite(
          context, facts, resolver, route, rhs, policy);
  }
  throw common::InternalError(
      "ExecuteLifecycleWrite", "unhandled CommitRouteKind");
}

// PlainAssign: non-managed overwrite. Asserts destination is non-managed,
// routes once, then executes plain write with no lifecycle.
auto LowerPlainAssign(
    Context& context, const CuFacts& facts, const mir::WriteTarget& dest,
    const mir::RightHandSide& rhs) -> Result<void> {
  CanonicalSlotAccess canonical(context, facts);
  return LowerPlainAssign(context, facts, canonical, dest, rhs);
}

auto LowerPlainAssign(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::WriteTarget& dest, const mir::RightHandSide& rhs)
    -> Result<void> {
  auto route = RouteWriteTarget(context, facts, resolver, dest);
  if (common::TypeContainsManaged(route.dest_type, *facts.types)) {
    throw common::InternalError(
        "LowerPlainAssign", "PlainAssign used for managed destination type");
  }
  return ExecutePlainWrite(context, facts, resolver, route, rhs);
}

// CopyAssign: explicit clone/copy lifecycle.
auto LowerCopyAssign(
    Context& context, const CuFacts& facts, const mir::WriteTarget& dest,
    const mir::RightHandSide& rhs) -> Result<void> {
  CanonicalSlotAccess canonical(context, facts);
  return LowerCopyAssign(context, facts, canonical, dest, rhs);
}

auto LowerCopyAssign(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::WriteTarget& dest, const mir::RightHandSide& rhs)
    -> Result<void> {
  auto route = RouteWriteTarget(context, facts, resolver, dest);
  return ExecuteLifecycleWrite(
      context, facts, resolver, route, rhs, OwnershipPolicy::kClone);
}

// MoveAssign: explicit move lifecycle.
auto LowerMoveAssign(
    Context& context, const CuFacts& facts, const mir::WriteTarget& dest,
    const mir::RightHandSide& rhs) -> Result<void> {
  CanonicalSlotAccess canonical(context, facts);
  return LowerMoveAssign(context, facts, canonical, dest, rhs);
}

auto LowerMoveAssign(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::WriteTarget& dest, const mir::RightHandSide& rhs)
    -> Result<void> {
  auto route = RouteWriteTarget(context, facts, resolver, dest);
  return ExecuteLifecycleWrite(
      context, facts, resolver, route, rhs, OwnershipPolicy::kMove);
}

// Execute a plain guarded write (no lifecycle, no OwnershipPolicy).
// For non-managed destination types only.
static auto ExecutePlainGuardedWrite(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const RoutedWriteTarget& route, llvm::Value* rhs_raw,
    const std::optional<PackedRValue>& packed_rhs) -> Result<void> {
  switch (route.kind) {
    case CommitRouteKind::kBitRange: {
      auto place =
          mir::RequireLocalDest(route.dest, "ExecutePlainGuardedWrite");
      return StoreBitRange(context, facts, place, *packed_rhs);
    }
    case CommitRouteKind::kResolverRouted: {
      auto place =
          mir::RequireLocalDest(route.dest, "ExecutePlainGuardedWrite");
      return resolver.CommitPlainSlotValue(place, rhs_raw, route.dest_type);
    }
    case CommitRouteKind::kDirect: {
      if (packed_rhs) {
        return DispatchPlainWrite(
            context, facts, route.dest,
            PackedRValueSource{
                .rvalue = *packed_rhs, .type_id = route.dest_type},
            route.dest_type);
      }
      return DispatchPlainWrite(
          context, facts, route.dest, RawValueSource{rhs_raw}, route.dest_type);
    }
  }
  throw common::InternalError(
      "ExecutePlainGuardedWrite", "unhandled CommitRouteKind");
}

// Execute a lifecycle guarded write (CopyAssign semantics with kClone).
// For managed destination types only.
static auto ExecuteLifecycleGuardedWrite(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const RoutedWriteTarget& route, llvm::Value* rhs_raw,
    const std::optional<PackedRValue>& packed_rhs) -> Result<void> {
  switch (route.kind) {
    case CommitRouteKind::kBitRange: {
      auto place =
          mir::RequireLocalDest(route.dest, "ExecuteLifecycleGuardedWrite");
      return StoreBitRange(context, facts, place, *packed_rhs);
    }
    case CommitRouteKind::kResolverRouted: {
      auto place =
          mir::RequireLocalDest(route.dest, "ExecuteLifecycleGuardedWrite");
      return CommitResolverRoutedValue(
          resolver, place, rhs_raw, route.dest_type, OwnershipPolicy::kClone);
    }
    case CommitRouteKind::kDirect: {
      if (packed_rhs) {
        return DispatchWrite(
            context, facts, route.dest,
            PackedRValueSource{
                .rvalue = *packed_rhs, .type_id = route.dest_type},
            route.dest_type, OwnershipPolicy::kClone);
      }
      return DispatchWrite(
          context, facts, route.dest, RawValueSource{rhs_raw}, route.dest_type,
          OwnershipPolicy::kClone);
    }
  }
  throw common::InternalError(
      "ExecuteLifecycleGuardedWrite", "unhandled CommitRouteKind");
}

auto LowerGuardedAssign(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::GuardedAssign& guarded) -> Result<void> {
  auto route = RouteWriteTarget(context, facts, resolver, guarded.dest);
  auto& builder = context.GetBuilder();
  const auto& types = *facts.types;

  // Evaluate RHS before guard check (must be unconditional).
  bool is_bit_range = route.kind == CommitRouteKind::kBitRange;
  bool is_packed_dest = is_bit_range || IsPacked(types[route.dest_type]);
  std::optional<PackedRValue> packed_rhs;
  llvm::Value* rhs_raw = nullptr;

  if (is_bit_range) {
    auto dest_place =
        mir::RequireLocalDest(route.dest, "LowerGuardedAssign/BitRange");
    auto path = ExtractPackedAccessPath(context, facts, dest_place);
    if (!path) return std::unexpected(path.error());
    auto subview = ResolvePackedSubview(context, *path);
    if (!subview) return std::unexpected(subview.error());
    auto packed = LowerRhsToPackedRValue(
        context, facts, resolver, guarded.rhs, subview->semantic_bit_width,
        subview->result_type);
    if (!packed) return std::unexpected(packed.error());
    packed_rhs = *packed;
  } else if (is_packed_dest) {
    uint32_t semantic_bits = PackedBitWidth(types[route.dest_type], types);
    auto packed = LowerRhsToPackedRValue(
        context, facts, resolver, guarded.rhs, semantic_bits, route.dest_type);
    if (!packed) return std::unexpected(packed.error());
    packed_rhs = *packed;
  } else {
    auto rhs_raw_or_err =
        LowerRhsRaw(context, facts, resolver, guarded.rhs, route.dest_type);
    if (!rhs_raw_or_err) return std::unexpected(rhs_raw_or_err.error());
    rhs_raw = *rhs_raw_or_err;
  }

  // Guard check (coerce to i1).
  auto guard_or_err = LowerOperand(context, facts, resolver, guarded.guard);
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

  // Write path: plain for non-managed, lifecycle for managed.
  builder.SetInsertPoint(do_write_bb);

  bool is_managed = common::TypeContainsManaged(route.dest_type, types);
  Result<void> write_result =
      is_managed ? ExecuteLifecycleGuardedWrite(
                       context, facts, resolver, route, rhs_raw, packed_rhs)
                 : ExecutePlainGuardedWrite(
                       context, facts, resolver, route, rhs_raw, packed_rhs);
  if (!write_result) return write_result;

  builder.CreateBr(skip_bb);

  builder.SetInsertPoint(skip_bb);
  return {};
}

}  // namespace lyra::lowering::mir_to_llvm
