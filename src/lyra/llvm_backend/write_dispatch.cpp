#include <variant>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>
#include <llvm/Support/Alignment.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/access.hpp"
#include "lyra/llvm_backend/commit/emit.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/packed_storage_view.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"
#include "lyra/llvm_backend/write_plan.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Resolve source to a raw LLVM value, lowering operand if needed.
auto ResolveRawValue(
    Context& ctx, const CuFacts& facts, const WriteSource& source)
    -> Result<llvm::Value*> {
  if (const auto* rv = std::get_if<RawValueSource>(&source)) {
    return rv->value;
  }
  return LowerOperandRaw(ctx, facts, *std::get<OperandSource>(source).operand);
}

// Extract PlaceId from operand source. InternalError if source is not an
// operand-backed place -- this is a genuine contract violation since only
// OperandSource callers can reach place-required write ops.
auto RequireOperandPlace(const WriteSource& source, const char* caller)
    -> mir::PlaceId {
  const auto* os = std::get_if<OperandSource>(&source);
  if (os == nullptr) {
    throw common::InternalError(
        caller, "write op requires operand source but got raw value");
  }
  const auto* pid = std::get_if<mir::PlaceId>(&os->operand->payload);
  if (pid == nullptr) {
    throw common::InternalError(
        caller, "write op requires place operand but got constant/literal");
  }
  return *pid;
}

// Emit helpers -- one per WriteOp. No re-classification inside.

auto EmitManagedScalarWrite(
    Context& ctx, const CuFacts& facts, const mir::WriteTarget& target,
    const WriteSource& source, TypeId type_id, OwnershipPolicy policy)
    -> Result<void> {
  auto raw = ResolveRawValue(ctx, facts, source);
  if (!raw) return std::unexpected(raw.error());

  // Move cleanup: null-out source handle if moving from temp
  if (const auto* os = std::get_if<OperandSource>(&source)) {
    if (const auto* pid = std::get_if<mir::PlaceId>(&os->operand->payload)) {
      CommitMoveCleanupIfTemp(ctx, facts, *pid, policy, type_id);
    }
  }

  // Resolve WriteTarget and dispatch to string/container commit
  auto wt = commit::Access::GetWriteTarget(ctx, target);
  if (!wt) return std::unexpected(wt.error());

  const auto& type = (*facts.types)[type_id];
  switch (GetManagedKind(type.Kind())) {
    case ManagedKind::kString:
      return CommitStringValue(ctx, facts, *wt, *raw, policy, type_id);
    case ManagedKind::kContainer:
      return CommitContainerValue(ctx, facts, *wt, *raw, policy, type_id);
    case ManagedKind::kNone:
      throw common::InternalError(
          "EmitManagedScalarWrite",
          "kManagedScalar shape but ManagedKind::kNone");
  }
  throw common::InternalError(
      "EmitManagedScalarWrite", "unreachable after ManagedKind switch");
}

auto EmitPointerScalarWrite(
    Context& ctx, const CuFacts& facts, const mir::WriteTarget& target,
    const WriteSource& source) -> Result<void> {
  auto raw = ResolveRawValue(ctx, facts, source);
  if (!raw) return std::unexpected(raw.error());

  auto wt = commit::Access::GetWriteTarget(ctx, target);
  if (!wt) return std::unexpected(wt.error());

  ctx.GetBuilder().CreateStore(*raw, wt->ptr);
  return {};
}

auto EmitPlainAggregateStore(
    Context& ctx, const CuFacts& facts, const mir::WriteTarget& target,
    const WriteSource& source) -> Result<void> {
  auto raw = ResolveRawValue(ctx, facts, source);
  if (!raw) return std::unexpected(raw.error());

  auto wt = commit::Access::GetWriteTarget(ctx, target);
  if (!wt) return std::unexpected(wt.error());

  ctx.GetBuilder().CreateStore(*raw, wt->ptr);

  // Notify if design slot.
  if (wt->canonical_signal_id.has_value() &&
      ctx.GetDesignStoreMode() != DesignStoreMode::kDirectInit) {
    auto& builder = ctx.GetBuilder();
    auto emit_notify = [&]() {
      if (wt->canonical_signal_id->IsExtRef()) {
        builder.CreateCall(
            ctx.GetLyraMarkDirtyExtRef(),
            {ctx.GetEnginePointer(), ctx.GetInstancePointer(),
             wt->canonical_signal_id->Emit(builder), builder.getInt32(0),
             builder.getInt32(0)});
      } else if (wt->canonical_signal_id->IsLocal()) {
        builder.CreateCall(
            ctx.GetLyraNotifySignalLocal(),
            {ctx.GetEnginePointer(),
             wt->canonical_signal_id->GetInstancePointer(
                 ctx.GetInstancePointer()),
             wt->ptr, wt->canonical_signal_id->Emit(builder)});
      } else {
        builder.CreateCall(
            ctx.GetLyraNotifySignalGlobal(),
            {ctx.GetEnginePointer(), wt->ptr,
             wt->canonical_signal_id->Emit(builder)});
      }
    };
    bool static_hit = wt->requires_static_dirty_propagation;
    if (static_hit) {
      emit_notify();
    } else if (wt->mutation_signal.has_value()) {
      ctx.EmitTraceBranch(
          *wt->mutation_signal, "notify.aggregate", "notify.skip", emit_notify,
          []() {});
    }
  }
  return {};
}

auto EmitPackedOrFloatWrite(
    Context& ctx, const CuFacts& facts, const mir::WriteTarget& target,
    const WriteSource& source, TypeId type_id) -> Result<void> {
  // Non-lossy packed path: PackedRValueSource carries preserved
  // 2-state/4-state semantics directly to CommitPackedValue.
  if (const auto* packed = std::get_if<PackedRValueSource>(&source)) {
    CommitPackedValue(ctx, facts, target, packed->rvalue, packed->type_id);
    return {};
  }

  // Legacy raw path: convert raw value to PackedRValue at the dispatch
  // boundary. This is the single conversion point for callers (system TF,
  // assoc_op, call, etc.) that produce raw llvm::Value* and route through
  // CommitValue -> DispatchWrite. The raw value's LLVM type is authoritative:
  // scalar means 2-state, struct {iN,iN} means 4-state.
  auto raw = ResolveRawValue(ctx, facts, source);
  if (!raw) return std::unexpected(raw.error());

  const auto& types = *facts.types;
  const Type& type = types[type_id];
  auto kind = type.Kind();
  if (kind == TypeKind::kEnum) {
    kind = types[type.AsEnum().base_type].Kind();
  }

  uint32_t semantic_bits = 0;
  llvm::Value* store_value = *raw;

  if (kind == TypeKind::kReal) {
    semantic_bits = 64;
    store_value = ctx.GetBuilder().CreateBitCast(
        *raw, llvm::Type::getInt64Ty(ctx.GetLlvmContext()), "real.as.i64");
  } else if (kind == TypeKind::kShortReal) {
    semantic_bits = 32;
    store_value = ctx.GetBuilder().CreateBitCast(
        *raw, llvm::Type::getInt32Ty(ctx.GetLlvmContext()), "shortreal.as.i32");
  } else {
    semantic_bits = PackedBitWidth(type, types);
  }

  auto rvalue = BuildPackedRValueFromRaw(ctx, store_value, semantic_bits);
  CommitPackedValue(ctx, facts, target, rvalue, type_id);
  return {};
}

auto EmitFieldByFieldStructWrite(
    Context& ctx, const CuFacts& facts, const mir::WriteTarget& target,
    mir::PlaceId source_place, TypeId type_id, OwnershipPolicy policy)
    -> Result<void> {
  return CommitStructFieldByField(
      ctx, facts, target, source_place, type_id, policy);
}

auto EmitFieldByFieldArrayWrite(
    Context& ctx, const CuFacts& facts, const mir::WriteTarget& target,
    mir::PlaceId source_place, TypeId type_id, OwnershipPolicy policy)
    -> Result<void> {
  return CommitArrayFieldByField(
      ctx, facts, target, source_place, type_id, policy);
}

auto EmitUnionMemcpyWrite(
    Context& ctx, const CuFacts& facts, const mir::WriteTarget& target,
    mir::PlaceId source_place, TypeId type_id) -> Result<void> {
  auto& builder = ctx.GetBuilder();
  const auto& types = *facts.types;

  auto wt_or_err = commit::Access::GetWriteTarget(ctx, target);
  if (!wt_or_err) {
    throw common::InternalError(
        "EmitUnionMemcpyWrite", "failed to resolve WriteTarget");
  }
  const auto& wt = *wt_or_err;
  llvm::Value* target_ptr = wt.ptr;

  auto info = GetUnionStorageInfo(ctx, facts, type_id);
  if (!info) return std::unexpected(info.error());

  TypeId src_type = mir::TypeOfPlace(types, ctx.GetMirArena()[source_place]);
  if (src_type != type_id) {
    throw common::InternalError(
        "EmitUnionMemcpyWrite",
        "source place type does not match target union type");
  }

  auto source_ptr = ctx.GetPlacePointer(source_place);
  if (!source_ptr) return std::unexpected(source_ptr.error());

  bool is_design_notify =
      wt.canonical_signal_id.has_value() &&
      ctx.GetDesignStoreMode() != DesignStoreMode::kDirectInit;

  auto emit_notify_path = [&]() {
    auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
    auto* func = builder.GetInsertBlock()->getParent();
    llvm::IRBuilder<> entry_builder(
        &func->getEntryBlock(), func->getEntryBlock().begin());
    auto* buf_ty = llvm::ArrayType::get(i8_ty, info->size);
    auto* snapshot =
        entry_builder.CreateAlloca(buf_ty, nullptr, "union.snapshot");
    builder.CreateMemCpy(
        snapshot, llvm::Align(1), target_ptr, llvm::Align(info->align),
        info->size);

    builder.CreateMemCpy(
        target_ptr, llvm::Align(info->align), *source_ptr,
        llvm::Align(info->align), info->size);

    auto view = BuildRawBytesStorageView(target_ptr, info->size);
    auto policy = BuildStorePolicyFromContext(
        ctx, wt.canonical_signal_id,
        wt.mutation_signal ? &*wt.mutation_signal : nullptr);
    auto result = NotifyPackedStorageWritten(ctx, view, snapshot, policy);
    if (!result) {
      throw common::InternalError(
          "EmitUnionMemcpyWrite", "NotifyPackedStorageWritten failed");
    }
  };

  auto emit_plain_memcpy = [&]() {
    builder.CreateMemCpy(
        target_ptr, llvm::Align(info->align), *source_ptr,
        llvm::Align(info->align), info->size);
  };

  if (is_design_notify && (wt.requires_static_dirty_propagation ||
                           !wt.mutation_signal.has_value())) {
    emit_notify_path();
  } else if (is_design_notify && wt.mutation_signal.has_value()) {
    ctx.EmitTraceBranch(
        *wt.mutation_signal, "union.notify", "union.plain", emit_notify_path,
        emit_plain_memcpy);
  } else {
    emit_plain_memcpy();
  }

  return {};
}

}  // namespace

auto ExecuteWritePlan(
    Context& ctx, const CuFacts& facts, const mir::WriteTarget& target,
    const WriteSource& source, const WritePlan& plan, OwnershipPolicy policy)
    -> Result<void> {
  switch (plan.op) {
    case WriteOp::kCommitManagedScalar:
      return EmitManagedScalarWrite(
          ctx, facts, target, source, plan.type_id, policy);

    case WriteOp::kCommitPointerScalar:
      return EmitPointerScalarWrite(ctx, facts, target, source);

    case WriteOp::kStorePlainAggregate:
      return EmitPlainAggregateStore(ctx, facts, target, source);

    case WriteOp::kCommitPackedOrFloatScalar:
      return EmitPackedOrFloatWrite(ctx, facts, target, source, plan.type_id);

    case WriteOp::kCommitFieldByFieldStruct: {
      if (!std::holds_alternative<OperandSource>(source)) {
        return std::unexpected(ctx.GetDiagnosticContext().MakeUnsupported(
            ctx.GetCurrentOrigin(),
            "field-by-field struct commit from raw value source "
            "(e.g. assoc array with managed struct element) not yet supported",
            UnsupportedCategory::kFeature));
      }
      auto src =
          RequireOperandPlace(source, "ExecuteWritePlan/FieldByFieldStruct");
      return EmitFieldByFieldStructWrite(
          ctx, facts, target, src, plan.type_id, policy);
    }

    case WriteOp::kCommitFieldByFieldArray: {
      if (!std::holds_alternative<OperandSource>(source)) {
        return std::unexpected(ctx.GetDiagnosticContext().MakeUnsupported(
            ctx.GetCurrentOrigin(),
            "field-by-field array commit from raw value source "
            "(e.g. assoc array with managed array element) not yet supported",
            UnsupportedCategory::kFeature));
      }
      auto src =
          RequireOperandPlace(source, "ExecuteWritePlan/FieldByFieldArray");
      return EmitFieldByFieldArrayWrite(
          ctx, facts, target, src, plan.type_id, policy);
    }

    case WriteOp::kCommitUnionMemcpy: {
      if (!std::holds_alternative<OperandSource>(source)) {
        return std::unexpected(ctx.GetDiagnosticContext().MakeUnsupported(
            ctx.GetCurrentOrigin(),
            "union memcpy commit from raw value source not yet supported",
            UnsupportedCategory::kFeature));
      }
      auto src = RequireOperandPlace(source, "ExecuteWritePlan/UnionMemcpy");
      return EmitUnionMemcpyWrite(ctx, facts, target, src, plan.type_id);
    }

    case WriteOp::kRejectUnsupported:
      return std::unexpected(ctx.GetDiagnosticContext().MakeUnsupported(
          ctx.GetCurrentOrigin(),
          "unpacked aggregate with container fields "
          "(dynamic array/queue) not yet supported",
          UnsupportedCategory::kFeature));
  }

  throw common::InternalError("ExecuteWritePlan", "unhandled WriteOp");
}

auto DispatchWrite(
    Context& ctx, const CuFacts& facts, const mir::WriteTarget& target,
    const WriteSource& source, TypeId type_id, OwnershipPolicy policy)
    -> Result<void> {
  auto plan = BuildWritePlan(type_id, *facts.types);
  return ExecuteWritePlan(ctx, facts, target, source, plan, policy);
}

auto DispatchPlainWrite(
    Context& ctx, const CuFacts& facts, const mir::WriteTarget& target,
    const WriteSource& source, TypeId type_id) -> Result<void> {
  auto plan = BuildWritePlan(type_id, *facts.types);

  // PlainAssign invariant: only non-managed write ops are reachable.
  switch (plan.op) {
    case WriteOp::kCommitPointerScalar:
      return EmitPointerScalarWrite(ctx, facts, target, source);
    case WriteOp::kStorePlainAggregate:
      return EmitPlainAggregateStore(ctx, facts, target, source);
    case WriteOp::kCommitPackedOrFloatScalar:
      return EmitPackedOrFloatWrite(ctx, facts, target, source, plan.type_id);
    case WriteOp::kCommitUnionMemcpy: {
      if (!std::holds_alternative<OperandSource>(source)) {
        return std::unexpected(ctx.GetDiagnosticContext().MakeUnsupported(
            ctx.GetCurrentOrigin(),
            "union memcpy commit from raw value source not yet supported",
            UnsupportedCategory::kFeature));
      }
      auto src = RequireOperandPlace(source, "DispatchPlainWrite/UnionMemcpy");
      return EmitUnionMemcpyWrite(ctx, facts, target, src, plan.type_id);
    }
    case WriteOp::kCommitManagedScalar:
    case WriteOp::kCommitFieldByFieldStruct:
    case WriteOp::kCommitFieldByFieldArray:
    case WriteOp::kRejectUnsupported:
      throw common::InternalError(
          "DispatchPlainWrite",
          "lifecycle-carrying WriteOp reached plain write path");
  }
  throw common::InternalError("DispatchPlainWrite", "unhandled WriteOp");
}

}  // namespace lyra::lowering::mir_to_llvm
