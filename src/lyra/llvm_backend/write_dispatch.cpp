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
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/ownership.hpp"
#include "lyra/llvm_backend/type_ops/managed.hpp"
#include "lyra/llvm_backend/write_plan.hpp"
#include "lyra/lowering/diagnostic_context.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/operand.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Resolve source to a raw LLVM value, lowering operand if needed.
auto ResolveRawValue(Context& ctx, const WriteSource& source)
    -> Result<llvm::Value*> {
  if (const auto* rv = std::get_if<RawValueSource>(&source)) {
    return rv->value;
  }
  return LowerOperandRaw(ctx, *std::get<OperandSource>(source).operand);
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
    Context& ctx, mir::PlaceId target, const WriteSource& source,
    TypeId type_id, OwnershipPolicy policy) -> Result<void> {
  auto raw = ResolveRawValue(ctx, source);
  if (!raw) return std::unexpected(raw.error());

  // Move cleanup: null-out source handle if moving from temp
  if (const auto* os = std::get_if<OperandSource>(&source)) {
    if (const auto* pid = std::get_if<mir::PlaceId>(&os->operand->payload)) {
      CommitMoveCleanupIfTemp(ctx, *pid, policy, type_id);
    }
  }

  // Resolve WriteTarget and dispatch to string/container commit
  auto wt = commit::Access::GetWriteTarget(ctx, target);
  if (!wt) return std::unexpected(wt.error());

  const auto& type = ctx.GetTypeArena()[type_id];
  switch (GetManagedKind(type.Kind())) {
    case ManagedKind::kString:
      return CommitStringValue(ctx, *wt, *raw, policy, type_id);
    case ManagedKind::kContainer:
      return CommitContainerValue(ctx, *wt, *raw, policy, type_id);
    case ManagedKind::kNone:
      throw common::InternalError(
          "EmitManagedScalarWrite",
          "kManagedScalar shape but ManagedKind::kNone");
  }
  throw common::InternalError(
      "EmitManagedScalarWrite", "unreachable after ManagedKind switch");
}

auto EmitPlainAggregateStore(
    Context& ctx, mir::PlaceId target, const WriteSource& source)
    -> Result<void> {
  auto raw = ResolveRawValue(ctx, source);
  if (!raw) return std::unexpected(raw.error());

  auto ptr = ctx.GetPlacePointer(target);
  if (!ptr) return std::unexpected(ptr.error());

  ctx.GetBuilder().CreateStore(*raw, *ptr);
  CommitNotifyAggregateIfDesignSlot(ctx, target);
  return {};
}

auto EmitPackedOrFloatWrite(
    Context& ctx, mir::PlaceId target, const WriteSource& source,
    TypeId type_id) -> Result<void> {
  auto raw = ResolveRawValue(ctx, source);
  if (!raw) return std::unexpected(raw.error());

  CommitPackedValueRaw(ctx, target, *raw, type_id);
  return {};
}

auto EmitFieldByFieldStructWrite(
    Context& ctx, mir::PlaceId target, mir::PlaceId source_place,
    TypeId type_id, OwnershipPolicy policy) -> Result<void> {
  return CommitStructFieldByField(ctx, target, source_place, type_id, policy);
}

auto EmitFieldByFieldArrayWrite(
    Context& ctx, mir::PlaceId target, mir::PlaceId source_place,
    TypeId type_id, OwnershipPolicy policy) -> Result<void> {
  return CommitArrayFieldByField(ctx, target, source_place, type_id, policy);
}

auto EmitUnionMemcpyWrite(
    Context& ctx, mir::PlaceId target, mir::PlaceId source_place,
    TypeId type_id) -> Result<void> {
  auto& builder = ctx.GetBuilder();
  const auto& types = ctx.GetTypeArena();

  auto target_ptr = ctx.GetPlacePointer(target);
  if (!target_ptr) return std::unexpected(target_ptr.error());

  auto info = GetUnionStorageInfo(ctx, type_id);
  if (!info) return std::unexpected(info.error());

  // Verify source type matches target union type
  TypeId src_type = mir::TypeOfPlace(types, ctx.GetMirArena()[source_place]);
  if (src_type != type_id) {
    throw common::InternalError(
        "EmitUnionMemcpyWrite",
        "source place type does not match target union type");
  }

  auto source_ptr = ctx.GetPlacePointer(source_place);
  if (!source_ptr) return std::unexpected(source_ptr.error());

  builder.CreateMemCpy(
      *target_ptr, llvm::Align(info->align), *source_ptr,
      llvm::Align(info->align), info->size);

  CommitNotifyUnionMemcpyIfDesignSlot(ctx, target, info->size);
  return {};
}

}  // namespace

auto ExecuteWritePlan(
    Context& ctx, mir::PlaceId target, const WriteSource& source,
    const WritePlan& plan, OwnershipPolicy policy) -> Result<void> {
  switch (plan.op) {
    case WriteOp::kCommitManagedScalar:
      return EmitManagedScalarWrite(ctx, target, source, plan.type_id, policy);

    case WriteOp::kStorePlainAggregate:
      return EmitPlainAggregateStore(ctx, target, source);

    case WriteOp::kCommitPackedOrFloatScalar:
      return EmitPackedOrFloatWrite(ctx, target, source, plan.type_id);

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
          ctx, target, src, plan.type_id, policy);
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
      return EmitFieldByFieldArrayWrite(ctx, target, src, plan.type_id, policy);
    }

    case WriteOp::kCommitUnionMemcpy: {
      if (!std::holds_alternative<OperandSource>(source)) {
        return std::unexpected(ctx.GetDiagnosticContext().MakeUnsupported(
            ctx.GetCurrentOrigin(),
            "union memcpy commit from raw value source not yet supported",
            UnsupportedCategory::kFeature));
      }
      auto src = RequireOperandPlace(source, "ExecuteWritePlan/UnionMemcpy");
      return EmitUnionMemcpyWrite(ctx, target, src, plan.type_id);
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
    Context& ctx, mir::PlaceId target, const WriteSource& source,
    TypeId type_id, OwnershipPolicy policy) -> Result<void> {
  auto plan = BuildWritePlan(type_id, ctx.GetTypeArena());
  return ExecuteWritePlan(ctx, target, source, plan, policy);
}

}  // namespace lyra::lowering::mir_to_llvm
