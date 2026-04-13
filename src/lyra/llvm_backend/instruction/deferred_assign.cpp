#include "lyra/llvm_backend/instruction/deferred_assign.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <variant>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/commit/signal_id_expr.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/cu_facts.hpp"
#include "lyra/llvm_backend/instruction/assign_core.hpp"
#include "lyra/llvm_backend/layout/layout.hpp"
#include "lyra/llvm_backend/packed_storage_view.hpp"
#include "lyra/llvm_backend/slot_access.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

using detail::HasIndexProjection;
using detail::IsFourStateScalarStruct;
using detail::LlvmTypeToString;
using detail::LowerRhsRaw;

namespace {

// Classification of deferred store targets for correct-by-construction codegen.
// Derived from MIR type (TypeOfPlace), not from LLVM type shape.
enum class StoreKind {
  kScalar2State,    // 2-state packed scalar (integer mask)
  kScalar4State,    // 4-state packed scalar (struct {val, unk} mask)
  kAggregateBytes,  // Aggregate (byte-level 0xFF mask via runtime)
};

// Shape information for a deferred store target.
// storage_ty is AUTHORITATIVE - never re-synthesize LLVM types from bit_width.
struct StoreShape {
  StoreKind kind = StoreKind::kAggregateBytes;
  llvm::Type* storage_ty = nullptr;  // LLVM type from GetPlaceLlvmType
  uint32_t bit_width = 0;            // For scalars: semantic width
  uint32_t byte_size = 0;            // For aggregates: from DataLayout
};

// Canonical deferred store classification by TypeId.
// Uses MIR type for semantic classification; LLVM type only for mechanical
// emission. Includes backstop: if MIR says scalar but LLVM disagrees, fall back
// to AggregateBytes (safe, always works).
auto ClassifyDeferredStoreByType(
    Context& context, const CuFacts& facts, TypeId type_id) -> StoreShape {
  const auto& types = *facts.types;
  const Type& type = types[type_id];
  auto* storage_ty = GetLlvmTypeForTypeId(
      context.GetLlvmContext(), type_id, types, facts.force_two_state);
  const auto& dl = context.GetModule().getDataLayout();

  bool is_scalar_mir = IsPacked(type);
  if (is_scalar_mir) {
    bool is_four_state = IsPackedFourState(facts, type);
    uint32_t bit_width = (type.Kind() == TypeKind::kIntegral)
                             ? type.AsIntegral().bit_width
                             : PackedBitWidth(type, types);

    if (is_four_state) {
      if (!IsFourStateScalarStruct(storage_ty)) {
        auto byte_size = static_cast<uint32_t>(dl.getTypeStoreSize(storage_ty));
        return StoreShape{
            .kind = StoreKind::kAggregateBytes,
            .storage_ty = storage_ty,
            .byte_size = byte_size};
      }
      return StoreShape{
          .kind = StoreKind::kScalar4State,
          .storage_ty = storage_ty,
          .bit_width = bit_width};
    }
    if (!storage_ty->isIntegerTy()) {
      auto byte_size = static_cast<uint32_t>(dl.getTypeStoreSize(storage_ty));
      return StoreShape{
          .kind = StoreKind::kAggregateBytes,
          .storage_ty = storage_ty,
          .byte_size = byte_size};
    }
    return StoreShape{
        .kind = StoreKind::kScalar2State,
        .storage_ty = storage_ty,
        .bit_width = bit_width};
  }

  auto byte_size = static_cast<uint32_t>(dl.getTypeStoreSize(storage_ty));
  return StoreShape{
      .kind = StoreKind::kAggregateBytes,
      .storage_ty = storage_ty,
      .byte_size = byte_size};
}

// Coerce raw RHS value to match store shape.
// - kScalar2State: ZExtOrTrunc to target integer
// - kScalar4State: Extract/create val+unk, pack into struct
// - kAggregateBytes: Assert type match (if mismatch, LowerRhsRaw bug)
auto CoerceValueToShape(
    Context& context, llvm::Value* raw_value, const StoreShape& shape)
    -> llvm::Value* {
  auto& builder = context.GetBuilder();

  switch (shape.kind) {
    case StoreKind::kScalar2State: {
      auto* target_ty = llvm::cast<llvm::IntegerType>(shape.storage_ty);
      if (raw_value->getType() == target_ty) return raw_value;
      return builder.CreateZExtOrTrunc(raw_value, target_ty);
    }
    case StoreKind::kScalar4State: {
      auto* struct_ty = llvm::cast<llvm::StructType>(shape.storage_ty);
      auto* elem_ty =
          llvm::cast<llvm::IntegerType>(struct_ty->getElementType(0));

      llvm::Value* val = nullptr;
      llvm::Value* unk = nullptr;
      if (raw_value->getType()->isStructTy()) {
        // Validate RHS is also a canonical 4-state struct
        if (!IsFourStateScalarStruct(raw_value->getType())) {
          throw common::InternalError(
              "CoerceValueToShape",
              std::format(
                  "kScalar4State expects canonical {{iN,iN}} struct, got {}",
                  LlvmTypeToString(raw_value->getType())));
        }
        val = builder.CreateExtractValue(raw_value, 0);
        unk = builder.CreateExtractValue(raw_value, 1);
      } else {
        val = raw_value;
        unk = llvm::ConstantInt::get(elem_ty, 0);
      }
      val = builder.CreateZExtOrTrunc(val, elem_ty);
      unk = builder.CreateZExtOrTrunc(unk, elem_ty);

      llvm::Value* result = llvm::UndefValue::get(struct_ty);
      result = builder.CreateInsertValue(result, val, 0);
      result = builder.CreateInsertValue(result, unk, 1);
      return result;
    }
    case StoreKind::kAggregateBytes: {
      // Type must match exactly - if not, it's a bug in LowerRhsRaw
      if (raw_value->getType() != shape.storage_ty) {
        throw common::InternalError(
            "CoerceValueToShape",
            std::format(
                "AggregateBytes type mismatch: rhs={}, expected={}",
                LlvmTypeToString(raw_value->getType()),
                LlvmTypeToString(shape.storage_ty)));
      }
      return raw_value;
    }
  }
  // Unreachable
  return nullptr;
}

// Generic NBA queue path for non-instance-owned targets.
//
// ARCHITECTURAL INVARIANT:
// This function emits calls to LyraScheduleNba{Local,Global}, which enqueue
// onto the generic nba_queue_. Only the following targets are permitted:
//   - Global/package signals (SignalCoordExpr::kGlobal)
//   - Cross-instance local signals (SignalCoordExpr::kLocalCrossInstance)
//   - ExternalRefId targets (always global)
// Instance-owned local signals (same-instance, owned inline or container)
// MUST use EmitDeferredWriteLocal instead. The ownership gate in
// LowerDeferredAssign enforces this at codegen time.
auto EmitDeferredStoreCore(
    Context& context, const CuFacts& facts, const mir::DeferredAssign& deferred,
    const StoreShape& shape, llvm::Value* write_ptr,
    llvm::Value* notify_base_ptr, const SignalCoordExpr& signal_id,
    TypeId target_type) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  auto raw_or_err = LowerRhsRaw(context, facts, deferred.rhs, target_type);
  if (!raw_or_err) return std::unexpected(raw_or_err.error());

  llvm::Value* source_value = CoerceValueToShape(context, *raw_or_err, shape);
  auto* val_alloca = builder.CreateAlloca(shape.storage_ty, nullptr, "nba.val");
  builder.CreateStore(source_value, val_alloca);

  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::PointerType::get(llvm_ctx, 0));
  uint32_t byte_size =
      (shape.kind == StoreKind::kAggregateBytes)
          ? shape.byte_size
          : static_cast<uint32_t>(
                context.GetModule().getDataLayout().getTypeStoreSize(
                    shape.storage_ty));

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  if (signal_id.IsExtRef()) {
    builder.CreateCall(
        context.GetLyraScheduleNbaExtRef(),
        {context.GetEnginePointer(), context.GetInstancePointer(),
         signal_id.Emit(builder), write_ptr, notify_base_ptr, val_alloca,
         null_ptr, llvm::ConstantInt::get(i32_ty, byte_size)});
  } else if (signal_id.IsLocal()) {
    builder.CreateCall(
        context.GetLyraScheduleNbaLocal(),
        {context.GetEnginePointer(),
         signal_id.GetInstancePointer(context.GetInstancePointer()), write_ptr,
         notify_base_ptr, val_alloca, null_ptr,
         llvm::ConstantInt::get(i32_ty, byte_size), signal_id.Emit(builder)});
  } else {
    builder.CreateCall(
        context.GetLyraScheduleNbaGlobal(),
        {context.GetEnginePointer(), write_ptr, notify_base_ptr, val_alloca,
         null_ptr, llvm::ConstantInt::get(i32_ty, byte_size),
         signal_id.Emit(builder)});
  }
  return {};
}

// Instance-owned deferred byte-range write for local NBA.
// Handles whole-slot and sub-slot writes. Identity-based: passes
// (local_signal_id, body_byte_offset, is_partial) instead of
// current-storage pointers. body_byte_offset is an LLVM Value to
// support both static (field/union) and dynamic (index) offsets.
auto EmitDeferredWriteLocal(
    Context& context, const CuFacts& facts, const mir::DeferredAssign& deferred,
    const StoreShape& shape, llvm::Value* body_byte_offset,
    const SignalCoordExpr& signal_id, TypeId target_type, bool is_partial)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  auto raw_or_err = LowerRhsRaw(context, facts, deferred.rhs, target_type);
  if (!raw_or_err) return std::unexpected(raw_or_err.error());

  llvm::Value* source_value = CoerceValueToShape(context, *raw_or_err, shape);
  auto* val_alloca = builder.CreateAlloca(shape.storage_ty, nullptr, "nba.val");
  builder.CreateStore(source_value, val_alloca);

  uint32_t byte_size =
      (shape.kind == StoreKind::kAggregateBytes)
          ? shape.byte_size
          : static_cast<uint32_t>(
                context.GetModule().getDataLayout().getTypeStoreSize(
                    shape.storage_ty));

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  builder.CreateCall(
      context.GetLyraDeferredWriteLocal(),
      {context.GetEnginePointer(),
       signal_id.GetInstancePointer(context.GetInstancePointer()), val_alloca,
       llvm::ConstantInt::get(i32_ty, byte_size), signal_id.Emit(builder),
       body_byte_offset, llvm::ConstantInt::get(i32_ty, is_partial ? 1 : 0)});
  return {};
}

// BitRangeProjection NBA: route through packed storage view module.
// The storage layer owns subview classification and emission shape;
// this function is a thin routing wrapper. The ownership flag is forwarded
// to PackedNbaPolicy so the packed dispatch can route local owned-inline
// targets to deferred storage.
auto LowerDeferredAssignBitRange(
    Context& context, const CuFacts& facts, const mir::DeferredAssign& deferred,
    const SignalCoordExpr& signal_id, mir::PlaceId dest, bool is_local_owned)
    -> Result<void> {
  auto path = ExtractPackedAccessPath(context, facts, dest);
  if (!path) return std::unexpected(path.error());

  auto subview = ResolvePackedSubview(context, *path);
  if (!subview) return std::unexpected(subview.error());

  auto rvalue = detail::LowerRhsToPackedRValue(
      context, facts, deferred.rhs, subview->semantic_bit_width,
      subview->result_type);
  if (!rvalue) return std::unexpected(rvalue.error());

  uint32_t slot_body_offset = 0;
  if (is_local_owned) {
    slot_body_offset = context.IsOwnedContainerSlot(dest)
                           ? context.GetContainerBodyByteOffset(dest)
                           : context.GetSlotBodyByteOffset(dest);
  }

  PackedNbaPolicy nba_policy{
      .engine_ptr = context.GetEnginePointer(),
      .notify_base_ptr = context.GetStorageRootPointer(dest),
      .signal_id = signal_id,
      .is_local_owned = is_local_owned,
      .slot_body_offset = slot_body_offset,
  };

  return EmitDeferredStoreToPackedSubview(
      context, *subview, *rvalue, nba_policy);
}

// Extract array bounds info from a place with IndexProjection.
// Returns (IndexProjection*, array_size) for the first IndexProjection found.
struct IndexProjectionInfo {
  const mir::IndexProjection* proj = nullptr;
  uint64_t array_size = 0;
};

auto ExtractIndexProjectionInfo(
    Context& context, const CuFacts& facts, mir::PlaceId dest)
    -> IndexProjectionInfo {
  const auto& types = *facts.types;
  const auto& place = context.LookupPlace(dest);

  const mir::IndexProjection* idx_proj = nullptr;
  TypeId array_type_id = place.root.type;
  for (const auto& proj : place.projections) {
    if (const auto* idx = std::get_if<mir::IndexProjection>(&proj.info)) {
      idx_proj = idx;
      break;
    }
    const Type& cur_type = types[array_type_id];
    if (const auto* fp = std::get_if<mir::FieldProjection>(&proj.info)) {
      array_type_id = cur_type.AsUnpackedStruct()
                          .fields[static_cast<size_t>(fp->field_index)]
                          .type;
    } else if (
        const auto* up = std::get_if<mir::UnionMemberProjection>(&proj.info)) {
      array_type_id = cur_type.AsUnpackedUnion().members[up->member_index].type;
    } else if (
        const auto* bp = std::get_if<mir::BitRangeProjection>(&proj.info)) {
      array_type_id = bp->element_type;
    }
  }

  const Type& arr_type = types[array_type_id];
  if (arr_type.Kind() != TypeKind::kUnpackedArray) {
    throw common::InternalError(
        "ExtractIndexProjectionInfo",
        std::format(
            "expected UnpackedArray at IndexProjection, got {}",
            ToString(arr_type.Kind())));
  }
  return {
      .proj = idx_proj, .array_size = arr_type.AsUnpackedArray().range.Size()};
}

// IndexProjection NBA: array element write with OOB guard.
// For local owned targets, uses deferred byte-range write with
// dynamic body_offset. For non-local, uses generic EmitDeferredStoreCore.
auto LowerDeferredAssignWithOobGuard(
    Context& context, const CuFacts& facts, const mir::DeferredAssign& deferred,
    const StoreShape& shape, const SignalCoordExpr& signal_id,
    mir::PlaceId dest, bool is_local_owned) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = *facts.types;

  auto [idx_proj, arr_size] = ExtractIndexProjectionInfo(context, facts, dest);

  // Compute bounds check
  auto index_or_err = LowerOperand(context, facts, idx_proj->index);
  if (!index_or_err) return std::unexpected(index_or_err.error());
  llvm::Value* index = *index_or_err;
  auto* arr_size_val = llvm::ConstantInt::get(index->getType(), arr_size);
  auto* in_bounds = builder.CreateICmpULT(index, arr_size_val, "nba.inbounds");

  auto* func = builder.GetInsertBlock()->getParent();
  auto* schedule_bb = llvm::BasicBlock::Create(llvm_ctx, "nba.schedule", func);
  auto* skip_bb = llvm::BasicBlock::Create(llvm_ctx, "nba.skip", func);
  builder.CreateCondBr(in_bounds, schedule_bb, skip_bb);

  builder.SetInsertPoint(schedule_bb);

  if (is_local_owned) {
    // Compute dynamic body_offset: base_offset + (write_ptr - slot_root).
    // GetPlacePointer applies all projections (field + index), so
    // ptr - root = sub-slot offset.
    // For inline slots: base is the inline slot offset.
    // For container slots: base is the appendix backing data offset.
    auto write_ptr_or_err = context.GetPlacePointer(dest);
    if (!write_ptr_or_err) return std::unexpected(write_ptr_or_err.error());
    llvm::Value* write_ptr = *write_ptr_or_err;
    llvm::Value* slot_root = context.GetStorageRootPointer(dest);

    auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
    auto* sub_offset = builder.CreatePtrDiff(
        llvm::Type::getInt8Ty(llvm_ctx), write_ptr, slot_root, "nba.suboff");
    auto* sub_offset_i32 = builder.CreateTrunc(sub_offset, i32_ty);
    uint32_t slot_body_off = context.IsOwnedContainerSlot(dest)
                                 ? context.GetContainerBodyByteOffset(dest)
                                 : context.GetSlotBodyByteOffset(dest);
    auto* body_offset = builder.CreateAdd(
        llvm::ConstantInt::get(i32_ty, slot_body_off), sub_offset_i32,
        "nba.bodyoff");

    TypeId dest_type = mir::TypeOfPlace(types, context.LookupPlace(dest));
    auto result = EmitDeferredWriteLocal(
        context, facts, deferred, shape, body_offset, signal_id, dest_type,
        true);
    if (!result) return result;
  } else {
    if (signal_id.GetKind() == SignalCoordExpr::Kind::kLocal) {
      if (context.IsOwnedInlineSlot(dest) ||
          context.IsOwnedContainerSlot(dest)) {
        throw common::InternalError(
            "LowerDeferredAssignWithOobGuard",
            "same-instance owned signal reached generic NBA queue fallback");
      }
    }
    auto write_ptr_or_err = context.GetPlacePointer(dest);
    if (!write_ptr_or_err) return std::unexpected(write_ptr_or_err.error());
    llvm::Value* write_ptr = *write_ptr_or_err;
    llvm::Value* notify_base_ptr = context.GetStorageRootPointer(dest);
    TypeId dest_type = mir::TypeOfPlace(types, context.LookupPlace(dest));
    auto result = EmitDeferredStoreCore(
        context, facts, deferred, shape, write_ptr, notify_base_ptr, signal_id,
        dest_type);
    if (!result) return result;
  }
  builder.CreateBr(skip_bb);

  builder.SetInsertPoint(skip_bb);
  return {};
}

}  // namespace

auto LowerDeferredAssign(
    Context& context, const CuFacts& facts, const mir::DeferredAssign& deferred)
    -> Result<void> {
  const auto& dest = deferred.dest;
  const auto* dest_place = std::get_if<mir::PlaceId>(&dest);

  // Resolve destination type and signal coord from WriteTarget.
  TypeId dest_type = detail::ResolveDestType(context, facts, dest);

  // Signal coord: for PlaceId use existing resolver, for ExternalRefId use
  // direct helper.
  SignalCoordExpr signal_id = dest_place != nullptr
                                  ? GetSignalCoordForNba(context, *dest_place)
                                  : context.EmitExternalRefSignalCoord(
                                        std::get<mir::ExternalRefId>(dest));

  // ROUTING INVARIANT: local owned state <= must use deferred storage.
  //
  // Same-instance owned signals (inline or container) go through
  // EmitDeferredWriteLocal -> LyraDeferredWriteLocal, writing to
  // per-instance deferred storage committed in CommitDeferredLocalNbas.
  //
  // Everything else (global, package, cross-instance, ExternalRefId)
  // goes through EmitDeferredStoreCore -> LyraScheduleNba{Local,Global},
  // enqueuing onto the generic nba_queue_.
  //
  // ExternalRefId always produces SignalCoordExpr::Global -- it never
  // reaches the ownership gate (dest_place is nullptr for external refs).
  bool is_local_owned_inline = dest_place != nullptr && signal_id.IsLocal() &&
                               context.IsOwnedInlineSlot(*dest_place);
  bool is_local_owned_container = dest_place != nullptr &&
                                  signal_id.IsLocal() &&
                                  context.IsOwnedContainerSlot(*dest_place);
  bool is_local_owned = is_local_owned_inline || is_local_owned_container;

  // Case 1: BitRangeProjection (PlaceId-only, external refs have no
  // projections). PackedNbaPolicy carries the ownership flag so the packed
  // subview dispatch can route to deferred storage for local targets.
  if (dest_place != nullptr && context.HasBitRangeProjection(*dest_place)) {
    return LowerDeferredAssignBitRange(
        context, facts, deferred, signal_id, *dest_place, is_local_owned);
  }

  // Classify destination once via MIR type.
  StoreShape shape = ClassifyDeferredStoreByType(context, facts, dest_type);

  // Case 2: IndexProjection (PlaceId-only). Dynamic offset, always partial.
  if (dest_place != nullptr &&
      HasIndexProjection(context.LookupPlace(*dest_place))) {
    return LowerDeferredAssignWithOobGuard(
        context, facts, deferred, shape, signal_id, *dest_place,
        is_local_owned);
  }

  // Case 3: Full-width write or static field/union projection chain.
  auto* i32_ty = llvm::Type::getInt32Ty(context.GetLlvmContext());

  if (is_local_owned) {
    // For inline slots: body_offset is the inline slot offset.
    // For container slots: body_offset is the appendix backing data offset.
    uint32_t slot_body_off =
        is_local_owned_inline ? context.GetSlotBodyByteOffset(*dest_place)
                              : context.GetContainerBodyByteOffset(*dest_place);
    const auto& projs = context.LookupPlace(*dest_place).projections;

    if (!projs.empty()) {
      auto static_proj = context.ComputeStaticProjectionOffset(*dest_place);
      if (!static_proj.has_value()) {
        throw common::InternalError(
            "LowerDeferredAssign",
            "local owned target has unsupported projection chain "
            "that reached Case 3 (not BitRange, not Index)");
      }
      auto [sub_offset, sub_size] = *static_proj;
      auto* body_offset =
          llvm::ConstantInt::get(i32_ty, slot_body_off + sub_offset);
      return EmitDeferredWriteLocal(
          context, facts, deferred, shape, body_offset, signal_id, dest_type,
          true);
    }
    // Whole-slot write: no projections.
    auto* body_offset = llvm::ConstantInt::get(i32_ty, slot_body_off);
    return EmitDeferredWriteLocal(
        context, facts, deferred, shape, body_offset, signal_id, dest_type,
        false);
  }

  // Non-local fallback: global, external ref, or cross-instance targets.
  // Invariant: if we reach here with a same-instance local signal, the
  // ownership gate above must have determined it is NOT instance-owned.
  // A same-instance owned signal reaching the generic queue is a bug.
  if (dest_place != nullptr &&
      signal_id.GetKind() == SignalCoordExpr::Kind::kLocal) {
    if (context.IsOwnedInlineSlot(*dest_place) ||
        context.IsOwnedContainerSlot(*dest_place)) {
      throw common::InternalError(
          "LowerDeferredAssign",
          "same-instance owned signal reached generic NBA queue fallback");
    }
  }

  llvm::Value* write_ptr = nullptr;
  llvm::Value* notify_base_ptr = nullptr;
  if (dest_place != nullptr) {
    auto write_ptr_or_err = context.GetPlacePointer(*dest_place);
    if (!write_ptr_or_err) return std::unexpected(write_ptr_or_err.error());
    write_ptr = *write_ptr_or_err;
    notify_base_ptr = context.GetStorageRootPointer(*dest_place);
  } else {
    auto ref_id = std::get<mir::ExternalRefId>(dest);
    write_ptr = context.EmitExternalRefAddress(ref_id);
    notify_base_ptr = write_ptr;
  }

  return EmitDeferredStoreCore(
      context, facts, deferred, shape, write_ptr, notify_base_ptr, signal_id,
      dest_type);
}

auto LowerDeferredAssign(
    Context& context, const CuFacts& facts, SlotAccessResolver& resolver,
    const mir::DeferredAssign& deferred) -> Result<void> {
  // DeferredAssign destinations are never managed (kDeferredWrite is
  // ineligible), but the RHS operands may read managed module slots.
  // Sync managed slots to canonical before delegating so canonical
  // operand reads see up-to-date values.
  if (auto* al = dynamic_cast<ActivationLocalSlotAccess*>(&resolver)) {
    al->SyncToCanonical();
  }
  return LowerDeferredAssign(context, facts, deferred);
}

}  // namespace lyra::lowering::mir_to_llvm
