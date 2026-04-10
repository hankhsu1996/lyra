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
auto ClassifyDeferredStoreByType(Context& context, TypeId type_id)
    -> StoreShape {
  const auto& types = context.GetTypeArena();
  const Type& type = types[type_id];
  auto* storage_ty = GetLlvmTypeForTypeId(
      context.GetLlvmContext(), type_id, types, context.IsForceTwoState());
  const auto& dl = context.GetModule().getDataLayout();

  bool is_scalar_mir = IsPacked(type);
  if (is_scalar_mir) {
    bool is_four_state = context.IsPackedFourState(type);
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

// Canonical deferred store core parameterized by TypeId.
// Handles value/mask construction uniformly via StoreShape classification.
auto EmitDeferredStoreCore(
    Context& context, const mir::DeferredAssign& deferred,
    const StoreShape& shape, llvm::Value* write_ptr,
    llvm::Value* notify_base_ptr, const SignalCoordExpr& signal_id,
    TypeId target_type) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();

  auto raw_or_err = LowerRhsRaw(context, deferred.rhs, target_type);
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
  if (signal_id.IsLocal()) {
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

// BitRangeProjection NBA: route through packed storage view module.
// The storage layer owns subview classification and emission shape;
// this function is a thin routing wrapper.
auto LowerDeferredAssignBitRange(
    Context& context, const mir::DeferredAssign& deferred,
    const SignalCoordExpr& signal_id, mir::PlaceId dest) -> Result<void> {
  auto path = ExtractPackedAccessPath(context, dest);
  if (!path) return std::unexpected(path.error());

  auto subview = ResolvePackedSubview(context, *path);
  if (!subview) return std::unexpected(subview.error());

  auto rvalue = detail::LowerRhsToPackedRValue(
      context, deferred.rhs, subview->semantic_bit_width, subview->result_type);
  if (!rvalue) return std::unexpected(rvalue.error());

  PackedNbaPolicy nba_policy{
      .engine_ptr = context.GetEnginePointer(),
      .notify_base_ptr = context.GetStorageRootPointer(dest),
      .signal_id = signal_id,
  };

  return EmitDeferredStoreToPackedSubview(
      context, *subview, *rvalue, nba_policy);
}

// IndexProjection NBA: array element write with OOB guard.
// Uses StoreShape classification and EmitDeferredStoreCore.
auto LowerDeferredAssignWithOobGuard(
    Context& context, const mir::DeferredAssign& deferred,
    const StoreShape& shape, const SignalCoordExpr& signal_id,
    mir::PlaceId dest) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& types = context.GetTypeArena();
  const auto& place = context.LookupPlace(dest);

  // Walk projections to find the first IndexProjection and compute the array
  // type AT that projection site. This handles cases like s.field[i] where the
  // root type is a struct but the indexed type is an array field.
  const mir::IndexProjection* idx_proj = nullptr;
  TypeId array_type_id = place.root.type;
  for (const auto& proj : place.projections) {
    if (const auto* idx = std::get_if<mir::IndexProjection>(&proj.info)) {
      idx_proj = idx;
      break;  // array_type_id is now the array being indexed
    }
    // Advance type through non-index projections
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
    // SliceProjection and DerefProjection not yet supported
  }

  const Type& arr_type = types[array_type_id];
  if (arr_type.Kind() != TypeKind::kUnpackedArray) {
    throw common::InternalError(
        "LowerDeferredAssignWithOobGuard",
        std::format(
            "expected UnpackedArray at IndexProjection, got {}",
            ToString(arr_type.Kind())));
  }
  auto arr_size = arr_type.AsUnpackedArray().range.Size();

  // Compute bounds check
  auto index_or_err = LowerOperand(context, idx_proj->index);
  if (!index_or_err) return std::unexpected(index_or_err.error());
  llvm::Value* index = *index_or_err;
  auto* arr_size_val = llvm::ConstantInt::get(index->getType(), arr_size);
  auto* in_bounds = builder.CreateICmpULT(index, arr_size_val, "nba.inbounds");

  // Create conditional branch
  auto* func = builder.GetInsertBlock()->getParent();
  auto* schedule_bb = llvm::BasicBlock::Create(llvm_ctx, "nba.schedule", func);
  auto* skip_bb = llvm::BasicBlock::Create(llvm_ctx, "nba.skip", func);
  builder.CreateCondBr(in_bounds, schedule_bb, skip_bb);

  // Schedule block: compute pointers and emit via shared core
  builder.SetInsertPoint(schedule_bb);
  auto write_ptr_or_err = context.GetPlacePointer(dest);
  if (!write_ptr_or_err) return std::unexpected(write_ptr_or_err.error());
  llvm::Value* write_ptr = *write_ptr_or_err;
  llvm::Value* notify_base_ptr = context.GetStorageRootPointer(dest);

  TypeId dest_type = mir::TypeOfPlace(types, context.LookupPlace(dest));
  auto result = EmitDeferredStoreCore(
      context, deferred, shape, write_ptr, notify_base_ptr, signal_id,
      dest_type);
  if (!result) return result;
  builder.CreateBr(skip_bb);

  builder.SetInsertPoint(skip_bb);
  return {};
}

}  // namespace

auto LowerDeferredAssign(Context& context, const mir::DeferredAssign& deferred)
    -> Result<void> {
  const auto& dest = deferred.dest;
  const auto* dest_place = std::get_if<mir::PlaceId>(&dest);

  // Resolve destination type and signal coord from WriteTarget.
  TypeId dest_type = detail::ResolveDestType(context, dest);

  // Signal coord: for PlaceId use existing resolver, for ExternalRefId use
  // direct helper.
  SignalCoordExpr signal_id = dest_place != nullptr
                                  ? GetSignalCoordForNba(context, *dest_place)
                                  : context.EmitExternalRefSignalCoord(
                                        std::get<mir::ExternalRefId>(dest));

  // Case 1: BitRangeProjection (PlaceId-only, external refs have no
  // projections).
  if (dest_place != nullptr && context.HasBitRangeProjection(*dest_place)) {
    return LowerDeferredAssignBitRange(
        context, deferred, signal_id, *dest_place);
  }

  // Classify destination once via MIR type.
  StoreShape shape = ClassifyDeferredStoreByType(context, dest_type);

  // Case 2: IndexProjection (PlaceId-only).
  if (dest_place != nullptr &&
      HasIndexProjection(context.LookupPlace(*dest_place))) {
    return LowerDeferredAssignWithOobGuard(
        context, deferred, shape, signal_id, *dest_place);
  }

  // Case 3: Simple full-width write. Works for both PlaceId and ExternalRefId.
  // Resolve write pointer and notify base from WriteTarget.
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
      context, deferred, shape, write_ptr, notify_base_ptr, signal_id,
      dest_type);
}

auto LowerDeferredAssign(
    Context& context, SlotAccessResolver& resolver,
    const mir::DeferredAssign& deferred) -> Result<void> {
  // DeferredAssign destinations are never managed (kDeferredWrite is
  // ineligible), but the RHS operands may read managed module slots.
  // Sync managed slots to canonical before delegating so canonical
  // operand reads see up-to-date values.
  if (auto* al = dynamic_cast<ActivationLocalSlotAccess*>(&resolver)) {
    al->SyncToCanonical();
  }
  return LowerDeferredAssign(context, deferred);
}

}  // namespace lyra::lowering::mir_to_llvm
