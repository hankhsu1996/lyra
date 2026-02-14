#include "lyra/llvm_backend/instruction/deferred_assign.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <string>
#include <variant>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/commit.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/instruction/assign_core.hpp"
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

// Classify destination for deferred store via MIR TypeOfPlace.
// Uses MIR type for semantic classification; LLVM type only for mechanical
// emission. Includes backstop: if MIR says scalar but LLVM disagrees, fall back
// to AggregateBytes (safe, always works).
auto ClassifyDeferredStore(Context& context, mir::PlaceId dest)
    -> Result<StoreShape> {
  const auto& types = context.GetTypeArena();
  const auto& arena = context.GetMirArena();

  TypeId dst_ty = mir::TypeOfPlace(types, arena[dest]);
  const Type& type = types[dst_ty];

  auto storage_ty_result = context.GetPlaceLlvmType(dest);
  if (!storage_ty_result) return std::unexpected(storage_ty_result.error());
  llvm::Type* storage_ty = *storage_ty_result;

  const auto& dl = context.GetModule().getDataLayout();

  // Classification: packed types (including kIntegral) are scalar; else
  // aggregate. IsPacked returns true only for kIntegral, kPackedArray,
  // kPackedStruct, kEnum. All unpacked types (kUnpackedStruct, kUnpackedArray,
  // kUnpackedUnion, kReal, kShortReal, kString, kDynamicArray, kQueue) go to
  // aggregate path.
  bool is_scalar_mir = IsPacked(type);

  if (is_scalar_mir) {
    bool is_four_state = (type.Kind() == TypeKind::kIntegral)
                             ? type.AsIntegral().is_four_state
                             : IsPackedFourState(type, types);
    uint32_t bit_width = (type.Kind() == TypeKind::kIntegral)
                             ? type.AsIntegral().bit_width
                             : PackedBitWidth(type, types);

    if (is_four_state) {
      // BACKSTOP: Validate storage_ty is canonical 4-state struct
      if (!IsFourStateScalarStruct(storage_ty)) {
        // MIR says scalar but LLVM disagrees -> fall back to AggregateBytes
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
    // BACKSTOP: Validate storage_ty is integer
    if (!storage_ty->isIntegerTy()) {
      // MIR says scalar but LLVM disagrees -> fall back to AggregateBytes
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

  // Everything else: aggregate (arrays, unpacked structs, real, etc.)
  auto byte_size = static_cast<uint32_t>(dl.getTypeStoreSize(storage_ty));
  return StoreShape{
      .kind = StoreKind::kAggregateBytes,
      .storage_ty = storage_ty,
      .byte_size = byte_size};
}

// Build all-ones mask constant for the given store shape.
// - Scalars: type-level all-ones (contiguous bytes)
// - Aggregates: explicit [N x i8] byte mask filled with 0xFF (safe for any
// type)
auto BuildAllOnesMask(Context& context, const StoreShape& shape)
    -> llvm::Constant* {
  auto& llvm_ctx = context.GetLlvmContext();

  switch (shape.kind) {
    case StoreKind::kScalar2State: {
      auto* int_ty = llvm::cast<llvm::IntegerType>(shape.storage_ty);
      return llvm::ConstantInt::get(
          int_ty, llvm::APInt::getAllOnes(int_ty->getBitWidth()));
    }
    case StoreKind::kScalar4State: {
      auto* st = llvm::cast<llvm::StructType>(shape.storage_ty);
      auto* elem_ty = llvm::cast<llvm::IntegerType>(st->getElementType(0));
      auto* ones = llvm::ConstantInt::get(
          elem_ty, llvm::APInt::getAllOnes(elem_ty->getBitWidth()));
      return llvm::ConstantStruct::get(st, {ones, ones});
    }
    case StoreKind::kAggregateBytes: {
      // CRITICAL: Byte-level mask, not Constant::getAllOnesValue
      // Runtime does byte-mask: (old & ~mask) | (new & mask)
      // Must be [byte_size x i8] filled with 0xFF
      // Use ConstantDataArray for efficiency (single allocation vs vector)
      std::string all_ones(shape.byte_size, static_cast<char>(0xFF));
      return llvm::ConstantDataArray::getString(llvm_ctx, all_ones, false);
    }
  }
  // Unreachable, but needed for -Wreturn-type
  return nullptr;
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

// Shared core for deferred store emission (used by Direct and WithOobGuard).
// Handles value/mask construction uniformly via StoreShape classification.
auto EmitDeferredStoreCore(
    Context& context, const mir::DeferredAssign& deferred,
    const StoreShape& shape, llvm::Value* write_ptr,
    llvm::Value* notify_base_ptr, uint32_t signal_id) -> Result<void> {
  auto& builder = context.GetBuilder();

  auto raw_or_err = LowerRhsRaw(context, deferred.rhs, deferred.dest);
  if (!raw_or_err) return std::unexpected(raw_or_err.error());

  llvm::Value* source_value = CoerceValueToShape(context, *raw_or_err, shape);
  llvm::Constant* mask_const = BuildAllOnesMask(context, shape);

  // Store value to alloca for runtime call
  auto* val_alloca = builder.CreateAlloca(shape.storage_ty, nullptr, "nba.val");
  builder.CreateStore(source_value, val_alloca);

  // Mask alloca: for aggregates, type is [byte_size x i8]; for scalars, same
  // as storage_ty
  llvm::Type* mask_ty = (shape.kind == StoreKind::kAggregateBytes)
                            ? mask_const->getType()
                            : shape.storage_ty;
  auto* mask_alloca = builder.CreateAlloca(mask_ty, nullptr, "nba.mask");
  builder.CreateStore(mask_const, mask_alloca);

  // Compute byte_size
  uint32_t byte_size =
      (shape.kind == StoreKind::kAggregateBytes)
          ? shape.byte_size
          : static_cast<uint32_t>(
                context.GetModule().getDataLayout().getTypeStoreSize(
                    shape.storage_ty));

  auto& llvm_ctx = context.GetLlvmContext();
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  builder.CreateCall(
      context.GetLyraScheduleNba(),
      {context.GetEnginePointer(), write_ptr, notify_base_ptr, val_alloca,
       mask_alloca, llvm::ConstantInt::get(i32_ty, byte_size),
       llvm::ConstantInt::get(i32_ty, signal_id)});

  return {};
}

// BitRangeProjection NBA: partial bit-range writes with shifted value/mask.
// Kept separate because it uses partial masks (not full-width).
auto LowerDeferredAssignBitRange(
    Context& context, const mir::DeferredAssign& deferred, uint32_t signal_id)
    -> Result<void> {
  auto& builder = context.GetBuilder();
  const auto& arena = context.GetMirArena();
  const auto& place = arena[deferred.dest];

  auto br_result = context.ComposeBitRange(deferred.dest);
  if (!br_result) return std::unexpected(br_result.error());
  auto [offset, width] = *br_result;

  auto ptr_or_err = context.GetPlacePointer(deferred.dest);
  if (!ptr_or_err) return std::unexpected(ptr_or_err.error());
  llvm::Value* ptr = *ptr_or_err;

  auto base_type_result = context.GetPlaceBaseType(deferred.dest);
  if (!base_type_result) return std::unexpected(base_type_result.error());
  llvm::Type* base_type = *base_type_result;

  // INVARIANT: BitRange is only valid on scalar bases
  if (!base_type->isIntegerTy() && !IsFourStateScalarStruct(base_type)) {
    throw common::InternalError(
        "LowerDeferredAssignBitRange", "BitRange on non-scalar base type");
  }

  llvm::Value* notify_base_ptr = GetDesignRootPointer(context, deferred.dest);

  if (IsFourStateScalarStruct(base_type)) {
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
    src_val = builder.CreateZExtOrTrunc(src_val, plane_type, "nba.src.val.ext");
    src_unk = builder.CreateZExtOrTrunc(src_unk, plane_type, "nba.src.unk.ext");
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

// IndexProjection NBA: array element write with OOB guard.
// Uses StoreShape classification and EmitDeferredStoreCore.
auto LowerDeferredAssignWithOobGuard(
    Context& context, const mir::DeferredAssign& deferred,
    const StoreShape& shape, uint32_t signal_id) -> Result<void> {
  auto& builder = context.GetBuilder();
  auto& llvm_ctx = context.GetLlvmContext();
  const auto& arena = context.GetMirArena();
  const auto& types = context.GetTypeArena();
  const auto& place = arena[deferred.dest];

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
  auto write_ptr_or_err = context.GetPlacePointer(deferred.dest);
  if (!write_ptr_or_err) return std::unexpected(write_ptr_or_err.error());
  llvm::Value* write_ptr = *write_ptr_or_err;
  llvm::Value* notify_base_ptr = GetDesignRootPointer(context, deferred.dest);

  auto result = EmitDeferredStoreCore(
      context, deferred, shape, write_ptr, notify_base_ptr, signal_id);
  if (!result) return result;
  builder.CreateBr(skip_bb);

  builder.SetInsertPoint(skip_bb);
  return {};
}

// Direct NBA: simple full-width write (no projections needing special
// handling). Uses StoreShape classification and EmitDeferredStoreCore.
auto LowerDeferredAssignDirect(
    Context& context, const mir::DeferredAssign& deferred,
    const StoreShape& shape, uint32_t signal_id) -> Result<void> {
  auto write_ptr_or_err = context.GetPlacePointer(deferred.dest);
  if (!write_ptr_or_err) return std::unexpected(write_ptr_or_err.error());
  llvm::Value* write_ptr = *write_ptr_or_err;
  llvm::Value* notify_base_ptr = GetDesignRootPointer(context, deferred.dest);

  return EmitDeferredStoreCore(
      context, deferred, shape, write_ptr, notify_base_ptr, signal_id);
}

}  // namespace

auto LowerDeferredAssign(Context& context, const mir::DeferredAssign& deferred)
    -> Result<void> {
  const auto& arena = context.GetMirArena();

  // Use canonical signal_id (after alias resolution) for notification
  // NBA is only valid for design places (GetSignalIdForNba throws if not)
  uint32_t signal_id = GetSignalIdForNba(context, deferred.dest);

  // Case 1: BitRangeProjection - partial bit-range writes (keep separate)
  if (context.HasBitRangeProjection(deferred.dest)) {
    return LowerDeferredAssignBitRange(context, deferred, signal_id);
  }

  // Classify destination once via MIR type
  auto shape_or_err = ClassifyDeferredStore(context, deferred.dest);
  if (!shape_or_err) return std::unexpected(shape_or_err.error());
  StoreShape shape = *shape_or_err;

  // Case 2: IndexProjection - array element write with OOB guard
  if (HasIndexProjection(arena[deferred.dest])) {
    return LowerDeferredAssignWithOobGuard(context, deferred, shape, signal_id);
  }

  // Case 3: Simple full-width write
  return LowerDeferredAssignDirect(context, deferred, shape, signal_id);
}

}  // namespace lyra::lowering::mir_to_llvm
