#include "lyra/llvm_backend/packed_storage_view.hpp"

#include <cstdint>
#include <expected>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/llvm_backend/layout/union_storage.hpp"
#include "lyra/llvm_backend/storage_boundary.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

auto PackedStorageView::RequireLocalBackingType() const -> llvm::Type* {
  if (backing_.is_canonical || backing_.local_llvm_type == nullptr) {
    throw common::InternalError(
        "PackedStorageView::RequireLocalBackingType",
        "local backing type requested for canonical or unset backing");
  }
  return backing_.local_llvm_type;
}

void PackedStorageView::SetCanonicalBacking() {
  backing_ = BackingInfo{.is_canonical = true, .local_llvm_type = nullptr};
}

void PackedStorageView::SetLocalBacking(llvm::Type* local_llvm_type) {
  if (local_llvm_type == nullptr) {
    throw common::InternalError(
        "PackedStorageView::SetLocalBacking",
        "non-canonical backing requires local backing type");
  }
  backing_ =
      BackingInfo{.is_canonical = false, .local_llvm_type = local_llvm_type};
}

// BitRangeProjection byte-alignment proof.
//
// Uses MIR-owned guaranteed_alignment_bits metadata set at lowering time.
// For constant offsets, also checks the actual value as a backstop.
auto IsBitRangeStepProvablyByteAligned(const mir::BitRangeProjection& br)
    -> bool {
  if (br.width % 8 != 0) return false;

  // MIR-owned alignment: set at lowering time from semantic analysis.
  if (br.guaranteed_alignment_bits >= 8) return true;

  // Constant offset backstop: check the actual value.
  if (const auto* constant = std::get_if<Constant>(&br.bit_offset.payload)) {
    const auto* integral = std::get_if<IntegralConstant>(&constant->value);
    if (integral == nullptr || integral->value.empty()) return false;
    return (integral->value[0] % 8) == 0;
  }

  return false;
}

// Resolve the packed storage root for a place with BitRangeProjection.
//
// Explicitly encapsulates the contract that GetPlacePointer and
// TypeOfPlaceBase both stop at the BitRangeProjection boundary, yielding
// the storage root pointer and type for the packed object that contains
// the accessed subobject.
//
// This helper exists so that the implicit "skip BitRange" behavior of
// Context::GetPlacePointer is not relied on silently throughout the module.
auto ResolvePackedStorageRoot(Context& ctx, mir::PlaceId place_id)
    -> Result<PackedStorageView> {
  const auto& arena = ctx.GetMirArena();
  const auto& types = ctx.GetTypeArena();
  const auto& place = arena[place_id];

  // TypeOfPlaceBase skips BitRangeProjection suffix.
  TypeId base_type_id = mir::TypeOfPlaceBase(types, place);
  const Type& base_type = types[base_type_id];
  uint32_t total_bits = PackedBitWidth(base_type, types);
  bool is_four_state = ctx.IsPackedFourState(base_type);
  uint32_t storage_plane_byte_size = GetStorageByteSize(total_bits);

  // GetPlacePointer skips BitRangeProjection suffix.
  auto ptr_result = ctx.GetPlacePointer(place_id);
  if (!ptr_result) return std::unexpected(ptr_result.error());

  bool is_canonical = place.root.kind == mir::PlaceRoot::Kind::kModuleSlot ||
                      place.root.kind == mir::PlaceRoot::Kind::kDesignGlobal;

  PackedStorageView view;
  view.base_ptr = *ptr_result;
  view.total_semantic_bits = total_bits;
  view.storage_plane_byte_size = storage_plane_byte_size;
  view.unk_plane_offset_bytes =
      is_four_state ? FourStateUnknownLaneOffset(total_bits) : 0;
  view.is_four_state = is_four_state;

  if (!is_canonical) {
    auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, base_type_id);
    if (!llvm_type_result) return std::unexpected(llvm_type_result.error());
    view.SetLocalBacking(*llvm_type_result);
  } else {
    view.SetCanonicalBacking();
  }

  return view;
}

auto ExtractPackedAccessPath(Context& ctx, mir::PlaceId place_id)
    -> Result<PackedAccessPath> {
  const auto& arena = ctx.GetMirArena();
  const auto& place = arena[place_id];

  auto storage_result = ResolvePackedStorageRoot(ctx, place_id);
  if (!storage_result) return std::unexpected(storage_result.error());

  PackedAccessPath path;
  path.storage = *storage_result;
  path.result_type = place.root.type;

  bool seen_bitrange = false;
  TypeId current_type = place.root.type;

  for (const auto& proj : place.projections) {
    if (const auto* br = std::get_if<mir::BitRangeProjection>(&proj.info)) {
      seen_bitrange = true;

      // Invariant check: guaranteed_alignment_bits must be a nonzero
      // power of two. Catches producer drift immediately.
      if (br->guaranteed_alignment_bits == 0 ||
          (br->guaranteed_alignment_bits &
           (br->guaranteed_alignment_bits - 1)) != 0) {
        throw common::InternalError(
            "ExtractPackedAccessPath",
            "guaranteed_alignment_bits must be a nonzero power of two");
      }

      PackedProjectionStep step;
      step.semantic_bits = br->width;
      step.result_type = br->element_type;
      step.is_provably_byte_aligned = IsBitRangeStepProvablyByteAligned(*br);

      auto offset_result = LowerOperand(ctx, br->bit_offset);
      if (!offset_result) return std::unexpected(offset_result.error());
      step.dynamic_bit_offset = *offset_result;

      path.steps.push_back(step);
      current_type = br->element_type;
    }
  }

  if (!seen_bitrange) {
    throw common::InternalError(
        "ExtractPackedAccessPath",
        "called on place with no BitRangeProjection");
  }

  path.result_type = current_type;
  return path;
}

auto ResolvePackedSubview(Context& ctx, const PackedAccessPath& path)
    -> Result<PackedSubviewAccess> {
  auto& builder = ctx.GetBuilder();
  auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());

  llvm::Value* total_bit_offset = nullptr;
  uint32_t final_width = 0;
  TypeId final_type;
  bool all_steps_byte_aligned = true;

  for (const auto& step : path.steps) {
    // Materialize the step's bit offset as an LLVM i32 value.
    llvm::Value* step_offset = step.dynamic_bit_offset;
    if (step_offset == nullptr) {
      step_offset = llvm::ConstantInt::get(i32_ty, step.static_bit_offset);
    } else {
      step_offset = builder.CreateZExtOrTrunc(step_offset, i32_ty);
    }

    if (total_bit_offset == nullptr) {
      total_bit_offset = step_offset;
    } else {
      total_bit_offset = builder.CreateAdd(total_bit_offset, step_offset);
    }
    final_width = step.semantic_bits;
    final_type = step.result_type;

    if (!step.is_provably_byte_aligned) {
      all_steps_byte_aligned = false;
    }
  }

  if (total_bit_offset == nullptr) {
    throw common::InternalError("ResolvePackedSubview", "empty access path");
  }

  PackedSubviewAccess access;
  access.storage = path.storage;
  access.semantic_bit_offset = total_bit_offset;
  access.semantic_bit_width = final_width;
  access.result_type = final_type;

  // Byte-addressable requires ALL of:
  // 1. Canonical storage (design slots). Non-canonical local storage uses
  //    LLVM struct alignment which may diverge from canonical byte offsets.
  // 2. The subview width is a multiple of 8.
  // 3. The composed bit offset is provably a multiple of 8 at all runtime
  //    values. Currently proven only by BitRangeProjection-specific
  //    analysis (IsBitRangeStepProvablyByteAligned). Future packed struct
  //    field and part-select steps will need their own proofs.
  if (path.storage.IsCanonicalBacking() && final_width % 8 == 0 &&
      all_steps_byte_aligned) {
    access.kind = PackedSubviewKind::kByteAddressable;
    access.byte_offset =
        builder.CreateLShr(total_bit_offset, 3, "psv.byte_off");
    // Use exact byte span (ceil(width/8)), not standalone allocation size
    // (GetStorageByteSize which rounds up to power-of-two). Within a packed
    // value, elements are packed at bit boundaries and span exactly
    // ceil(width/8) bytes. Using the rounded-up size would read/write past
    // the plane boundary for non-power-of-two widths at the tail.
    access.subview_byte_span = (final_width + 7) / 8;
  } else {
    access.kind = PackedSubviewKind::kBitAddressable;
  }

  return access;
}

namespace {

// Load a full-width packed plane chunk from canonical storage via explicit
// i8 GEP. Only valid for canonical storage.
auto EmitTypedPlaneLoad(
    llvm::IRBuilder<>& builder, llvm::LLVMContext& llvm_ctx,
    llvm::Value* base_ptr, uint64_t byte_offset, uint32_t storage_bits,
    const char* name) -> llvm::Value* {
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* lane_ty = llvm::IntegerType::get(llvm_ctx, storage_bits);
  auto* ptr =
      builder.CreateGEP(i8_ty, base_ptr, builder.getInt64(byte_offset), name);
  return builder.CreateLoad(lane_ty, ptr);
}

// Write a runtime-computed SSA plane value to canonical storage via i8 GEP.
// Runtime plane SSA only -- rejects constants. Used by StoreFullPlanes for
// canonical runtime writeback paths such as bit-addressable RMW.
// Constants must use EmitCanonicalPlaneWrite.
void EmitRuntimePlaneWriteback(
    llvm::IRBuilderBase& builder, llvm::Value* dest_ptr, uint64_t byte_offset,
    llvm::Value* plane_value, const char* name) {
  if (llvm::isa<llvm::Constant>(plane_value)) {
    throw common::InternalError(
        "EmitRuntimePlaneWriteback",
        "constant operands must use EmitCanonicalPlaneWrite, "
        "not the runtime writeback path");
  }
  auto* i8_ty = llvm::Type::getInt8Ty(builder.getContext());
  auto* ptr =
      builder.CreateGEP(i8_ty, dest_ptr, builder.getInt64(byte_offset), name);
  builder.CreateStore(plane_value, ptr);
}

// Load plane values from packed storage, normalized to canonical lane width.
// Canonical: byte-addressed GEP (already at lane width).
// Non-canonical: LLVM typed load + normalize from backing width to lane width.
// Always returns LanePlaneValues where both planes are at canonical lane width.
auto LoadLanePlanes(Context& ctx, const PackedStorageView& storage)
    -> LanePlaneValues {
  auto& builder = ctx.GetBuilder();
  auto lane_bits =
      GetCanonicalLaneBits(SemanticBits::FromRaw(storage.total_semantic_bits));

  if (storage.IsCanonicalBacking()) {
    auto val = LaneValue::FromNormalized(
        EmitTypedPlaneLoad(
            builder, ctx.GetLlvmContext(), storage.base_ptr, 0, lane_bits.Raw(),
            "fp.val.ptr"),
        lane_bits);
    std::optional<LaneValue> unk;
    if (storage.is_four_state) {
      unk = LaneValue::FromNormalized(
          EmitTypedPlaneLoad(
              builder, ctx.GetLlvmContext(), storage.base_ptr,
              storage.unk_plane_offset_bytes, lane_bits.Raw(), "fp.unk.ptr"),
          lane_bits);
    }
    return LanePlaneValues::Make(val, unk);
  }

  // Non-canonical: LLVM typed load, then normalize to lane width.
  auto* loaded = builder.CreateLoad(
      storage.RequireLocalBackingType(), storage.base_ptr, "fp.local");
  if (storage.is_four_state) {
    auto* st =
        llvm::dyn_cast<llvm::StructType>(storage.RequireLocalBackingType());
    if (st == nullptr || st->getNumElements() != 2 ||
        st->getElementType(0) != st->getElementType(1) ||
        !st->getElementType(0)->isIntegerTy()) {
      throw common::InternalError(
          "LoadLanePlanes",
          "non-canonical 4-state local storage must be a struct with "
          "exactly 2 identical integer elements");
    }
    auto* raw_val = builder.CreateExtractValue(loaded, 0, "fp.val");
    auto* raw_unk = builder.CreateExtractValue(loaded, 1, "fp.unk");
    auto val = NormalizeToLaneWidth(builder, raw_val, lane_bits);
    auto unk = NormalizeToLaneWidth(builder, raw_unk, lane_bits);
    return LanePlaneValues::Make(val, unk);
  }

  auto val = NormalizeToLaneWidth(builder, loaded, lane_bits);
  return LanePlaneValues::Make(val, std::nullopt);
}

// Store lane-domain plane values to packed storage.
// Canonical: store directly (already at correct width).
// Non-canonical: convert from lane width back to backing element width.
void StoreLanePlanes(
    Context& ctx, const PackedStorageView& storage,
    const LanePlaneValues& planes) {
  auto& builder = ctx.GetBuilder();
  auto expected_lane_bits =
      GetCanonicalLaneBits(SemanticBits::FromRaw(storage.total_semantic_bits));

  if (planes.Val().Bits() != expected_lane_bits) {
    throw common::InternalError(
        "StoreLanePlanes",
        std::format(
            "val lane bits {} does not match storage lane bits {}",
            planes.Val().Bits().Raw(), expected_lane_bits.Raw()));
  }

  if (storage.is_four_state != planes.Unk().has_value()) {
    throw common::InternalError(
        "StoreLanePlanes",
        std::format(
            "plane presence mismatch: storage is_four_state={}, unk present={}",
            storage.is_four_state, planes.Unk().has_value()));
  }

  if (planes.Unk().has_value() && planes.Unk()->Bits() != expected_lane_bits) {
    throw common::InternalError(
        "StoreLanePlanes",
        std::format(
            "unk lane bits {} does not match storage lane bits {}",
            planes.Unk()->Bits().Raw(), expected_lane_bits.Raw()));
  }

  if (storage.IsCanonicalBacking()) {
    EmitRuntimePlaneWriteback(
        builder, storage.base_ptr, 0, planes.Val().Raw(), "fp.val.store");
    if (storage.is_four_state) {
      EmitRuntimePlaneWriteback(
          builder, storage.base_ptr, storage.unk_plane_offset_bytes,
          planes.Unk()->Raw(), "fp.unk.store");
    }
    return;
  }

  // Non-canonical: convert lane-width values back to backing element width.
  if (storage.is_four_state) {
    auto* st =
        llvm::dyn_cast<llvm::StructType>(storage.RequireLocalBackingType());
    if (st == nullptr || st->getNumElements() != 2 ||
        st->getElementType(0) != st->getElementType(1) ||
        !st->getElementType(0)->isIntegerTy()) {
      throw common::InternalError(
          "StoreLanePlanes",
          "non-canonical 4-state local storage must be a struct with "
          "exactly 2 identical integer elements");
    }
    auto* elem_ty = st->getElementType(0);
    auto* store_val = ConvertLaneToBackingWidth(builder, planes.Val(), elem_ty);
    auto* store_unk =
        ConvertLaneToBackingWidth(builder, *planes.Unk(), elem_ty);
    llvm::Value* packed =
        llvm::UndefValue::get(storage.RequireLocalBackingType());
    packed = builder.CreateInsertValue(packed, store_val, 0);
    packed = builder.CreateInsertValue(packed, store_unk, 1);
    builder.CreateStore(packed, storage.base_ptr);
    return;
  }

  // Non-canonical 2-state: scalar store.
  auto* store_val = ConvertLaneToBackingWidth(
      builder, planes.Val(), storage.RequireLocalBackingType());
  builder.CreateStore(store_val, storage.base_ptr);
}

auto EmitByteAddressableLoad(Context& ctx, const PackedSubviewAccess& access)
    -> PackedRValue {
  auto& builder = ctx.GetBuilder();
  auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
  uint32_t storage_bits = access.subview_byte_span * 8;
  auto* load_ty = llvm::IntegerType::get(ctx.GetLlvmContext(), storage_bits);

  auto* val_ptr = builder.CreateGEP(
      i8_ty, access.storage.base_ptr, access.byte_offset, "psv.val.ptr");
  llvm::Value* val = builder.CreateLoad(load_ty, val_ptr, "psv.val");

  if (storage_bits > access.semantic_bit_width) {
    auto mask =
        llvm::APInt::getLowBitsSet(storage_bits, access.semantic_bit_width);
    val = builder.CreateAnd(
        val, llvm::ConstantInt::get(load_ty, mask), "psv.val.mask");
  }

  PackedRValue result;
  result.val = val;
  result.semantic_bits = access.semantic_bit_width;

  if (access.storage.is_four_state) {
    auto* unk_off = builder.CreateAdd(
        access.byte_offset,
        llvm::ConstantInt::get(
            access.byte_offset->getType(),
            access.storage.unk_plane_offset_bytes),
        "psv.unk.off");
    auto* unk_ptr = builder.CreateGEP(
        i8_ty, access.storage.base_ptr, unk_off, "psv.unk.ptr");
    llvm::Value* unk = builder.CreateLoad(load_ty, unk_ptr, "psv.unk");

    if (storage_bits > access.semantic_bit_width) {
      auto mask =
          llvm::APInt::getLowBitsSet(storage_bits, access.semantic_bit_width);
      unk = builder.CreateAnd(
          unk, llvm::ConstantInt::get(load_ty, mask), "psv.unk.mask");
    }
    result.unk = unk;
  }

  return result;
}

// Bit-addressable load: load full base value, shift right, mask/truncate.
// Operates in lane domain via LoadLanePlanes.
auto EmitBitAddressableLoad(Context& ctx, const PackedSubviewAccess& access)
    -> PackedRValue {
  auto& builder = ctx.GetBuilder();

  auto planes = LoadLanePlanes(ctx, access.storage);
  auto lane_bits = planes.Val().Bits();
  auto* lane_ty = GetLaneIntType(ctx.GetLlvmContext(), lane_bits);

  PackedRValue result;
  result.semantic_bits = access.semantic_bit_width;

  uint32_t elem_storage_bytes = GetStorageByteSize(access.semantic_bit_width);
  uint32_t elem_storage_bits = elem_storage_bytes * 8;
  auto* shift_amt = builder.CreateZExtOrTrunc(
      access.semantic_bit_offset, lane_ty, "br.offset");

  // Value plane: shift + truncate + mask
  llvm::Value* val =
      builder.CreateLShr(planes.Val().Raw(), shift_amt, "br.val.shr");
  if (elem_storage_bits < lane_bits.Raw()) {
    auto* elem_ty =
        llvm::IntegerType::get(ctx.GetLlvmContext(), elem_storage_bits);
    val = builder.CreateTrunc(val, elem_ty, "br.val.trunc");
  }
  if (access.semantic_bit_width < elem_storage_bits) {
    auto mask = llvm::APInt::getLowBitsSet(
        elem_storage_bits, access.semantic_bit_width);
    val = builder.CreateAnd(
        val,
        llvm::ConstantInt::get(
            llvm::IntegerType::get(ctx.GetLlvmContext(), elem_storage_bits),
            mask),
        "br.val.mask");
  }
  result.val = val;

  // Unknown plane: same shift + truncate + mask
  if (access.storage.is_four_state && planes.Unk().has_value()) {
    llvm::Value* unk =
        builder.CreateLShr(planes.Unk()->Raw(), shift_amt, "br.unk.shr");
    if (elem_storage_bits < lane_bits.Raw()) {
      auto* elem_ty =
          llvm::IntegerType::get(ctx.GetLlvmContext(), elem_storage_bits);
      unk = builder.CreateTrunc(unk, elem_ty, "br.unk.trunc");
    }
    if (access.semantic_bit_width < elem_storage_bits) {
      auto mask = llvm::APInt::getLowBitsSet(
          elem_storage_bits, access.semantic_bit_width);
      unk = builder.CreateAnd(
          unk,
          llvm::ConstantInt::get(
              llvm::IntegerType::get(ctx.GetLlvmContext(), elem_storage_bits),
              mask),
          "br.unk.mask");
    }
    result.unk = unk;
  }

  return result;
}

// Byte-addressable store: narrow GEP + narrow load/store/compare per plane.
// Returns single semantic changed predicate (i1).
// Invariant: only valid for canonical storage.
auto EmitByteAddressableStore(
    Context& ctx, const PackedSubviewAccess& access, const PackedRValue& value,
    const PackedStorePlan& plan) -> llvm::Value* {
  if (!access.storage.IsCanonicalBacking()) {
    throw common::InternalError(
        "EmitByteAddressableStore",
        "byte-addressable store requires canonical storage");
  }

  auto& builder = ctx.GetBuilder();
  auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
  uint32_t storage_bits = access.subview_byte_span * 8;
  auto* store_ty = llvm::IntegerType::get(ctx.GetLlvmContext(), storage_bits);

  // Value plane: store unconditionally, compare only when needed.
  auto* val_ptr = builder.CreateGEP(
      i8_ty, access.storage.base_ptr, access.byte_offset, "psw.val.ptr");

  llvm::Value* new_val =
      builder.CreateZExtOrTrunc(value.val, store_ty, "psw.val.new");
  if (storage_bits > access.semantic_bit_width) {
    auto mask =
        llvm::APInt::getLowBitsSet(storage_bits, access.semantic_bit_width);
    new_val = builder.CreateAnd(
        new_val, llvm::ConstantInt::get(store_ty, mask), "psw.val.fit");
  }

  llvm::Value* changed = nullptr;
  if (plan.needs_changed) {
    auto* old_val = builder.CreateLoad(store_ty, val_ptr, "psw.val.old");
    builder.CreateStore(new_val, val_ptr);
    changed = builder.CreateICmpNE(old_val, new_val, "psw.val.changed");
  } else {
    builder.CreateStore(new_val, val_ptr);
  }

  // Unknown plane: consume PackedStorePlan mechanically.
  // Storage mutation follows unk_lowering. Change-detection follows
  // needs_changed. These are orthogonal dimensions.
  switch (plan.unk_lowering) {
    case UnknownPlaneLowering::kNone:
      break;

    case UnknownPlaneLowering::kWritePlaneAtOffset: {
      auto* unk_off = builder.CreateAdd(
          access.byte_offset,
          llvm::ConstantInt::get(
              access.byte_offset->getType(),
              access.storage.unk_plane_offset_bytes),
          "psw.unk.off");
      auto* unk_ptr = builder.CreateGEP(
          i8_ty, access.storage.base_ptr, unk_off, "psw.unk.ptr");

      auto* new_unk =
          builder.CreateZExtOrTrunc(plan.unk_value, store_ty, "psw.unk.new");
      if (storage_bits > access.semantic_bit_width) {
        auto mask =
            llvm::APInt::getLowBitsSet(storage_bits, access.semantic_bit_width);
        new_unk = builder.CreateAnd(
            new_unk, llvm::ConstantInt::get(store_ty, mask), "psw.unk.fit");
      }

      if (plan.needs_changed) {
        auto* old_unk = builder.CreateLoad(store_ty, unk_ptr, "psw.unk.old");
        builder.CreateStore(new_unk, unk_ptr);
        auto* unk_changed =
            builder.CreateICmpNE(old_unk, new_unk, "psw.unk.changed");
        changed = builder.CreateOr(changed, unk_changed, "psw.changed");
      } else {
        builder.CreateStore(new_unk, unk_ptr);
      }
      break;
    }

    case UnknownPlaneLowering::kConditionalClearAtOffset: {
      auto* unk_off = builder.CreateAdd(
          access.byte_offset,
          llvm::ConstantInt::get(
              access.byte_offset->getType(),
              access.storage.unk_plane_offset_bytes),
          "psw.unk.off");
      auto* unk_ptr = builder.CreateGEP(
          i8_ty, access.storage.base_ptr, unk_off, "psw.unk.ptr");
      auto* old_unk = builder.CreateLoad(store_ty, unk_ptr, "psw.unk.old");
      auto* zero = llvm::ConstantInt::get(store_ty, 0);
      auto* unk_nonzero = builder.CreateICmpNE(old_unk, zero, "psw.unk.dirty");

      auto* func = builder.GetInsertBlock()->getParent();
      auto* clear_bb =
          llvm::BasicBlock::Create(ctx.GetLlvmContext(), "psw.unk.clear", func);
      auto* merge_bb =
          llvm::BasicBlock::Create(ctx.GetLlvmContext(), "psw.unk.done", func);

      builder.CreateCondBr(unk_nonzero, clear_bb, merge_bb);

      builder.SetInsertPoint(clear_bb);
      builder.CreateStore(zero, unk_ptr);
      builder.CreateBr(merge_bb);

      builder.SetInsertPoint(merge_bb);
      if (plan.needs_changed) {
        changed = builder.CreateOr(changed, unk_nonzero, "psw.changed");
      }
      break;
    }

    case UnknownPlaneLowering::kUnconditionalClearAtOffset: {
      auto* unk_off = builder.CreateAdd(
          access.byte_offset,
          llvm::ConstantInt::get(
              access.byte_offset->getType(),
              access.storage.unk_plane_offset_bytes),
          "psw.unk.off");
      auto* unk_ptr = builder.CreateGEP(
          i8_ty, access.storage.base_ptr, unk_off, "psw.unk.ptr");
      auto* zero = llvm::ConstantInt::get(store_ty, 0);
      builder.CreateStore(zero, unk_ptr);
      break;
    }

    default:
      throw common::InternalError(
          "EmitByteAddressableStore",
          "invalid unk_lowering for byte-addressable path");
  }

  return changed;
}

// Bit-addressable store: full-width RMW via LoadLanePlanes/StoreLanePlanes.
// Operates entirely in lane domain. Returns changed predicate (i1).
auto EmitBitAddressableStore(
    Context& ctx, const PackedSubviewAccess& access, const PackedRValue& value,
    const PackedStorePlan& plan) -> llvm::Value* {
  auto& builder = ctx.GetBuilder();

  auto old_planes = LoadLanePlanes(ctx, access.storage);
  auto lane_bits = old_planes.Val().Bits();
  auto* lane_ty = GetLaneIntType(ctx.GetLlvmContext(), lane_bits);

  auto* shift_amt = builder.CreateZExtOrTrunc(
      access.semantic_bit_offset, lane_ty, "rmw.offset");
  auto mask_ap =
      llvm::APInt::getLowBitsSet(lane_bits.Raw(), access.semantic_bit_width);
  auto* mask = llvm::ConstantInt::get(lane_ty, mask_ap);
  auto* mask_shifted = builder.CreateShl(mask, shift_amt, "rmw.mask");
  auto* not_mask = builder.CreateNot(mask_shifted, "rmw.notmask");

  // Value plane RMW
  auto* src_val = builder.CreateZExtOrTrunc(value.val, lane_ty, "rmw.src.val");
  auto* val_shifted = builder.CreateShl(src_val, shift_amt, "rmw.val.shl");
  auto* val_cleared =
      builder.CreateAnd(old_planes.Val().Raw(), not_mask, "rmw.val.clear");
  auto* val_result = builder.CreateOr(val_cleared, val_shifted, "rmw.val");

  llvm::Value* changed = nullptr;
  if (plan.needs_changed) {
    changed = builder.CreateICmpNE(
        old_planes.Val().Raw(), val_result, "rmw.val.changed");
  }

  // Unknown plane RMW: consume PackedStorePlan mechanically.
  std::optional<LaneValue> new_unk;
  switch (plan.unk_lowering) {
    case UnknownPlaneLowering::kNone:
      break;

    case UnknownPlaneLowering::kMergePlaneBitsRmw: {
      auto* src_unk =
          builder.CreateZExtOrTrunc(plan.unk_value, lane_ty, "rmw.src.unk");
      auto* unk_shifted = builder.CreateShl(src_unk, shift_amt, "rmw.unk.shl");
      auto* unk_cleared =
          builder.CreateAnd(old_planes.Unk()->Raw(), not_mask, "rmw.unk.clear");
      auto* unk_result = builder.CreateOr(unk_cleared, unk_shifted, "rmw.unk");
      new_unk = LaneValue::FromNormalized(unk_result, lane_bits);

      if (plan.needs_changed) {
        auto* unk_changed = builder.CreateICmpNE(
            old_planes.Unk()->Raw(), unk_result, "rmw.unk.changed");
        changed = builder.CreateOr(changed, unk_changed, "rmw.changed");
      }
      break;
    }

    case UnknownPlaneLowering::kMaskedClearBitsRmw: {
      auto* unk_result =
          builder.CreateAnd(old_planes.Unk()->Raw(), not_mask, "rmw.unk.clear");
      new_unk = LaneValue::FromNormalized(unk_result, lane_bits);

      if (plan.needs_changed) {
        auto* old_unk_bits = builder.CreateAnd(
            old_planes.Unk()->Raw(), mask_shifted, "rmw.unk.oldbits");
        auto* unk_nonzero = builder.CreateICmpNE(
            old_unk_bits, llvm::ConstantInt::get(lane_ty, 0), "rmw.unk.dirty");
        changed = builder.CreateOr(changed, unk_nonzero, "rmw.changed");
      }
      break;
    }

    default:
      throw common::InternalError(
          "EmitBitAddressableStore", "invalid unk_lowering for RMW path");
  }

  auto new_val = LaneValue::FromNormalized(val_result, lane_bits);
  StoreLanePlanes(ctx, access.storage, LanePlaneValues::Make(new_val, new_unk));
  return changed;
}

}  // namespace

auto EmitLoadFromPackedSubview(Context& ctx, const PackedSubviewAccess& access)
    -> Result<PackedRValue> {
  if (access.kind == PackedSubviewKind::kByteAddressable) {
    return EmitByteAddressableLoad(ctx, access);
  }
  return EmitBitAddressableLoad(ctx, access);
}

auto GetSubviewDirtyRange(Context& ctx, const PackedSubviewAccess& access)
    -> PackedDirtyRange {
  auto* i32_ty = llvm::Type::getInt32Ty(ctx.GetLlvmContext());
  if (access.kind == PackedSubviewKind::kByteAddressable) {
    return PackedDirtyRange{
        .byte_offset = ctx.GetBuilder().CreateZExtOrTrunc(
            access.byte_offset, i32_ty, "dirty.off"),
        .byte_size = llvm::ConstantInt::get(i32_ty, access.subview_byte_span),
    };
  }
  return PackedDirtyRange{
      .byte_offset = llvm::ConstantInt::get(i32_ty, 0),
      .byte_size = llvm::ConstantInt::get(i32_ty, 0),
  };
}

void EmitPackedStoreNotification(
    Context& ctx, llvm::Value* changed, const PackedStorePolicy& policy,
    const PackedDirtyRange& dirty_range) {
  // No notification for init, non-design, or deferred.
  if (policy.store_mode == PackedStoreMode::kDirectInit) return;
  if (!policy.signal_id.has_value()) return;
  if (policy.notification_deferred) return;

  // When static contract allows suppression, compute a runtime trace
  // guard that will be ANDed into the should_mark condition below.
  // If static contract requires propagation, trace_guard is nullptr
  // (meaning unconditional propagation).
  llvm::Value* trace_guard = nullptr;
  if (!policy.requires_static_dirty_propagation &&
      policy.mutation_owner_slot.has_value()) {
    trace_guard = ctx.EmitIsTraceObservedOwnerSlot(*policy.mutation_owner_slot);
  }

  // Constant-fold: if changed is a known false constant, skip entirely.
  if (const auto* ci = llvm::dyn_cast<llvm::ConstantInt>(changed)) {
    if (ci->isZero()) return;
  }

  // Reject null engine_ptr for all notify modes.
  if (policy.engine_ptr == nullptr) {
    throw common::InternalError(
        "EmitPackedStoreNotification", "notify store mode requires engine_ptr");
  }

  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();

  // Cross-context: guard changed with engine != null
  llvm::Value* should_mark = changed;
  if (policy.store_mode == PackedStoreMode::kNotifyCrossContext) {
    auto* engine_not_null = builder.CreateICmpNE(
        policy.engine_ptr,
        llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(llvm_ctx)),
        "engine.nonnull");
    should_mark = builder.CreateAnd(changed, engine_not_null, "should_mark");
  }

  // When static contract allows suppression, AND the runtime trace
  // guard into the mark condition. Only emit dirty mark if the value
  // changed AND trace is observing this slot.
  if (trace_guard != nullptr) {
    should_mark =
        builder.CreateAnd(should_mark, trace_guard, "should_mark.traced");
  }

  auto* fn = builder.GetInsertBlock()->getParent();
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  auto* done_bb = llvm::BasicBlock::Create(llvm_ctx, "psw.done", fn);

  // Check if this is a full-slot dirty mark eligible for first-dirty fast path.
  bool is_full_slot = false;
  if (const auto* off_ci =
          llvm::dyn_cast<llvm::ConstantInt>(dirty_range.byte_offset)) {
    if (const auto* sz_ci =
            llvm::dyn_cast<llvm::ConstantInt>(dirty_range.byte_size)) {
      is_full_slot = off_ci->isZero() && sz_ci->isZero();
    }
  }

  if (is_full_slot && policy.first_dirty_seen != nullptr &&
      policy.store_mode == PackedStoreMode::kNotifySimulation) {
    // Inline first-dirty fast path: check per-delta seen bitmap.
    // Common case (already dirty) skips the runtime call entirely.
    auto* null_ptr =
        llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(llvm_ctx));
    auto* check_bitmap_bb =
        llvm::BasicBlock::Create(llvm_ctx, "check_bitmap", fn);
    auto* check_seen_bb = llvm::BasicBlock::Create(llvm_ctx, "check_seen", fn);
    auto* mark_first_bb = llvm::BasicBlock::Create(llvm_ctx, "mark_first", fn);
    auto* fallback_bb =
        llvm::BasicBlock::Create(llvm_ctx, "mark_dirty_fallback", fn);

    builder.CreateCondBr(should_mark, check_bitmap_bb, done_bb);

    builder.SetInsertPoint(check_bitmap_bb);
    auto* slot_id = policy.signal_id->Emit(builder);
    auto* bitmap_nonnull = builder.CreateICmpNE(
        policy.first_dirty_seen, null_ptr, "bitmap.nonnull");
    builder.CreateCondBr(bitmap_nonnull, check_seen_bb, fallback_bb);

    builder.SetInsertPoint(check_seen_bb);
    auto* seen_ptr = builder.CreateGEP(
        i8_ty, policy.first_dirty_seen, {slot_id}, "seen_ptr");
    auto* seen = builder.CreateLoad(i8_ty, seen_ptr, "seen");
    auto* already_dirty = builder.CreateICmpNE(
        seen, llvm::ConstantInt::get(i8_ty, 0), "already_dirty");
    builder.CreateCondBr(already_dirty, done_bb, mark_first_bb);

    builder.SetInsertPoint(mark_first_bb);
    builder.CreateCall(
        ctx.GetLyraMarkDirtyFirst(), {policy.engine_ptr, slot_id});
    builder.CreateBr(done_bb);

    builder.SetInsertPoint(fallback_bb);
    builder.CreateCall(
        ctx.GetLyraMarkDirty(),
        {policy.engine_ptr, slot_id, llvm::ConstantInt::get(i32_ty, 0),
         llvm::ConstantInt::get(i32_ty, 0)});
    builder.CreateBr(done_bb);
  } else {
    auto* dirty_bb = llvm::BasicBlock::Create(llvm_ctx, "psw.dirty", fn);
    builder.CreateCondBr(should_mark, dirty_bb, done_bb);

    builder.SetInsertPoint(dirty_bb);
    builder.CreateCall(
        ctx.GetLyraMarkDirty(),
        {policy.engine_ptr, policy.signal_id->Emit(builder),
         dirty_range.byte_offset, dirty_range.byte_size});
    builder.CreateBr(done_bb);
  }

  builder.SetInsertPoint(done_bb);
}

auto EmitStoreToPackedSubview(
    Context& ctx, const PackedSubviewAccess& access, const PackedRValue& value,
    const PackedStorePolicy& policy) -> Result<void> {
  auto recipe_ctx = ClassifySubviewRecipeContext(access.kind);
  auto plan = BuildPackedStorePlan(
      access.storage, value, recipe_ctx, policy.notification_deferred);

  llvm::Value* changed = nullptr;
  if (access.kind == PackedSubviewKind::kByteAddressable) {
    changed = EmitByteAddressableStore(ctx, access, value, plan);
  } else {
    changed = EmitBitAddressableStore(ctx, access, value, plan);
  }

  if (plan.needs_changed) {
    auto dirty_range = GetSubviewDirtyRange(ctx, access);
    EmitPackedStoreNotification(ctx, changed, policy, dirty_range);
  }
  return {};
}

// Create an entry-block alloca for temporary canonical buffer operations.
// All PSV canonical-buffer temporaries use entry-block placement for
// predictable allocation and consistent stack layout.
auto CreateEntryBlockAlloca(
    llvm::IRBuilder<>& builder, llvm::Type* ty, const char* name)
    -> llvm::AllocaInst* {
  auto* func = builder.GetInsertBlock()->getParent();
  llvm::IRBuilder<> entry_builder(
      &func->getEntryBlock(), func->getEntryBlock().begin());
  return entry_builder.CreateAlloca(ty, nullptr, name);
}

// Encode a runtime-computed plane value into a temporary byte buffer via
// typed alloca + memcpy. Runtime plane SSA only -- not the constant/pattern
// materialization boundary. Used for temporary byte-buffer encoding for
// runtime helper handoff (e.g., LyraStorePacked).
void EncodePlaneValueToByteBuffer(
    llvm::IRBuilder<>& builder, llvm::LLVMContext& llvm_ctx,
    llvm::Value* buf_ptr, uint64_t byte_offset, llvm::Value* plane_value) {
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* value_ty = plane_value->getType();
  uint64_t byte_size =
      builder.GetInsertBlock()->getModule()->getDataLayout().getTypeStoreSize(
          value_ty);
  auto* src_alloca = CreateEntryBlockAlloca(builder, value_ty, "cbuf.src");
  builder.CreateStore(plane_value, src_alloca);
  auto* dest_ptr = builder.CreateGEP(
      i8_ty, buf_ptr, builder.getInt64(byte_offset), "cbuf.dst");
  builder.CreateMemCpy(
      dest_ptr, llvm::Align(1), src_alloca, llvm::Align(1), byte_size);
}

// Decode a plane value from a temporary byte buffer via memcpy to a typed
// alloca. Mirrors EncodePlaneValueToByteBuffer. All temporary allocas use
// entry-block placement for predictable stack layout.
auto DecodePlaneValueFromByteBuffer(
    llvm::IRBuilder<>& builder, llvm::LLVMContext& llvm_ctx,
    llvm::Value* buf_ptr, uint64_t byte_offset, llvm::Type* plane_type)
    -> llvm::Value* {
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  uint64_t byte_size =
      builder.GetInsertBlock()->getModule()->getDataLayout().getTypeStoreSize(
          plane_type);
  auto* dest_alloca = CreateEntryBlockAlloca(builder, plane_type, "cbuf.rd");
  auto* src_ptr = builder.CreateGEP(
      i8_ty, buf_ptr, builder.getInt64(byte_offset), "cbuf.src");
  builder.CreateMemCpy(
      dest_alloca, llvm::Align(1), src_ptr, llvm::Align(1), byte_size);
  return builder.CreateLoad(plane_type, dest_alloca, "cbuf.val");
}

// Prepare a narrow plane value for NBA: truncate/extend and mask.
auto PrepareNarrowPlaneValue(
    llvm::IRBuilder<>& builder, llvm::LLVMContext& llvm_ctx,
    llvm::Value* raw_value, uint32_t subview_byte_span,
    uint32_t semantic_bit_width) -> llvm::Value* {
  uint32_t storage_bits = subview_byte_span * 8;
  auto* store_ty = llvm::IntegerType::get(llvm_ctx, storage_bits);
  auto* store_val = builder.CreateZExtOrTrunc(raw_value, store_ty, "nba.val");
  if (storage_bits > semantic_bit_width) {
    auto mask = llvm::APInt::getLowBitsSet(storage_bits, semantic_bit_width);
    store_val = builder.CreateAnd(
        store_val, llvm::ConstantInt::get(store_ty, mask), "nba.val.fit");
  }
  return store_val;
}

// Emit a 2-state byte-addressable narrow NBA overwrite.
void EmitNarrow2StateNbaCall(
    Context& ctx, const PackedSubviewAccess& access,
    const PackedNbaPolicy& policy, llvm::Value* plane_value) {
  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  auto* store_val = PrepareNarrowPlaneValue(
      builder, llvm_ctx, plane_value, access.subview_byte_span,
      access.semantic_bit_width);

  auto* write_ptr = builder.CreateGEP(
      i8_ty, access.storage.base_ptr, access.byte_offset, "nba.write.ptr");

  uint32_t narrow_bytes = access.subview_byte_span;
  auto* buf_ty = llvm::ArrayType::get(i8_ty, narrow_bytes);
  auto* val_alloca = builder.CreateAlloca(buf_ty, nullptr, "nba.val.a");
  EncodePlaneValueToByteBuffer(builder, llvm_ctx, val_alloca, 0, store_val);

  auto* null_ptr =
      llvm::ConstantPointerNull::get(llvm::PointerType::get(llvm_ctx, 0));
  builder.CreateCall(
      ctx.GetLyraScheduleNba(),
      {policy.engine_ptr, write_ptr, policy.notify_base_ptr, val_alloca,
       null_ptr, llvm::ConstantInt::get(i32_ty, narrow_bytes),
       policy.signal_id.Emit(builder)});
}

// Emit a 4-state byte-addressable narrow NBA using the canonical two-plane
// runtime helper. unk_payload is the resolved unknown-plane value from the
// plan (plan.unk_value for kStoreFromRhs recipes, materialized zero for
// kClearToZero recipes). The function does not independently decide
// unknown-plane policy.
void EmitNarrow4StateNbaCall(
    Context& ctx, const PackedSubviewAccess& access,
    const PackedNbaPolicy& policy, llvm::Value* val_raw,
    llvm::Value* unk_payload) {
  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  uint32_t narrow_bytes = access.subview_byte_span;

  auto* val_store = PrepareNarrowPlaneValue(
      builder, llvm_ctx, val_raw, narrow_bytes, access.semantic_bit_width);

  auto* unk_store = PrepareNarrowPlaneValue(
      builder, llvm_ctx, unk_payload, narrow_bytes, access.semantic_bit_width);

  auto* write_ptr = builder.CreateGEP(
      i8_ty, access.storage.base_ptr, access.byte_offset, "nba.write.ptr");

  auto* buf_ty = llvm::ArrayType::get(i8_ty, narrow_bytes);
  auto* val_alloca = builder.CreateAlloca(buf_ty, nullptr, "nba.val.a");
  EncodePlaneValueToByteBuffer(builder, llvm_ctx, val_alloca, 0, val_store);

  auto* unk_alloca = builder.CreateAlloca(buf_ty, nullptr, "nba.unk.a");
  EncodePlaneValueToByteBuffer(builder, llvm_ctx, unk_alloca, 0, unk_store);

  builder.CreateCall(
      ctx.GetLyraScheduleNbaCanonicalPacked(),
      {policy.engine_ptr, write_ptr, policy.notify_base_ptr, val_alloca,
       unk_alloca, llvm::ConstantInt::get(i32_ty, narrow_bytes),
       llvm::ConstantInt::get(i32_ty, access.storage.unk_plane_offset_bytes),
       policy.signal_id.Emit(builder)});
}

// Emit a full-width masked NBA call for a bit-addressable subview.
// Uses canonical packed storage ABI for byte sizes (not LLVM DataLayout).
// unk_lowering and unk_payload are from the PackedStorePlan; the function
// does not independently decide unknown-plane policy.
void EmitFullWidthMaskedNbaCall(
    Context& ctx, const PackedSubviewAccess& access,
    const PackedNbaPolicy& policy, const PackedRValue& value,
    UnknownPlaneLowering unk_lowering, llvm::Value* unk_payload) {
  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  auto lane_bits = GetCanonicalLaneBits(
      SemanticBits::FromRaw(access.storage.total_semantic_bits));
  auto* lane_ty = GetLaneIntType(llvm_ctx, lane_bits);

  auto* shift_amt = builder.CreateZExtOrTrunc(
      access.semantic_bit_offset, lane_ty, "nba.offset");
  auto mask_ap =
      llvm::APInt::getLowBitsSet(lane_bits.Raw(), access.semantic_bit_width);
  auto* mask = llvm::ConstantInt::get(lane_ty, mask_ap);
  auto* mask_shifted = builder.CreateShl(mask, shift_amt, "nba.mask");

  auto* src_val = builder.CreateZExtOrTrunc(value.val, lane_ty, "nba.src.val");
  auto* val_shifted = builder.CreateShl(src_val, shift_amt, "nba.val.shl");

  uint32_t plane_bytes = access.storage.storage_plane_byte_size;

  if (unk_lowering != UnknownPlaneLowering::kNone) {
    uint32_t total_bytes = plane_bytes + plane_bytes;

    auto* src_unk =
        builder.CreateZExtOrTrunc(unk_payload, lane_ty, "nba.src.unk");
    auto* unk_shifted = builder.CreateShl(src_unk, shift_amt, "nba.unk.shl");

    auto* buf_ty = llvm::ArrayType::get(i8_ty, total_bytes);
    auto* val_alloca = builder.CreateAlloca(buf_ty, nullptr, "nba.val.a");
    auto* mask_alloca = builder.CreateAlloca(buf_ty, nullptr, "nba.mask.a");

    EncodePlaneValueToByteBuffer(builder, llvm_ctx, val_alloca, 0, val_shifted);
    EncodePlaneValueToByteBuffer(
        builder, llvm_ctx, val_alloca, plane_bytes, unk_shifted);
    EncodePlaneValueToByteBuffer(
        builder, llvm_ctx, mask_alloca, 0, mask_shifted);
    EncodePlaneValueToByteBuffer(
        builder, llvm_ctx, mask_alloca, plane_bytes, mask_shifted);

    builder.CreateCall(
        ctx.GetLyraScheduleNba(),
        {policy.engine_ptr, access.storage.base_ptr, policy.notify_base_ptr,
         val_alloca, mask_alloca, llvm::ConstantInt::get(i32_ty, total_bytes),
         policy.signal_id.Emit(builder)});
  } else {
    auto* buf_ty = llvm::ArrayType::get(i8_ty, plane_bytes);
    auto* val_alloca = builder.CreateAlloca(buf_ty, nullptr, "nba.val.a");
    auto* mask_alloca = builder.CreateAlloca(buf_ty, nullptr, "nba.mask.a");

    EncodePlaneValueToByteBuffer(builder, llvm_ctx, val_alloca, 0, val_shifted);
    EncodePlaneValueToByteBuffer(
        builder, llvm_ctx, mask_alloca, 0, mask_shifted);

    builder.CreateCall(
        ctx.GetLyraScheduleNba(),
        {policy.engine_ptr, access.storage.base_ptr, policy.notify_base_ptr,
         val_alloca, mask_alloca, llvm::ConstantInt::get(i32_ty, plane_bytes),
         policy.signal_id.Emit(builder)});
  }
}

auto EmitDeferredStoreToPackedSubview(
    Context& ctx, const PackedSubviewAccess& access, const PackedRValue& value,
    const PackedNbaPolicy& policy) -> Result<void> {
  auto recipe_ctx = (access.kind == PackedSubviewKind::kByteAddressable)
                        ? StoreRecipeContext::kDeferredSubview
                        : StoreRecipeContext::kDeferredFullWidth;
  // Deferred stores are inherently notification-deferred (scheduled for
  // later application by the runtime). The changed predicate is not used.
  auto plan = BuildPackedStorePlan(access.storage, value, recipe_ctx, true);

  if (access.kind == PackedSubviewKind::kByteAddressable) {
    switch (plan.unk_lowering) {
      case UnknownPlaneLowering::kNone:
        EmitNarrow2StateNbaCall(ctx, access, policy, value.val);
        break;
      case UnknownPlaneLowering::kMaterializeForRuntime:
        EmitNarrow4StateNbaCall(ctx, access, policy, value.val, plan.unk_value);
        break;
      case UnknownPlaneLowering::kMaterializeZeroForRuntime: {
        auto* zero = llvm::ConstantInt::get(value.val->getType(), 0);
        EmitNarrow4StateNbaCall(ctx, access, policy, value.val, zero);
        break;
      }
      default:
        throw common::InternalError(
            "EmitDeferredStoreToPackedSubview",
            "invalid unk_lowering for deferred byte-addr");
    }
  } else {
    llvm::Value* unk_payload = nullptr;
    if (plan.unk_lowering == UnknownPlaneLowering::kMaterializeForRuntime) {
      unk_payload = plan.unk_value;
    } else if (
        plan.unk_lowering == UnknownPlaneLowering::kMaterializeZeroForRuntime) {
      auto deferred_lane_bits = GetCanonicalLaneBits(
          SemanticBits::FromRaw(access.storage.total_semantic_bits));
      unk_payload =
          LaneValue::FromNormalized(
              llvm::ConstantInt::get(
                  GetLaneIntType(ctx.GetLlvmContext(), deferred_lane_bits), 0),
              deferred_lane_bits)
              .Raw();
    }
    EmitFullWidthMaskedNbaCall(
        ctx, access, policy, value, plan.unk_lowering, unk_payload);
  }
  return {};
}

namespace {

auto NeedsChangedDetection(bool notification_deferred) -> bool {
  return !notification_deferred;
}

}  // namespace

auto BuildPackedStorePlan(
    const PackedStorageView& storage, const PackedRValue& rhs,
    StoreRecipeContext recipe_context, bool notification_deferred)
    -> PackedStorePlan {
  bool needs_changed = NeedsChangedDetection(notification_deferred);
  if (!storage.is_four_state) {
    return {
        .unk_policy = UnknownPlanePolicy::kNone,
        .unk_lowering = UnknownPlaneLowering::kNone,
        .needs_changed = needs_changed};
  }
  if (rhs.unk != nullptr) {
    return {
        .unk_policy = UnknownPlanePolicy::kStoreFromRhs,
        .unk_lowering = SelectUnknownPlaneLowering(
            UnknownPlanePolicy::kStoreFromRhs, recipe_context,
            notification_deferred),
        .unk_value = rhs.unk,
        .needs_changed = needs_changed,
    };
  }
  return {
      .unk_policy = UnknownPlanePolicy::kClearToZero,
      .unk_lowering = SelectUnknownPlaneLowering(
          UnknownPlanePolicy::kClearToZero, recipe_context,
          notification_deferred),
      .needs_changed = needs_changed,
  };
}

auto BuildWholeValueStorageView(
    Context& ctx, llvm::Value* base_ptr, TypeId type_id, bool is_canonical)
    -> PackedStorageView {
  const auto& types = ctx.GetTypeArena();
  const Type& type = types[type_id];
  auto kind = type.Kind();
  if (kind == TypeKind::kEnum) {
    kind = types[type.AsEnum().base_type].Kind();
  }

  uint32_t total_bits = 0;
  bool is_four_state = false;
  if (kind == TypeKind::kReal) {
    total_bits = 64;
    is_four_state = false;
  } else if (kind == TypeKind::kShortReal) {
    total_bits = 32;
    is_four_state = false;
  } else {
    total_bits = PackedBitWidth(type, types);
    is_four_state = ctx.IsPackedFourState(type);
  }
  uint32_t storage_plane_byte_size = GetStorageByteSize(total_bits);

  PackedStorageView view;
  view.base_ptr = base_ptr;
  view.total_semantic_bits = total_bits;
  view.storage_plane_byte_size = storage_plane_byte_size;
  view.unk_plane_offset_bytes =
      is_four_state ? FourStateUnknownLaneOffset(total_bits) : 0;
  view.is_four_state = is_four_state;

  if (!is_canonical) {
    auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, type_id);
    if (!llvm_type_result) {
      throw common::InternalError(
          "BuildWholeValueStorageView",
          "failed to resolve LLVM type for non-canonical storage");
    }
    view.SetLocalBacking(*llvm_type_result);
  } else {
    view.SetCanonicalBacking();
  }

  return view;
}

auto BuildRawBytesStorageView(llvm::Value* base_ptr, uint32_t byte_size)
    -> PackedStorageView {
  PackedStorageView view;
  view.base_ptr = base_ptr;
  view.total_semantic_bits = byte_size * 8;
  view.storage_plane_byte_size = byte_size;
  view.unk_plane_offset_bytes = 0;
  view.is_four_state = false;
  view.SetCanonicalBacking();
  return view;
}

auto BuildStorePolicyFromContext(
    Context& ctx, std::optional<SignalIdExpr> signal_id,
    const mir::SignalRef* mutation_signal) -> PackedStorePolicy {
  PackedStorePolicy policy;

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

  policy.notification_deferred =
      ctx.GetNotificationPolicy() == NotificationPolicy::kDeferred;
  if (mutation_signal != nullptr) {
    policy.requires_static_dirty_propagation =
        ctx.RequiresStaticDirtyPropagation(*mutation_signal);
    policy.mutation_owner_slot = ctx.GetCanonicalOwnerSlot(*mutation_signal);
  }
  policy.signal_id = std::move(signal_id);
  policy.engine_ptr = ctx.GetEnginePointer();
  policy.first_dirty_seen = ctx.GetFirstDirtySeenPtr();

  return policy;
}

auto GetPlanePointers(Context& ctx, const PackedStorageView& storage)
    -> PackedPlanePointers {
  auto& builder = ctx.GetBuilder();

  PackedPlanePointers result;
  result.val_ptr = storage.base_ptr;

  if (storage.is_four_state) {
    auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
    result.unk_ptr = builder.CreateGEP(
        i8_ty, storage.base_ptr,
        builder.getInt64(storage.unk_plane_offset_bytes), "planes.unk");
  }

  return result;
}

namespace {

// Returns true if byte_size is a power of 2 and <= 16 (eligible for inline
// compare+store instead of runtime LyraStorePacked call).
auto IsInlineStoreEligible(uint32_t byte_size) -> bool {
  return byte_size > 0 && byte_size <= 16 && (byte_size & (byte_size - 1)) == 0;
}

// Full-slot dirty range (byte_offset=0, byte_size=0).
auto MakeFullSlotDirtyRange(llvm::LLVMContext& llvm_ctx) -> PackedDirtyRange {
  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);
  return PackedDirtyRange{
      .byte_offset = llvm::ConstantInt::get(i32_ty, 0),
      .byte_size = llvm::ConstantInt::get(i32_ty, 0),
  };
}

// Flatten typed lane-domain planes to a single canonical integer for inline
// compare+store. 2-state: val (iN). 4-state: val | (unk << lane_bits) -> i(2N).
auto FlattenToCanonicalBits(
    llvm::IRBuilder<>& builder, llvm::LLVMContext& llvm_ctx,
    const LanePlaneValues& planes) -> llvm::Value* {
  auto lane_bits = planes.Val().Bits();

  if (!planes.Unk().has_value()) {
    return planes.Val().Raw();
  }

  uint32_t total_bits = lane_bits.Raw() * 2;
  auto* total_ty = llvm::IntegerType::get(llvm_ctx, total_bits);
  auto* val_ext =
      builder.CreateZExt(planes.Val().Raw(), total_ty, "canon.val.ext");
  auto* unk_ext =
      builder.CreateZExt(planes.Unk()->Raw(), total_ty, "canon.unk.ext");
  auto* unk_shifted = builder.CreateShl(
      unk_ext, llvm::ConstantInt::get(total_ty, lane_bits.Raw()),
      "canon.unk.shl");
  return builder.CreateOr(val_ext, unk_shifted, "canon.bits");
}

// Compute total canonical byte size for a storage view.
auto TotalCanonicalByteSize(const PackedStorageView& storage) -> uint32_t {
  return storage.is_four_state ? storage.storage_plane_byte_size * 2
                               : storage.storage_plane_byte_size;
}

// Store flattened canonical bits directly to inline-eligible canonical
// storage. Only valid when IsInlineStoreEligible(TotalCanonicalByteSize)
// is true, which guarantees the flattened value fits a single typed store
// within the inline store policy (<= 16 bytes, power-of-2).
void EmitInlineCanonicalDirectStore(
    Context& ctx, const PackedStorageView& storage,
    llvm::Value* canonical_bits) {
  uint32_t total_bytes = TotalCanonicalByteSize(storage);
  if (!IsInlineStoreEligible(total_bytes)) {
    throw common::InternalError(
        "EmitInlineCanonicalDirectStore",
        std::format("total_bytes {} is not inline-eligible", total_bytes));
  }
  ctx.GetBuilder().CreateStore(canonical_bits, storage.base_ptr);
}

// Emit the inline store path for whole-value packed stores.
// When needs_changed is true: loads old, stores new, compares, notifies.
// When needs_changed is false: stores new only.
void EmitInlineWholeValueStore(
    Context& ctx, const PackedStorageView& storage,
    llvm::Value* new_canonical_bits, const PackedStorePolicy& policy,
    bool needs_changed) {
  auto& builder = ctx.GetBuilder();

  if (needs_changed) {
    auto* bits_type = new_canonical_bits->getType();
    auto* old_bits = builder.CreateLoad(bits_type, storage.base_ptr, "wv.old");
    builder.CreateStore(new_canonical_bits, storage.base_ptr);
    auto* changed =
        builder.CreateICmpNE(old_bits, new_canonical_bits, "wv.chg");
    auto dirty_range = MakeFullSlotDirtyRange(ctx.GetLlvmContext());
    EmitPackedStoreNotification(ctx, changed, policy, dirty_range);
  } else {
    builder.CreateStore(new_canonical_bits, storage.base_ptr);
  }
}

// Emit a guarded LyraStorePacked runtime call with cross-context engine-null
// protection. This is the single internal call site for LyraStorePacked in
// the PSV module. All large-value compare+store+notify paths funnel here.
//
// LyraStorePacked semantics: compares src_ptr bytes against dst_ptr bytes,
// copies src_ptr to dst_ptr if different, dirty-marks on change.
//
// For kNotifyCrossContext, engine_ptr may be null at runtime. The call is
// guarded with a branch so the runtime helper is never invoked with a null
// engine.
void EmitGuardedLyraStorePacked(
    Context& ctx, const PackedStorePolicy& policy, llvm::Value* dst_ptr,
    llvm::Value* src_ptr, uint32_t byte_size) {
  if (policy.engine_ptr == nullptr) {
    throw common::InternalError(
        "EmitGuardedLyraStorePacked", "notify store mode requires engine_ptr");
  }
  if (!policy.signal_id.has_value()) {
    throw common::InternalError(
        "EmitGuardedLyraStorePacked", "notify store mode requires signal_id");
  }

  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();

  // When static contract allows suppression, check trace at runtime.
  // If not trace-observed, do plain memcpy without compare+notify.
  // trace_done_bb is set when we need a merge point after the notify path.
  llvm::BasicBlock* trace_done_bb = nullptr;
  if (!policy.requires_static_dirty_propagation &&
      policy.mutation_owner_slot.has_value()) {
    auto* trace_observed =
        ctx.EmitIsTraceObservedOwnerSlot(*policy.mutation_owner_slot);
    auto* fn = builder.GetInsertBlock()->getParent();
    auto* notify_bb = llvm::BasicBlock::Create(llvm_ctx, "rt.traced", fn);
    auto* plain_bb = llvm::BasicBlock::Create(llvm_ctx, "rt.plain", fn);
    trace_done_bb = llvm::BasicBlock::Create(llvm_ctx, "rt.tdone", fn);
    builder.CreateCondBr(trace_observed, notify_bb, plain_bb);

    builder.SetInsertPoint(plain_bb);
    builder.CreateMemCpy(
        dst_ptr, llvm::Align(1), src_ptr, llvm::Align(1), byte_size);
    builder.CreateBr(trace_done_bb);

    // Continue emitting the notify path into notify_bb.
    builder.SetInsertPoint(notify_bb);
  }

  auto* i32_ty = llvm::Type::getInt32Ty(llvm_ctx);

  auto emit_call = [&]() {
    builder.CreateCall(
        ctx.GetLyraStorePacked(),
        {policy.engine_ptr, dst_ptr, src_ptr,
         llvm::ConstantInt::get(i32_ty, byte_size),
         policy.signal_id->Emit(builder), llvm::ConstantInt::get(i32_ty, 0),
         llvm::ConstantInt::get(i32_ty, 0)});
  };

  if (policy.store_mode == PackedStoreMode::kNotifyCrossContext) {
    auto* fn = builder.GetInsertBlock()->getParent();
    auto* call_bb = llvm::BasicBlock::Create(llvm_ctx, "rt.store", fn);
    auto* done_bb = llvm::BasicBlock::Create(llvm_ctx, "rt.done", fn);
    auto* engine_not_null = builder.CreateICmpNE(
        policy.engine_ptr,
        llvm::ConstantPointerNull::get(llvm::PointerType::getUnqual(llvm_ctx)),
        "engine.nonnull");
    builder.CreateCondBr(engine_not_null, call_bb, done_bb);

    builder.SetInsertPoint(call_bb);
    emit_call();
    builder.CreateBr(done_bb);

    builder.SetInsertPoint(done_bb);
  } else {
    emit_call();
  }

  // If trace branch was emitted, merge the notify path back to
  // the trace done block.
  if (trace_done_bb != nullptr) {
    builder.CreateBr(trace_done_bb);
    builder.SetInsertPoint(trace_done_bb);
  }
}

// Raw plane values for byte-buffer encoding. Structured unwrap from typed
// LanePlaneValues at the byte-serialization boundary.
struct RawPlaneEncodingInputs {
  llvm::Value* val = nullptr;
  llvm::Value* unk = nullptr;
  bool has_unk = false;
};

auto GetRawPlaneEncodingInputs(const LanePlaneValues& planes)
    -> RawPlaneEncodingInputs {
  return RawPlaneEncodingInputs{
      .val = planes.Val().Raw(),
      .unk = planes.Unk().has_value() ? planes.Unk()->Raw() : nullptr,
      .has_unk = planes.Unk().has_value(),
  };
}

// Emit the large-value runtime-helper path for whole-value packed stores.
// Materializes canonical bytes into an alloca buffer and calls LyraStorePacked.
// Takes typed lane-domain planes; unwraps to raw values at the narrow
// byte-serialization boundary via GetRawPlaneEncodingInputs.
void EmitLargeWholeValueStore(
    Context& ctx, const PackedStorageView& storage,
    const LanePlaneValues& planes, const PackedStorePolicy& policy) {
  auto& builder = ctx.GetBuilder();
  auto& llvm_ctx = ctx.GetLlvmContext();

  uint32_t total_bytes = TotalCanonicalByteSize(storage);
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);

  auto* func = builder.GetInsertBlock()->getParent();
  llvm::IRBuilder<> entry_builder(
      &func->getEntryBlock(), func->getEntryBlock().begin());
  auto* buf_ty = llvm::ArrayType::get(i8_ty, total_bytes);
  auto* temp = entry_builder.CreateAlloca(buf_ty, nullptr, "wv.buf");

  builder.CreateMemSet(temp, builder.getInt8(0), total_bytes, llvm::Align(1));

  auto inputs = GetRawPlaneEncodingInputs(planes);

  // Skip encoding zero-constant planes -- the buffer is already zeroed.
  bool val_is_zero = llvm::isa<llvm::ConstantInt>(inputs.val) &&
                     llvm::cast<llvm::ConstantInt>(inputs.val)->isZero();
  if (!val_is_zero) {
    EncodePlaneValueToByteBuffer(builder, llvm_ctx, temp, 0, inputs.val);
  }
  if (inputs.has_unk) {
    bool unk_is_zero = llvm::isa<llvm::ConstantInt>(inputs.unk) &&
                       llvm::cast<llvm::ConstantInt>(inputs.unk)->isZero();
    if (!unk_is_zero) {
      EncodePlaneValueToByteBuffer(
          builder, llvm_ctx, temp, storage.unk_plane_offset_bytes, inputs.unk);
    }
  }

  if (policy.store_mode == PackedStoreMode::kDirectInit ||
      policy.notification_deferred) {
    builder.CreateMemCpy(
        storage.base_ptr, llvm::MaybeAlign(), temp, llvm::MaybeAlign(),
        total_bytes);
    return;
  }

  EmitGuardedLyraStorePacked(ctx, policy, storage.base_ptr, temp, total_bytes);
}

// Core whole-value store implementation shared by StorePackedValue and
// StorePackedValueFromCanonicalBytes. Takes typed lane-domain plane values
// plus the resolved unknown-plane action.
void EmitWholeValueStoreCore(
    Context& ctx, const PackedStorageView& storage, LaneValue val,
    std::optional<LaneValue> unk, UnknownPlaneLowering unk_lowering,
    const PackedStorePolicy& policy, bool needs_changed) {
  auto& builder = ctx.GetBuilder();
  auto expected_lane_bits =
      GetCanonicalLaneBits(SemanticBits::FromRaw(storage.total_semantic_bits));

  if (val.Bits() != expected_lane_bits) {
    throw common::InternalError(
        "EmitWholeValueStoreCore",
        std::format(
            "val lane bits {} does not match storage lane bits {}",
            val.Bits().Raw(), expected_lane_bits.Raw()));
  }

  if (!storage.IsCanonicalBacking()) {
    // Non-canonical (local allocas): typed store, no compare/notify.
    StoreLanePlanes(ctx, storage, LanePlaneValues::Make(val, unk));
    return;
  }

  // Helper: make a zero LaneValue at canonical lane width.
  auto make_zero_unk = [&]() -> LaneValue {
    return LaneValue::FromNormalized(
        llvm::ConstantInt::get(
            GetLaneIntType(ctx.GetLlvmContext(), expected_lane_bits), 0),
        expected_lane_bits);
  };

  if (policy.store_mode == PackedStoreMode::kDirectInit) {
    // Canonical direct init: no compare, no notify.
    switch (unk_lowering) {
      case UnknownPlaneLowering::kNone:
      case UnknownPlaneLowering::kFlattenAndStoreWhole:
      case UnknownPlaneLowering::kMaterializeForRuntime:
        break;
      case UnknownPlaneLowering::kFlattenZeroAndStoreWhole:
      case UnknownPlaneLowering::kMaterializeZeroForRuntime:
        unk = make_zero_unk();
        break;
      default:
        throw common::InternalError(
            "EmitWholeValueStoreCore",
            "invalid unk_lowering for whole-value init path");
    }
    uint32_t total_bytes = TotalCanonicalByteSize(storage);
    if (IsInlineStoreEligible(total_bytes)) {
      auto planes = LanePlaneValues::Make(val, unk);
      auto* canonical_bits =
          FlattenToCanonicalBits(builder, ctx.GetLlvmContext(), planes);
      EmitInlineCanonicalDirectStore(ctx, storage, canonical_bits);
    } else {
      EmitLargeWholeValueStore(
          ctx, storage, LanePlaneValues::Make(val, unk), policy);
    }
    return;
  }

  // Notification paths: recipe-driven unknown-plane lowering.
  uint32_t total_bytes = TotalCanonicalByteSize(storage);

  auto store_planes = [&](const LanePlaneValues& planes) {
    if (IsInlineStoreEligible(total_bytes)) {
      auto* canonical_bits =
          FlattenToCanonicalBits(builder, ctx.GetLlvmContext(), planes);
      EmitInlineWholeValueStore(
          ctx, storage, canonical_bits, policy, needs_changed);
    } else {
      EmitLargeWholeValueStore(ctx, storage, planes, policy);
    }
  };

  switch (unk_lowering) {
    case UnknownPlaneLowering::kNone:
      store_planes(LanePlaneValues::Make(val, std::nullopt));
      break;

    case UnknownPlaneLowering::kFlattenAndStoreWhole:
      store_planes(LanePlaneValues::Make(val, unk));
      break;

    case UnknownPlaneLowering::kFlattenZeroAndStoreWhole:
      store_planes(LanePlaneValues::Make(val, make_zero_unk()));
      break;

    case UnknownPlaneLowering::kMaterializeForRuntime:
      EmitLargeWholeValueStore(
          ctx, storage, LanePlaneValues::Make(val, unk), policy);
      break;

    case UnknownPlaneLowering::kMaterializeZeroForRuntime:
      EmitLargeWholeValueStore(
          ctx, storage, LanePlaneValues::Make(val, make_zero_unk()), policy);
      break;

    default:
      throw common::InternalError(
          "EmitWholeValueStoreCore",
          "invalid unk_lowering for whole-value path");
  }
}

}  // namespace

auto ClassifySubviewRecipeContext(PackedSubviewKind kind)
    -> StoreRecipeContext {
  switch (kind) {
    case PackedSubviewKind::kByteAddressable:
      return StoreRecipeContext::kSplitPlaneSubview;
    case PackedSubviewKind::kBitAddressable:
      return StoreRecipeContext::kBitAddressableRmw;
  }
  throw common::InternalError(
      "ClassifySubviewRecipeContext", "unknown subview kind");
}

auto ClassifyWholeValueRecipeContext(const PackedStorageView& storage)
    -> StoreRecipeContext {
  uint32_t total = TotalCanonicalByteSize(storage);
  if (IsInlineStoreEligible(total)) {
    if (storage.is_four_state &&
        storage.unk_plane_offset_bytes != storage.storage_plane_byte_size) {
      throw common::InternalError(
          "ClassifyWholeValueRecipeContext",
          "inline 4-state storage has non-adjacent planes");
    }
    return StoreRecipeContext::kWholeValueInlineInterleaved;
  }
  return StoreRecipeContext::kWholeValueRuntimeAssisted;
}

auto SelectUnknownPlaneLowering(
    UnknownPlanePolicy policy, StoreRecipeContext context,
    bool notification_deferred) -> UnknownPlaneLowering {
  if (policy == UnknownPlanePolicy::kNone) {
    return UnknownPlaneLowering::kNone;
  }
  if (policy == UnknownPlanePolicy::kStoreFromRhs) {
    switch (context) {
      case StoreRecipeContext::kSplitPlaneSubview:
        return UnknownPlaneLowering::kWritePlaneAtOffset;
      case StoreRecipeContext::kBitAddressableRmw:
        return UnknownPlaneLowering::kMergePlaneBitsRmw;
      case StoreRecipeContext::kWholeValueInlineInterleaved:
        return UnknownPlaneLowering::kFlattenAndStoreWhole;
      case StoreRecipeContext::kWholeValueRuntimeAssisted:
      case StoreRecipeContext::kDeferredSubview:
      case StoreRecipeContext::kDeferredFullWidth:
        return UnknownPlaneLowering::kMaterializeForRuntime;
    }
  }
  // kClearToZero: select recipe based on context and notification liveness.
  switch (context) {
    case StoreRecipeContext::kSplitPlaneSubview:
      // When notification is deferred, the changed predicate is dead.
      // Unconditional clear avoids per-element load+compare+branch overhead.
      return notification_deferred
                 ? UnknownPlaneLowering::kUnconditionalClearAtOffset
                 : UnknownPlaneLowering::kConditionalClearAtOffset;
    case StoreRecipeContext::kBitAddressableRmw:
      return UnknownPlaneLowering::kMaskedClearBitsRmw;
    case StoreRecipeContext::kWholeValueInlineInterleaved:
      return UnknownPlaneLowering::kFlattenZeroAndStoreWhole;
    case StoreRecipeContext::kWholeValueRuntimeAssisted:
    case StoreRecipeContext::kDeferredSubview:
    case StoreRecipeContext::kDeferredFullWidth:
      return UnknownPlaneLowering::kMaterializeZeroForRuntime;
  }
  throw common::InternalError(
      "SelectUnknownPlaneLowering", "unhandled policy/context combination");
}

auto MaterializePackedValue(Context& ctx, const PackedStorageView& storage)
    -> Result<PackedRValue> {
  auto planes = LoadLanePlanes(ctx, storage);
  auto lane_bits = planes.Val().Bits();
  auto* lane_ty = GetLaneIntType(ctx.GetLlvmContext(), lane_bits);

  PackedRValue result;
  result.semantic_bits = storage.total_semantic_bits;
  result.val = planes.Val().Raw();

  if (lane_bits.Raw() > storage.total_semantic_bits) {
    auto mask = llvm::APInt::getLowBitsSet(
        lane_bits.Raw(), storage.total_semantic_bits);
    result.val = ctx.GetBuilder().CreateAnd(
        planes.Val().Raw(), llvm::ConstantInt::get(lane_ty, mask),
        "mat.val.mask");
  }

  if (storage.is_four_state && planes.Unk().has_value()) {
    result.unk = planes.Unk()->Raw();
    if (lane_bits.Raw() > storage.total_semantic_bits) {
      auto mask = llvm::APInt::getLowBitsSet(
          lane_bits.Raw(), storage.total_semantic_bits);
      result.unk = ctx.GetBuilder().CreateAnd(
          planes.Unk()->Raw(), llvm::ConstantInt::get(lane_ty, mask),
          "mat.unk.mask");
    }
  }

  return result;
}

auto StorePackedValue(
    Context& ctx, const PackedStorageView& storage, const PackedRValue& value,
    const PackedStorePolicy& policy) -> Result<void> {
  auto recipe_ctx = ClassifyWholeValueRecipeContext(storage);
  auto plan = BuildPackedStorePlan(
      storage, value, recipe_ctx, policy.notification_deferred);

  auto wv_lane_bits =
      GetCanonicalLaneBits(SemanticBits::FromRaw(storage.total_semantic_bits));

  auto& builder = ctx.GetBuilder();
  auto lane_val = NormalizeToLaneWidth(builder, value.val, wv_lane_bits);

  std::optional<LaneValue> lane_unk;
  if (plan.unk_lowering == UnknownPlaneLowering::kFlattenAndStoreWhole ||
      plan.unk_lowering == UnknownPlaneLowering::kMaterializeForRuntime) {
    lane_unk = NormalizeToLaneWidth(builder, plan.unk_value, wv_lane_bits);
  }

  EmitWholeValueStoreCore(
      ctx, storage, lane_val, lane_unk, plan.unk_lowering, policy,
      plan.needs_changed);
  return {};
}

auto StorePackedValueFromCanonicalBytes(
    Context& ctx, const PackedStorageView& storage, llvm::Value* src_bytes_ptr,
    const PackedStorePolicy& policy) -> Result<void> {
  auto& builder = ctx.GetBuilder();
  uint32_t total_bytes = TotalCanonicalByteSize(storage);

  if (policy.store_mode == PackedStoreMode::kDirectInit ||
      policy.notification_deferred) {
    builder.CreateMemCpy(
        storage.base_ptr, llvm::MaybeAlign(), src_bytes_ptr, llvm::MaybeAlign(),
        total_bytes);
    return {};
  }

  if (!storage.IsCanonicalBacking()) {
    builder.CreateMemCpy(
        storage.base_ptr, llvm::MaybeAlign(), src_bytes_ptr, llvm::MaybeAlign(),
        total_bytes);
    return {};
  }

  // Load planes from the canonical byte buffer using the alignment-safe
  // read helper, then route through the shared inline/large path.
  auto cb_lane_bits =
      GetCanonicalLaneBits(SemanticBits::FromRaw(storage.total_semantic_bits));
  auto* lane_ty = GetLaneIntType(ctx.GetLlvmContext(), cb_lane_bits);

  auto lane_val = LaneValue::FromNormalized(
      DecodePlaneValueFromByteBuffer(
          builder, ctx.GetLlvmContext(), src_bytes_ptr, 0, lane_ty),
      cb_lane_bits);

  std::optional<LaneValue> lane_unk;
  if (storage.is_four_state) {
    lane_unk = LaneValue::FromNormalized(
        DecodePlaneValueFromByteBuffer(
            builder, ctx.GetLlvmContext(), src_bytes_ptr,
            storage.unk_plane_offset_bytes, lane_ty),
        cb_lane_bits);
  }

  // Canonical bytes carry the full unk plane; always unconditional store
  // when 4-state, kNone when 2-state. kClearToZero does not apply
  // here because this path has pre-materialized bytes, not an RHS.
  auto unk_lowering = lane_unk.has_value()
                          ? UnknownPlaneLowering::kFlattenAndStoreWhole
                          : UnknownPlaneLowering::kNone;
  EmitWholeValueStoreCore(
      ctx, storage, lane_val, lane_unk, unk_lowering, policy,
      NeedsChangedDetection(policy.notification_deferred));
  return {};
}

auto NotifyPackedStorageWritten(
    Context& ctx, const PackedStorageView& storage,
    llvm::Value* old_snapshot_ptr, const PackedStorePolicy& policy)
    -> Result<void> {
  uint32_t total_bytes = TotalCanonicalByteSize(storage);

  if (policy.store_mode == PackedStoreMode::kDirectInit ||
      policy.notification_deferred) {
    return {};
  }

  if (!storage.IsCanonicalBacking() || !policy.signal_id.has_value()) {
    return {};
  }

  if (IsInlineStoreEligible(total_bytes)) {
    auto& builder = ctx.GetBuilder();
    auto* bits_ty =
        llvm::IntegerType::get(ctx.GetLlvmContext(), total_bytes * 8);
    auto* old_bits =
        builder.CreateLoad(bits_ty, old_snapshot_ptr, "posthoc.old");
    auto* new_bits =
        builder.CreateLoad(bits_ty, storage.base_ptr, "posthoc.new");
    auto* changed = builder.CreateICmpNE(old_bits, new_bits, "posthoc.chg");
    auto dirty_range = MakeFullSlotDirtyRange(ctx.GetLlvmContext());
    EmitPackedStoreNotification(ctx, changed, policy, dirty_range);
  } else {
    EmitGuardedLyraStorePacked(
        ctx, policy, storage.base_ptr, old_snapshot_ptr, total_bytes);
  }

  return {};
}

auto BuildPackedRValueFromRaw(
    Context& ctx, llvm::Value* raw, uint32_t semantic_bits) -> PackedRValue {
  auto& builder = ctx.GetBuilder();
  PackedRValue result;
  result.semantic_bits = semantic_bits;

  if (raw->getType()->isStructTy()) {
    auto* st = llvm::cast<llvm::StructType>(raw->getType());
    if (st->getNumElements() != 2 || !st->getElementType(0)->isIntegerTy() ||
        st->getElementType(0) != st->getElementType(1)) {
      throw common::InternalError(
          "BuildPackedRValueFromRaw",
          "struct input must be canonical {iN, iN} 4-state packed form");
    }
    result.val = builder.CreateExtractValue(raw, 0, "rhs.val");
    result.unk = builder.CreateExtractValue(raw, 1, "rhs.unk");
  } else {
    result.val = raw;
    // unk stays nullptr -- scalar LLVM type means the source variable is
    // 2-state. No zero synthesis.
  }
  return result;
}

auto LoadPackedPlace(
    Context& ctx, mir::PlaceId place_id, llvm::Type* target_type)
    -> Result<llvm::Value*> {
  auto path_result = ExtractPackedAccessPath(ctx, place_id);
  if (!path_result) return std::unexpected(path_result.error());

  auto subview_result = ResolvePackedSubview(ctx, *path_result);
  if (!subview_result) return std::unexpected(subview_result.error());

  auto rval_result = EmitLoadFromPackedSubview(ctx, *subview_result);
  if (!rval_result) return std::unexpected(rval_result.error());

  return ConvertPackedRValueToLegacyLlvmValue(ctx, *rval_result, target_type);
}

auto ConvertPackedRValueToLegacyLlvmValue(
    Context& ctx, const PackedRValue& rval, llvm::Type* target_type)
    -> llvm::Value* {
  auto& builder = ctx.GetBuilder();

  // Legacy conversion: the TARGET type determines the output shape.
  // struct -> emit {val, unk} form (zero for unk if RHS is 2-state)
  // integer -> emit plain value (mask out unknown bits if 4-state)
  // This is a legacy interop boundary, not a semantic decision point.
  if (target_type->isStructTy()) {
    auto* result_struct = llvm::cast<llvm::StructType>(target_type);
    auto* result_elem = result_struct->getElementType(0);
    llvm::Value* val =
        builder.CreateZExtOrTrunc(rval.val, result_elem, "psv.to.val");
    llvm::Value* unk =
        rval.unk != nullptr
            ? builder.CreateZExtOrTrunc(rval.unk, result_elem, "psv.to.unk")
            : llvm::ConstantInt::get(result_elem, 0);
    llvm::Value* result = llvm::UndefValue::get(result_struct);
    result = builder.CreateInsertValue(result, val, 0);
    result = builder.CreateInsertValue(result, unk, 1);
    return result;
  }

  if (target_type->isIntegerTy()) {
    llvm::Value* val = rval.val;
    if (rval.unk != nullptr) {
      auto* not_unk = builder.CreateNot(rval.unk, "psv.to.notunk");
      val = builder.CreateAnd(val, not_unk, "psv.to.known");
    }
    return builder.CreateZExtOrTrunc(val, target_type, "psv.to.ext");
  }

  // Float types (real/shortreal): bitcast from integer representation.
  // PackedRValue carries the bitcasted integer; convert back to float.
  if (target_type->isDoubleTy() || target_type->isFloatTy()) {
    auto* int_ty = target_type->isDoubleTy()
                       ? llvm::Type::getInt64Ty(ctx.GetLlvmContext())
                       : llvm::Type::getInt32Ty(ctx.GetLlvmContext());
    auto* int_val = builder.CreateZExtOrTrunc(rval.val, int_ty, "psv.to.int");
    return builder.CreateBitCast(int_val, target_type, "psv.to.float");
  }

  throw common::InternalError(
      "ConvertPackedRValueToLegacyLlvmValue", "unexpected target type");
}

}  // namespace lyra::lowering::mir_to_llvm
