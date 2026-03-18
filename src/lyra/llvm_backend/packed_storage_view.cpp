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
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

// BitRangeProjection byte-alignment proof.
//
// For constant offsets, checks the actual value.
// For dynamic offsets, relies on the explicit is_element_scaled flag
// set at the MIR lowering site. Element-scaled offsets are always
// multiples of element_width (= br.width), so when br.width % 8 == 0,
// the offset is provably byte-aligned.
auto IsBitRangeStepProvablyByteAligned(const mir::BitRangeProjection& br)
    -> bool {
  if (br.width % 8 != 0) return false;

  // Constant offset: check the actual value.
  if (const auto* constant = std::get_if<Constant>(&br.bit_offset.payload)) {
    const auto* integral = std::get_if<IntegralConstant>(&constant->value);
    if (integral == nullptr || integral->value.empty()) return false;
    return (integral->value[0] % 8) == 0;
  }

  // Dynamic offset: only provably byte-aligned when the offset was
  // produced by element-index scaling (offset = index * element_width).
  // This flag is set at the MIR lowering site for packed array element
  // access. Part-selects, range selects, and bit selects leave it false.
  return br.is_element_scaled;
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
  uint32_t value_plane_bytes = GetStorageByteSize(total_bits);

  // GetPlacePointer skips BitRangeProjection suffix.
  auto ptr_result = ctx.GetPlacePointer(place_id);
  if (!ptr_result) return std::unexpected(ptr_result.error());

  bool is_canonical = place.root.kind == mir::PlaceRoot::Kind::kModuleSlot ||
                      place.root.kind == mir::PlaceRoot::Kind::kDesignGlobal;

  PackedStorageView view{
      .base_ptr = *ptr_result,
      .total_semantic_bits = total_bits,
      .value_plane_bytes = value_plane_bytes,
      .unk_plane_offset_bytes =
          is_four_state ? FourStateUnknownLaneOffset(total_bits) : 0,
      .is_four_state = is_four_state,
      .is_canonical_storage = is_canonical,
  };

  if (!is_canonical) {
    // Resolve the LLVM storage type for the packed storage root directly
    // from the base TypeId. This is the type of the local alloca that
    // backs this packed object, not a projected/derived type.
    auto llvm_type_result = BuildLlvmTypeForTypeId(ctx, base_type_id);
    if (!llvm_type_result) return std::unexpected(llvm_type_result.error());
    view.local_llvm_type = *llvm_type_result;
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
  if (path.storage.is_canonical_storage && final_width % 8 == 0 &&
      all_steps_byte_aligned) {
    access.kind = PackedSubviewKind::kByteAddressable;
    access.byte_offset =
        builder.CreateLShr(total_bit_offset, 3, "psv.byte_off");
    access.storage_byte_width = GetStorageByteSize(final_width);
  } else {
    access.kind = PackedSubviewKind::kBitAddressable;
  }

  return access;
}

namespace {

// Load a full-width packed plane chunk from canonical storage via explicit
// i8 GEP. Only valid for canonical storage.
auto LoadPackedPlaneChunk(
    llvm::IRBuilder<>& builder, llvm::LLVMContext& llvm_ctx,
    llvm::Value* base_ptr, uint64_t byte_offset, uint32_t storage_bits,
    const char* name) -> llvm::Value* {
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* lane_ty = llvm::IntegerType::get(llvm_ctx, storage_bits);
  auto* ptr =
      builder.CreateGEP(i8_ty, base_ptr, builder.getInt64(byte_offset), name);
  return builder.CreateLoad(lane_ty, ptr);
}

// Store a full-width packed plane chunk to canonical storage via explicit
// i8 GEP. Only valid for canonical storage.
void StorePackedPlaneChunk(
    llvm::IRBuilder<>& builder, llvm::LLVMContext& llvm_ctx,
    llvm::Value* base_ptr, uint64_t byte_offset, llvm::Value* value,
    const char* name) {
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  auto* ptr =
      builder.CreateGEP(i8_ty, base_ptr, builder.getInt64(byte_offset), name);
  builder.CreateStore(value, ptr);
}

// Full-plane values for bit-addressable operations.
struct FullPlaneValues {
  llvm::Value* val = nullptr;
  llvm::Value* unk = nullptr;
};

// Load full-plane values from packed storage, dispatching on storage kind.
// Canonical: byte-addressed GEP to each plane.
// Non-canonical: LLVM typed load + ExtractValue for 4-state structs.
auto LoadFullPlanes(Context& ctx, const PackedStorageView& storage)
    -> FullPlaneValues {
  auto& builder = ctx.GetBuilder();
  uint32_t base_storage_bits = storage.value_plane_bytes * 8;

  if (storage.is_canonical_storage) {
    FullPlaneValues planes;
    planes.val = LoadPackedPlaneChunk(
        builder, ctx.GetLlvmContext(), storage.base_ptr, 0, base_storage_bits,
        "fp.val.ptr");
    if (storage.is_four_state) {
      planes.unk = LoadPackedPlaneChunk(
          builder, ctx.GetLlvmContext(), storage.base_ptr,
          storage.unk_plane_offset_bytes, base_storage_bits, "fp.unk.ptr");
    }
    return planes;
  }

  // Non-canonical: LLVM typed load.
  auto* loaded =
      builder.CreateLoad(storage.local_llvm_type, storage.base_ptr, "fp.local");
  FullPlaneValues planes;
  if (storage.is_four_state) {
    auto* st = llvm::dyn_cast<llvm::StructType>(storage.local_llvm_type);
    if (st == nullptr || st->getNumElements() != 2 ||
        st->getElementType(0) != st->getElementType(1) ||
        !st->getElementType(0)->isIntegerTy()) {
      throw common::InternalError(
          "LoadFullPlanes",
          "non-canonical 4-state local storage must be a struct with "
          "exactly 2 identical integer elements");
    }
    planes.val = builder.CreateExtractValue(loaded, 0, "fp.val");
    planes.unk = builder.CreateExtractValue(loaded, 1, "fp.unk");
  } else {
    planes.val = loaded;
  }
  return planes;
}

// Store full-plane values to packed storage, dispatching on storage kind.
// Canonical: byte-addressed GEP to each plane.
// Non-canonical: LLVM typed store (InsertValue for 4-state structs).
void StoreFullPlanes(
    Context& ctx, const PackedStorageView& storage, llvm::Value* val,
    llvm::Value* unk) {
  auto& builder = ctx.GetBuilder();

  if (storage.is_canonical_storage) {
    StorePackedPlaneChunk(
        builder, ctx.GetLlvmContext(), storage.base_ptr, 0, val,
        "fp.val.store");
    if (storage.is_four_state && unk != nullptr) {
      StorePackedPlaneChunk(
          builder, ctx.GetLlvmContext(), storage.base_ptr,
          storage.unk_plane_offset_bytes, unk, "fp.unk.store");
    }
    return;
  }

  // Non-canonical: LLVM typed store.
  if (storage.is_four_state) {
    auto* st = llvm::dyn_cast<llvm::StructType>(storage.local_llvm_type);
    if (st == nullptr || st->getNumElements() != 2 ||
        st->getElementType(0) != st->getElementType(1) ||
        !st->getElementType(0)->isIntegerTy()) {
      throw common::InternalError(
          "StoreFullPlanes",
          "non-canonical 4-state local storage must be a struct with "
          "exactly 2 identical integer elements");
    }
    llvm::Value* packed = llvm::UndefValue::get(storage.local_llvm_type);
    packed = builder.CreateInsertValue(packed, val, 0);
    packed = builder.CreateInsertValue(
        packed,
        unk != nullptr ? unk : llvm::ConstantInt::get(val->getType(), 0), 1);
    builder.CreateStore(packed, storage.base_ptr);
  } else {
    builder.CreateStore(val, storage.base_ptr);
  }
}

auto EmitByteAddressableLoad(Context& ctx, const PackedSubviewAccess& access)
    -> PackedRValue {
  auto& builder = ctx.GetBuilder();
  auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
  uint32_t storage_bits = access.storage_byte_width * 8;
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
  result.is_four_state = access.storage.is_four_state;

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
// Uses LoadFullPlanes for storage-kind-aware full-plane access.
auto EmitBitAddressableLoad(Context& ctx, const PackedSubviewAccess& access)
    -> PackedRValue {
  auto& builder = ctx.GetBuilder();

  uint32_t base_storage_bits = access.storage.value_plane_bytes * 8;
  auto planes = LoadFullPlanes(ctx, access.storage);

  PackedRValue result;
  result.semantic_bits = access.semantic_bit_width;
  result.is_four_state = access.storage.is_four_state;

  uint32_t elem_storage_bytes = GetStorageByteSize(access.semantic_bit_width);
  uint32_t elem_storage_bits = elem_storage_bytes * 8;
  auto* lane_ty =
      llvm::IntegerType::get(ctx.GetLlvmContext(), base_storage_bits);
  auto* shift_amt = builder.CreateZExtOrTrunc(
      access.semantic_bit_offset, lane_ty, "br.offset");

  // Value plane: shift + truncate + mask
  llvm::Value* val = builder.CreateLShr(planes.val, shift_amt, "br.val.shr");
  if (elem_storage_bits < base_storage_bits) {
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
  if (access.storage.is_four_state && planes.unk != nullptr) {
    llvm::Value* unk = builder.CreateLShr(planes.unk, shift_amt, "br.unk.shr");
    if (elem_storage_bits < base_storage_bits) {
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
    Context& ctx, const PackedSubviewAccess& access, const PackedRValue& value)
    -> llvm::Value* {
  if (!access.storage.is_canonical_storage) {
    throw common::InternalError(
        "EmitByteAddressableStore",
        "byte-addressable store requires canonical storage");
  }

  auto& builder = ctx.GetBuilder();
  auto* i8_ty = llvm::Type::getInt8Ty(ctx.GetLlvmContext());
  uint32_t storage_bits = access.storage_byte_width * 8;
  auto* store_ty = llvm::IntegerType::get(ctx.GetLlvmContext(), storage_bits);

  // Value plane
  auto* val_ptr = builder.CreateGEP(
      i8_ty, access.storage.base_ptr, access.byte_offset, "psw.val.ptr");
  auto* old_val = builder.CreateLoad(store_ty, val_ptr, "psw.val.old");

  llvm::Value* new_val =
      builder.CreateZExtOrTrunc(value.val, store_ty, "psw.val.new");
  if (storage_bits > access.semantic_bit_width) {
    auto mask =
        llvm::APInt::getLowBitsSet(storage_bits, access.semantic_bit_width);
    new_val = builder.CreateAnd(
        new_val, llvm::ConstantInt::get(store_ty, mask), "psw.val.fit");
  }
  builder.CreateStore(new_val, val_ptr);

  llvm::Value* changed =
      builder.CreateICmpNE(old_val, new_val, "psw.val.changed");

  // Unknown plane (4-state)
  if (access.storage.is_four_state) {
    auto* unk_off = builder.CreateAdd(
        access.byte_offset,
        llvm::ConstantInt::get(
            access.byte_offset->getType(),
            access.storage.unk_plane_offset_bytes),
        "psw.unk.off");
    auto* unk_ptr = builder.CreateGEP(
        i8_ty, access.storage.base_ptr, unk_off, "psw.unk.ptr");
    auto* old_unk = builder.CreateLoad(store_ty, unk_ptr, "psw.unk.old");

    llvm::Value* new_unk = nullptr;
    if (value.unk != nullptr) {
      new_unk = builder.CreateZExtOrTrunc(value.unk, store_ty, "psw.unk.new");
    } else {
      new_unk = llvm::ConstantInt::get(store_ty, 0);
    }
    if (storage_bits > access.semantic_bit_width) {
      auto mask =
          llvm::APInt::getLowBitsSet(storage_bits, access.semantic_bit_width);
      new_unk = builder.CreateAnd(
          new_unk, llvm::ConstantInt::get(store_ty, mask), "psw.unk.fit");
    }
    builder.CreateStore(new_unk, unk_ptr);

    auto* unk_changed =
        builder.CreateICmpNE(old_unk, new_unk, "psw.unk.changed");
    changed = builder.CreateOr(changed, unk_changed, "psw.changed");
  }

  return changed;
}

// Bit-addressable store: full-width RMW via LoadFullPlanes/StoreFullPlanes.
// Returns single semantic changed predicate (i1).
auto EmitBitAddressableStore(
    Context& ctx, const PackedSubviewAccess& access, const PackedRValue& value)
    -> llvm::Value* {
  auto& builder = ctx.GetBuilder();

  uint32_t base_storage_bits = access.storage.value_plane_bytes * 8;
  auto* lane_ty =
      llvm::IntegerType::get(ctx.GetLlvmContext(), base_storage_bits);

  auto old_planes = LoadFullPlanes(ctx, access.storage);

  auto* shift_amt = builder.CreateZExtOrTrunc(
      access.semantic_bit_offset, lane_ty, "rmw.offset");
  auto mask_ap =
      llvm::APInt::getLowBitsSet(base_storage_bits, access.semantic_bit_width);
  auto* mask = llvm::ConstantInt::get(lane_ty, mask_ap);
  auto* mask_shifted = builder.CreateShl(mask, shift_amt, "rmw.mask");
  auto* not_mask = builder.CreateNot(mask_shifted, "rmw.notmask");

  // Value plane RMW
  auto* src_val = builder.CreateZExtOrTrunc(value.val, lane_ty, "rmw.src.val");
  auto* val_shifted = builder.CreateShl(src_val, shift_amt, "rmw.val.shl");
  auto* val_cleared =
      builder.CreateAnd(old_planes.val, not_mask, "rmw.val.clear");
  auto* val_result = builder.CreateOr(val_cleared, val_shifted, "rmw.val");

  llvm::Value* changed =
      builder.CreateICmpNE(old_planes.val, val_result, "rmw.val.changed");

  // Unknown plane RMW (4-state)
  llvm::Value* unk_result = nullptr;
  if (access.storage.is_four_state && old_planes.unk != nullptr) {
    llvm::Value* src_unk = nullptr;
    if (value.unk != nullptr) {
      src_unk = builder.CreateZExtOrTrunc(value.unk, lane_ty, "rmw.src.unk");
    } else {
      src_unk = llvm::ConstantInt::get(lane_ty, 0);
    }
    auto* unk_shifted = builder.CreateShl(src_unk, shift_amt, "rmw.unk.shl");
    auto* unk_cleared =
        builder.CreateAnd(old_planes.unk, not_mask, "rmw.unk.clear");
    unk_result = builder.CreateOr(unk_cleared, unk_shifted, "rmw.unk");

    auto* unk_changed =
        builder.CreateICmpNE(old_planes.unk, unk_result, "rmw.unk.changed");
    changed = builder.CreateOr(changed, unk_changed, "rmw.changed");
  }

  StoreFullPlanes(ctx, access.storage, val_result, unk_result);
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
        .byte_size = llvm::ConstantInt::get(i32_ty, access.storage_byte_width),
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

  auto* fn = builder.GetInsertBlock()->getParent();
  auto* dirty_bb = llvm::BasicBlock::Create(llvm_ctx, "psw.dirty", fn);
  auto* done_bb = llvm::BasicBlock::Create(llvm_ctx, "psw.done", fn);

  builder.CreateCondBr(should_mark, dirty_bb, done_bb);

  builder.SetInsertPoint(dirty_bb);
  builder.CreateCall(
      ctx.GetLyraMarkDirty(),
      {policy.engine_ptr, policy.signal_id->Emit(builder),
       dirty_range.byte_offset, dirty_range.byte_size});
  builder.CreateBr(done_bb);

  builder.SetInsertPoint(done_bb);
}

auto EmitStoreToPackedSubview(
    Context& ctx, const PackedSubviewAccess& access, const PackedRValue& value,
    const PackedStorePolicy& policy) -> Result<void> {
  llvm::Value* changed = nullptr;
  if (access.kind == PackedSubviewKind::kByteAddressable) {
    changed = EmitByteAddressableStore(ctx, access, value);
  } else {
    changed = EmitBitAddressableStore(ctx, access, value);
  }

  auto dirty_range = GetSubviewDirtyRange(ctx, access);
  EmitPackedStoreNotification(ctx, changed, policy, dirty_range);
  return {};
}

auto EmitSchedulePackedSubviewWrite(
    [[maybe_unused]] Context& ctx,
    [[maybe_unused]] const PackedSubviewAccess& access,
    [[maybe_unused]] const PackedRValue& value,
    [[maybe_unused]] const PackedStorePolicy& policy) -> Result<void> {
  throw common::InternalError(
      "EmitSchedulePackedSubviewWrite", "not yet implemented in new module");
}

auto MaterializePackedValue(
    [[maybe_unused]] Context& ctx,
    [[maybe_unused]] const PackedStorageView& storage) -> Result<PackedRValue> {
  throw common::InternalError(
      "MaterializePackedValue", "not yet implemented in new module");
}

auto StorePackedValue(
    [[maybe_unused]] Context& ctx,
    [[maybe_unused]] const PackedStorageView& storage,
    [[maybe_unused]] const PackedRValue& value,
    [[maybe_unused]] const PackedStorePolicy& policy) -> Result<void> {
  throw common::InternalError(
      "StorePackedValue", "not yet implemented in new module");
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

  if (target_type->isStructTy() && rval.is_four_state) {
    auto* result_struct = llvm::cast<llvm::StructType>(target_type);
    auto* result_elem = result_struct->getElementType(0);
    llvm::Value* val =
        builder.CreateZExtOrTrunc(rval.val, result_elem, "psv.to.val");
    llvm::Value* unk = builder.CreateZExtOrTrunc(
        rval.unk != nullptr ? rval.unk
                            : llvm::ConstantInt::get(rval.val->getType(), 0),
        result_elem, "psv.to.unk");
    llvm::Value* result = llvm::UndefValue::get(result_struct);
    result = builder.CreateInsertValue(result, val, 0);
    result = builder.CreateInsertValue(result, unk, 1);
    return result;
  }

  if (target_type->isIntegerTy()) {
    llvm::Value* val = rval.val;
    if (rval.is_four_state && rval.unk != nullptr) {
      auto* not_unk = builder.CreateNot(rval.unk, "psv.to.notunk");
      val = builder.CreateAnd(val, not_unk, "psv.to.known");
    }
    return builder.CreateZExtOrTrunc(val, target_type, "psv.to.ext");
  }

  throw common::InternalError(
      "ConvertPackedRValueToLegacyLlvmValue", "unexpected target type");
}

}  // namespace lyra::lowering::mir_to_llvm
