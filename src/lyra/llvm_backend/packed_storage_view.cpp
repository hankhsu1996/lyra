#include "lyra/llvm_backend/packed_storage_view.hpp"

#include <cstdint>
#include <expected>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

#include "lyra/common/constant.hpp"
#include "lyra/common/integral_constant.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/type_queries.hpp"
#include "lyra/llvm_backend/compute/operand.hpp"
#include "lyra/llvm_backend/context.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"
#include "lyra/llvm_backend/type_query.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/place_type.hpp"

namespace lyra::lowering::mir_to_llvm {

// BitRangeProjection-specific byte-alignment proof.
//
// A BitRangeProjection's bit_offset is a MIR operand that may be:
// - A dynamic expression like (index * element_width) from packed array
//   element access, where the offset is always a multiple of element_width.
// - A constant offset from a fixed range/part select like data[11:4].
//
// For constant offsets, we check the actual value.
// For dynamic offsets, the proof holds when offset = index * element_width
// and element_width is a multiple of 8. This covers packed array element
// access but NOT arbitrary dynamic part-selects.
//
// This is NOT a general alignment rule for all future step kinds.
auto IsBitRangeStepProvablyByteAligned(const mir::BitRangeProjection& br)
    -> bool {
  if (br.width % 8 != 0) return false;

  // Check if the offset is a compile-time constant.
  if (const auto* constant = std::get_if<Constant>(&br.bit_offset.payload)) {
    const auto* integral = std::get_if<IntegralConstant>(&constant->value);
    if (integral == nullptr || integral->value.empty()) return false;
    // For constants, check the actual bit offset value.
    return (integral->value[0] % 8) == 0;
  }

  // Dynamic offset: assume byte-aligned when width is a multiple of 8.
  // This is correct for packed array element access where the MIR offset
  // is (index * element_width). Not correct for arbitrary dynamic
  // part-selects -- those would need a more specific proof.
  return true;
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
  bool is_four_state =
      IsPackedFourState(base_type, types, ctx.IsForceTwoState());
  uint32_t value_plane_bytes = GetStorageByteSize(total_bits);

  // GetPlacePointer skips BitRangeProjection suffix.
  auto ptr_result = ctx.GetPlacePointer(place_id);
  if (!ptr_result) return std::unexpected(ptr_result.error());

  return PackedStorageView{
      .base_ptr = *ptr_result,
      .total_semantic_bits = total_bits,
      .value_plane_bytes = value_plane_bytes,
      .unk_plane_offset_bytes =
          is_four_state ? FourStateUnknownLaneOffset(total_bits) : 0,
      .is_four_state = is_four_state,
  };
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

  // Byte-addressable requires BOTH:
  // 1. The subview width is a multiple of 8.
  // 2. The composed bit offset is provably a multiple of 8 at all runtime
  //    values. Currently proven only by BitRangeProjection-specific
  //    analysis (IsBitRangeStepProvablyByteAligned). Future packed struct
  //    field and part-select steps will need their own proofs.
  if (final_width % 8 == 0 && all_steps_byte_aligned) {
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
// i8 GEP. Both byte-addressable and bit-addressable paths use this to
// ensure uniform pointer discipline over canonical packed storage.
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
  result.storage_bytes = access.storage_byte_width;
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
//
// Current fallback implementation for kBitAddressable subviews. Loads the
// full monolithic packed value from canonical storage and extracts via
// shift-mask. This is NOT the intended long-term shape for mixed or
// localized-refinement cases -- future work should introduce intermediate
// byte-addressable base localization before bit refinement.
auto EmitBitAddressableLoad(Context& ctx, const PackedSubviewAccess& access)
    -> PackedRValue {
  auto& builder = ctx.GetBuilder();

  uint32_t base_storage_bits = access.storage.value_plane_bytes * 8;

  PackedRValue result;
  result.semantic_bits = access.semantic_bit_width;
  result.storage_bytes = GetStorageByteSize(access.semantic_bit_width);
  result.is_four_state = access.storage.is_four_state;

  uint32_t elem_storage_bits = result.storage_bytes * 8;

  if (access.storage.is_four_state) {
    // Load both planes via explicit GEP (uniform pointer discipline).
    auto* val_full = LoadPackedPlaneChunk(
        builder, ctx.GetLlvmContext(), access.storage.base_ptr, 0,
        base_storage_bits, "br.val.base.ptr");
    auto* unk_full = LoadPackedPlaneChunk(
        builder, ctx.GetLlvmContext(), access.storage.base_ptr,
        access.storage.unk_plane_offset_bytes, base_storage_bits,
        "br.unk.base.ptr");

    auto* lane_ty =
        llvm::IntegerType::get(ctx.GetLlvmContext(), base_storage_bits);
    auto* shift_amt = builder.CreateZExtOrTrunc(
        access.semantic_bit_offset, lane_ty, "br.offset");
    llvm::Value* val = builder.CreateLShr(val_full, shift_amt, "br.val.shr");
    llvm::Value* unk = builder.CreateLShr(unk_full, shift_amt, "br.unk.shr");

    if (elem_storage_bits < base_storage_bits) {
      auto* elem_ty =
          llvm::IntegerType::get(ctx.GetLlvmContext(), elem_storage_bits);
      val = builder.CreateTrunc(val, elem_ty, "br.val.trunc");
      unk = builder.CreateTrunc(unk, elem_ty, "br.unk.trunc");
    }

    if (access.semantic_bit_width < elem_storage_bits) {
      auto mask = llvm::APInt::getLowBitsSet(
          elem_storage_bits, access.semantic_bit_width);
      auto* mask_val = llvm::ConstantInt::get(
          llvm::IntegerType::get(ctx.GetLlvmContext(), elem_storage_bits),
          mask);
      val = builder.CreateAnd(val, mask_val, "br.val.mask");
      unk = builder.CreateAnd(unk, mask_val, "br.unk.mask");
    }

    result.val = val;
    result.unk = unk;
  } else {
    // 2-state: single plane via explicit GEP.
    auto* base = LoadPackedPlaneChunk(
        builder, ctx.GetLlvmContext(), access.storage.base_ptr, 0,
        base_storage_bits, "br.2s.base.ptr");

    auto* lane_ty =
        llvm::IntegerType::get(ctx.GetLlvmContext(), base_storage_bits);
    auto* shift_amt = builder.CreateZExtOrTrunc(
        access.semantic_bit_offset, lane_ty, "br.offset");
    llvm::Value* val = builder.CreateLShr(base, shift_amt, "br.shr");

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
          "br.mask");
    }

    result.val = val;
  }

  return result;
}

}  // namespace

auto EmitLoadFromPackedSubview(Context& ctx, const PackedSubviewAccess& access)
    -> Result<PackedRValue> {
  if (access.kind == PackedSubviewKind::kByteAddressable) {
    return EmitByteAddressableLoad(ctx, access);
  }
  return EmitBitAddressableLoad(ctx, access);
}

auto EmitStoreToPackedSubview(
    [[maybe_unused]] Context& ctx,
    [[maybe_unused]] const PackedSubviewAccess& access,
    [[maybe_unused]] const PackedRValue& value,
    [[maybe_unused]] const PackedStorePolicy& policy) -> Result<void> {
  throw common::InternalError(
      "EmitStoreToPackedSubview", "not yet implemented in new module");
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
