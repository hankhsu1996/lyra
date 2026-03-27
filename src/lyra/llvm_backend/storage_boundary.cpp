#include "lyra/llvm_backend/storage_boundary.hpp"

#include <format>

#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/overloaded.hpp"
#include "lyra/llvm_backend/canonical_plane_write.hpp"
#include "lyra/llvm_backend/layout/storage_contract.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// LLVM integer type for one lane of canonical packed storage.
// Width is the storage lane width (power-of-2 rounded), not the semantic
// bit width. For a 65-bit packed value, this returns i72 (9 bytes * 8).
auto GetLaneIntType(llvm::LLVMContext& ctx, uint32_t bit_width)
    -> llvm::IntegerType* {
  uint32_t storage_bits = GetStorageByteSize(bit_width) * 8;
  return llvm::IntegerType::get(ctx, storage_bits);
}

// Assert that a packed value is at storage lane width.
// Hard invariant failure if not. Callers must have invoked
// LowerToStorageLaneWidth before reaching this point.
void AssertStorageLaneWidth(
    llvm::Value* value, const PackedStorageSpec& spec, llvm::LLVMContext& ctx,
    const char* caller) {
  auto* lane_ty = GetLaneIntType(ctx, spec.bit_width);

  if (spec.is_four_state) {
    auto* st = llvm::dyn_cast<llvm::StructType>(value->getType());
    if (st == nullptr || st->getNumElements() != 2 ||
        st->getElementType(0) != lane_ty || st->getElementType(1) != lane_ty) {
      throw common::InternalError(
          caller, std::format(
                      "packed 4-state value not at storage lane width -- "
                      "expected {{i{0}, i{0}}}, caller must invoke "
                      "LowerToStorageLaneWidth first",
                      lane_ty->getBitWidth()));
    }
  } else {
    if (value->getType() != lane_ty) {
      throw common::InternalError(
          caller, std::format(
                      "packed 2-state value not at storage lane width -- "
                      "expected i{}, caller must invoke "
                      "LowerToStorageLaneWidth first",
                      lane_ty->getBitWidth()));
    }
  }
}

// Assert that a float value matches its spec.
void AssertFloatValueType(
    llvm::Value* value, const FloatStorageSpec& spec, llvm::LLVMContext& ctx,
    const char* caller) {
  auto* expected_ty = spec.layout.total_byte_size == 4
                          ? llvm::Type::getFloatTy(ctx)
                          : llvm::Type::getDoubleTy(ctx);
  if (value->getType() != expected_ty) {
    throw common::InternalError(caller, "float spec/value type mismatch");
  }
}

}  // namespace

auto LowerToStorageLaneWidth(
    llvm::IRBuilderBase& builder, llvm::Value* value,
    const PackedStorageSpec& spec) -> llvm::Value* {
  auto& ctx = builder.getContext();
  auto* lane_ty = GetLaneIntType(ctx, spec.bit_width);

  if (spec.is_four_state) {
    // SSA contract: 4-state packed values are {iN, iN} structs.
    auto* st = llvm::dyn_cast<llvm::StructType>(value->getType());
    if (st == nullptr || st->getNumElements() != 2 ||
        !st->getElementType(0)->isIntegerTy() ||
        !st->getElementType(1)->isIntegerTy()) {
      throw common::InternalError(
          "LowerToStorageLaneWidth",
          "4-state packed value must be {iN, iN} SSA struct");
    }
    if (st->getElementType(0) == lane_ty) {
      return value;
    }
    auto* val = builder.CreateExtractValue(value, 0);
    auto* unk = builder.CreateExtractValue(value, 1);
    val = builder.CreateZExtOrTrunc(val, lane_ty, "lower.val");
    unk = builder.CreateZExtOrTrunc(unk, lane_ty, "lower.unk");
    auto* struct_ty = llvm::StructType::get(ctx, {lane_ty, lane_ty});
    llvm::Value* result = llvm::UndefValue::get(struct_ty);
    result = builder.CreateInsertValue(result, val, 0);
    result = builder.CreateInsertValue(result, unk, 1);
    return result;
  }

  // SSA contract: 2-state packed values are integers.
  if (!value->getType()->isIntegerTy()) {
    throw common::InternalError(
        "LowerToStorageLaneWidth", "2-state packed value must be integer");
  }
  if (value->getType() == lane_ty) {
    return value;
  }
  return builder.CreateZExtOrTrunc(value, lane_ty, "lower.2s");
}

auto EmitLoadFromCanonicalStorage(
    llvm::IRBuilderBase& builder, llvm::Value* slot_ptr,
    const SlotStorageSpec& spec) -> llvm::Value* {
  auto& ctx = builder.getContext();
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);

  return std::visit(
      common::Overloaded{
          [&](const PackedStorageSpec& s) -> llvm::Value* {
            auto* lane_ty = GetLaneIntType(ctx, s.bit_width);
            if (!s.is_four_state) {
              return builder.CreateLoad(lane_ty, slot_ptr, "load.2s");
            }
            auto* known_ptr = slot_ptr;
            auto* unknown_ptr = builder.CreateGEP(
                i8_ty, slot_ptr, builder.getInt64(s.UnknownLaneOffset()),
                "unk_lane_ptr");

            auto* known = builder.CreateLoad(lane_ty, known_ptr, "load.val");
            auto* unknown =
                builder.CreateLoad(lane_ty, unknown_ptr, "load.unk");

            auto* struct_ty = llvm::StructType::get(ctx, {lane_ty, lane_ty});
            llvm::Value* result = llvm::UndefValue::get(struct_ty);
            result = builder.CreateInsertValue(result, known, 0);
            result = builder.CreateInsertValue(result, unknown, 1);
            return result;
          },
          [&](const FloatStorageSpec& s) -> llvm::Value* {
            auto* fp_ty = s.layout.total_byte_size == 4
                              ? llvm::Type::getFloatTy(ctx)
                              : llvm::Type::getDoubleTy(ctx);
            return builder.CreateLoad(fp_ty, slot_ptr, "load.fp");
          },
          [&](const auto&) -> llvm::Value* {
            throw common::InternalError(
                "EmitLoadFromCanonicalStorage",
                "scalar storage boundary only -- aggregates use recursive "
                "projection through SlotStorageSpec");
          }},
      spec.data);
}

auto EmitStoreToCanonicalStorage(
    llvm::IRBuilderBase& builder, llvm::Value* slot_ptr, llvm::Value* value,
    const SlotStorageSpec& spec, const StorageSpecArena& arena) -> void {
  auto& ctx = builder.getContext();
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);

  std::visit(
      common::Overloaded{
          [&](const PackedStorageSpec& s) {
            AssertStorageLaneWidth(
                value, s, ctx, "EmitStoreToCanonicalStorage");
            if (!s.is_four_state) {
              builder.CreateStore(value, slot_ptr);
              return;
            }
            auto* known = builder.CreateExtractValue(value, 0, "store.val");
            auto* unknown = builder.CreateExtractValue(value, 1, "store.unk");

            builder.CreateStore(known, slot_ptr);
            auto* unknown_ptr = builder.CreateGEP(
                i8_ty, slot_ptr, builder.getInt64(s.UnknownLaneOffset()),
                "unk_lane_ptr");
            builder.CreateStore(unknown, unknown_ptr);
          },
          [&](const FloatStorageSpec& s) {
            AssertFloatValueType(value, s, ctx, "EmitStoreToCanonicalStorage");
            builder.CreateStore(value, slot_ptr);
          },
          [&](const ArrayStorageSpec& s) {
            const auto& elem_spec = arena.Get(s.element_spec_id);
            for (uint32_t i = 0; i < s.element_count; ++i) {
              auto* elem_ptr = builder.CreateGEP(
                  i8_ty, slot_ptr,
                  builder.getInt64(static_cast<uint64_t>(i) * s.element_stride),
                  "arr_elem_ptr");
              auto* elem_val = builder.CreateExtractValue(value, i);
              EmitStoreToCanonicalStorage(
                  builder, elem_ptr, elem_val, elem_spec, arena);
            }
          },
          [&](const StructStorageSpec& s) {
            for (uint32_t i = 0; i < s.fields.size(); ++i) {
              const auto& field = s.fields[i];
              const auto& field_spec = arena.Get(field.field_spec_id);
              auto* field_ptr = builder.CreateGEP(
                  i8_ty, slot_ptr, builder.getInt64(field.byte_offset),
                  "struct_field_ptr");
              auto* field_val = builder.CreateExtractValue(value, i);
              EmitStoreToCanonicalStorage(
                  builder, field_ptr, field_val, field_spec, arena);
            }
          },
          [&](const auto&) {
            throw common::InternalError(
                "EmitStoreToCanonicalStorage",
                "unsupported storage spec variant (union/handle)");
          }},
      spec.data);
}

auto EmitPackedToCanonicalBits(
    llvm::IRBuilderBase& builder, llvm::Value* value,
    const SlotStorageSpec& spec) -> llvm::Value* {
  auto& ctx = builder.getContext();

  return std::visit(
      common::Overloaded{
          [&](const PackedStorageSpec& s) -> llvm::Value* {
            AssertStorageLaneWidth(value, s, ctx, "EmitPackedToCanonicalBits");
            if (!s.is_four_state) {
              return value;
            }
            auto* lane_ty = GetLaneIntType(ctx, s.bit_width);
            unsigned lane_bits = lane_ty->getBitWidth();
            auto* wide_ty = llvm::Type::getIntNTy(ctx, lane_bits * 2);

            auto* known = builder.CreateExtractValue(value, 0, "canon.val");
            auto* unknown = builder.CreateExtractValue(value, 1, "canon.unk");

            auto* val_ext = builder.CreateZExt(known, wide_ty, "canon.val.ext");
            auto* unk_ext =
                builder.CreateZExt(unknown, wide_ty, "canon.unk.ext");
            auto* unk_shifted = builder.CreateShl(
                unk_ext, llvm::ConstantInt::get(wide_ty, lane_bits),
                "canon.unk.shl");

            return builder.CreateOr(val_ext, unk_shifted, "canon.bits");
          },
          [&](const FloatStorageSpec& s) -> llvm::Value* {
            AssertFloatValueType(value, s, ctx, "EmitPackedToCanonicalBits");
            auto* int_ty =
                llvm::Type::getIntNTy(ctx, s.layout.total_byte_size * 8);
            return builder.CreateBitCast(value, int_ty, "canon.fp");
          },
          [&](const auto&) -> llvm::Value* {
            throw common::InternalError(
                "EmitPackedToCanonicalBits",
                "scalar storage boundary only -- aggregates materialize "
                "canonical bytes through SlotStorageSpec recursion");
          }},
      spec.data);
}

void EmitStoreFourStateX(
    llvm::IRBuilderBase& builder, llvm::Value* slot_ptr, uint32_t bit_width) {
  uint32_t lane_bytes = GetStorageByteSize(bit_width);

  // Value plane: zero.
  EmitCanonicalPlaneWrite(
      builder, slot_ptr, 0,
      {.kind = PlaneConstantKind::kZero,
       .semantic_bit_width = bit_width,
       .storage_byte_size = lane_bytes});

  // Unknown plane: low-bits-set mask (X-encoding).
  EmitCanonicalPlaneWrite(
      builder, slot_ptr, FourStateUnknownLaneOffset(bit_width),
      {.kind = PlaneConstantKind::kLowBitsSetMask,
       .semantic_bit_width = bit_width,
       .storage_byte_size = lane_bytes});
}

void EmitStoreUnknownMask(
    llvm::IRBuilderBase& builder, llvm::Value* slot_ptr, uint32_t bit_width) {
  uint32_t lane_bytes = GetStorageByteSize(bit_width);

  EmitCanonicalPlaneWrite(
      builder, slot_ptr, FourStateUnknownLaneOffset(bit_width),
      {.kind = PlaneConstantKind::kLowBitsSetMask,
       .semantic_bit_width = bit_width,
       .storage_byte_size = lane_bytes});
}

auto EmitLoadFourStateFromCanonical(
    llvm::IRBuilderBase& builder, llvm::LLVMContext& llvm_ctx,
    llvm::Value* slot_ptr, uint32_t bit_width) -> llvm::Value* {
  auto* i8_ty = llvm::Type::getInt8Ty(llvm_ctx);
  uint32_t storage_bits = GetStorageByteSize(bit_width) * 8;
  auto* lane_ty = llvm::IntegerType::get(llvm_ctx, storage_bits);

  auto* val = builder.CreateLoad(lane_ty, slot_ptr, "canon.val");
  auto* unk_ptr = builder.CreateGEP(
      i8_ty, slot_ptr, builder.getInt64(FourStateUnknownLaneOffset(bit_width)),
      "canon.unk.ptr");
  auto* unk = builder.CreateLoad(lane_ty, unk_ptr, "canon.unk");

  auto* struct_ty = llvm::StructType::get(llvm_ctx, {lane_ty, lane_ty});
  llvm::Value* result = llvm::UndefValue::get(struct_ty);
  result = builder.CreateInsertValue(result, val, 0);
  result = builder.CreateInsertValue(result, unk, 1);
  return result;
}

auto GetCanonicalLaneBits(SemanticBits bits) -> LaneBits {
  return LaneBits::FromRaw(GetStorageByteSize(bits.Raw()) * 8);
}

auto GetBackingBits(SemanticBits bits) -> BackingBits {
  // Backing-domain rounding matches GetBackingLlvmType:
  // power-of-2 up to 64 bits, exact semantic width above 64 bits.
  uint32_t w = bits.Raw();
  if (w <= 8) return BackingBits::FromRaw(8);
  if (w <= 16) return BackingBits::FromRaw(16);
  if (w <= 32) return BackingBits::FromRaw(32);
  if (w <= 64) return BackingBits::FromRaw(64);
  return BackingBits::FromRaw(w);
}

auto GetLaneIntType(llvm::LLVMContext& ctx, LaneBits bits)
    -> llvm::IntegerType* {
  return llvm::IntegerType::get(ctx, bits.Raw());
}

auto NormalizeToLaneWidth(
    llvm::IRBuilder<>& builder, llvm::Value* raw, LaneBits target)
    -> LaneValue {
  auto* lane_ty = GetLaneIntType(builder.getContext(), target);
  auto* normalized = builder.CreateZExtOrTrunc(raw, lane_ty, "lane.norm");
  return LaneValue::FromNormalized(normalized, target);
}

auto ConvertLaneToBackingWidth(
    llvm::IRBuilder<>& builder, LaneValue val, llvm::Type* backing_ty)
    -> llvm::Value* {
  return builder.CreateZExtOrTrunc(val.Raw(), backing_ty, "lane.back");
}

}  // namespace lyra::lowering::mir_to_llvm
