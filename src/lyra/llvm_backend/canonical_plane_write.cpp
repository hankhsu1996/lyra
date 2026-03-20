#include "lyra/llvm_backend/canonical_plane_write.hpp"

#include <cstdint>
#include <format>

#include <llvm/ADT/APInt.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Alignment.h>

#include "lyra/common/internal_error.hpp"

namespace lyra::lowering::mir_to_llvm {

namespace {

// Validate intent invariants at the public boundary.
void ValidateIntent(const PlaneConstantIntent& intent, const char* caller) {
  if (intent.storage_byte_size == 0) {
    throw common::InternalError(caller, "storage_byte_size must be non-zero");
  }
  if (intent.kind == PlaneConstantKind::kLowBitsSetMask) {
    if (intent.semantic_bit_width == 0) {
      throw common::InternalError(
          caller, "kLowBitsSetMask requires non-zero semantic_bit_width");
    }
    if (intent.semantic_bit_width > intent.storage_byte_size * 8) {
      throw common::InternalError(
          caller, std::format(
                      "semantic_bit_width {} exceeds storage capacity {} bits",
                      intent.semantic_bit_width, intent.storage_byte_size * 8));
    }
  }
}

// Typed constant store for small plane writes (storage_byte_size <= 8).
void EmitSmallConstantPlaneWrite(
    llvm::IRBuilderBase& builder, llvm::Value* dest_ptr, uint64_t byte_offset,
    const PlaneConstantIntent& intent) {
  if (intent.storage_byte_size > 8) {
    throw common::InternalError(
        "EmitSmallConstantPlaneWrite",
        std::format(
            "storage byte size {} exceeds small-write limit of 8",
            intent.storage_byte_size));
  }

  auto& ctx = builder.getContext();
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  uint32_t storage_bits = intent.storage_byte_size * 8;
  auto* store_ty = llvm::IntegerType::get(ctx, storage_bits);

  auto* ptr = builder.CreateGEP(i8_ty, dest_ptr, builder.getInt64(byte_offset));

  switch (intent.kind) {
    case PlaneConstantKind::kZero: {
      if (intent.already_zeroed) return;
      builder.CreateStore(llvm::ConstantInt::get(store_ty, 0), ptr);
      return;
    }
    case PlaneConstantKind::kLowBitsSetMask: {
      auto mask =
          llvm::APInt::getLowBitsSet(storage_bits, intent.semantic_bit_width);
      builder.CreateStore(llvm::ConstantInt::get(store_ty, mask), ptr);
      return;
    }
    case PlaneConstantKind::kRepeatedByte: {
      uint64_t pattern = 0;
      for (uint32_t i = 0; i < intent.storage_byte_size; ++i) {
        pattern |= static_cast<uint64_t>(intent.repeated_byte) << (i * 8);
      }
      builder.CreateStore(llvm::ConstantInt::get(store_ty, pattern), ptr);
      return;
    }
  }
}

// Emit a low-bits-set mask for large widths using byte-oriented operations.
// Returns the number of meaningful bytes written (for storage-tail
// finalization by the caller).
auto EmitLargeLowBitsSetMask(
    llvm::IRBuilderBase& builder, llvm::Value* plane_ptr,
    const PlaneConstantIntent& intent) -> uint32_t {
  auto& ctx = builder.getContext();
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);

  uint32_t full_bytes = intent.semantic_bit_width / 8;
  uint32_t remainder_bits = intent.semantic_bit_width % 8;
  uint32_t meaningful_bytes = full_bytes + (remainder_bits != 0 ? 1 : 0);

  if (full_bytes > 0) {
    builder.CreateMemSet(
        plane_ptr, builder.getInt8(0xFF), full_bytes, llvm::Align(1));
  }

  if (remainder_bits != 0) {
    auto* partial_ptr = builder.CreateGEP(
        i8_ty, plane_ptr, builder.getInt64(full_bytes), "mask.partial");
    auto partial_mask = static_cast<uint8_t>((1U << remainder_bits) - 1);
    builder.CreateStore(
        llvm::ConstantInt::get(i8_ty, partial_mask), partial_ptr);
  }

  return meaningful_bytes;
}

// Byte-oriented constant plane write for large widths (storage_byte_size > 8).
void EmitLargeConstantPlaneWrite(
    llvm::IRBuilderBase& builder, llvm::Value* dest_ptr, uint64_t byte_offset,
    const PlaneConstantIntent& intent) {
  if (intent.storage_byte_size <= 8) {
    throw common::InternalError(
        "EmitLargeConstantPlaneWrite",
        std::format(
            "storage byte size {} must exceed 8 for large write",
            intent.storage_byte_size));
  }

  auto& ctx = builder.getContext();
  auto* i8_ty = llvm::Type::getInt8Ty(ctx);
  auto* plane_ptr =
      builder.CreateGEP(i8_ty, dest_ptr, builder.getInt64(byte_offset));

  switch (intent.kind) {
    case PlaneConstantKind::kZero: {
      if (intent.already_zeroed) return;
      builder.CreateMemSet(
          plane_ptr, builder.getInt8(0), intent.storage_byte_size,
          llvm::Align(1));
      return;
    }
    case PlaneConstantKind::kLowBitsSetMask: {
      uint32_t bytes_written =
          EmitLargeLowBitsSetMask(builder, plane_ptr, intent);
      if (bytes_written < intent.storage_byte_size) {
        uint32_t tail_bytes = intent.storage_byte_size - bytes_written;
        auto* tail_ptr = builder.CreateGEP(
            i8_ty, plane_ptr, builder.getInt64(bytes_written), "mask.tail");
        builder.CreateMemSet(
            tail_ptr, builder.getInt8(0), tail_bytes, llvm::Align(1));
      }
      return;
    }
    case PlaneConstantKind::kRepeatedByte: {
      builder.CreateMemSet(
          plane_ptr, builder.getInt8(intent.repeated_byte),
          intent.storage_byte_size, llvm::Align(1));
      return;
    }
  }
}

}  // namespace

void EmitCanonicalPlaneWrite(
    llvm::IRBuilderBase& builder, llvm::Value* dest_ptr, uint64_t byte_offset,
    const PlaneConstantIntent& intent) {
  ValidateIntent(intent, "EmitCanonicalPlaneWrite");

  if (intent.storage_byte_size <= 8) {
    EmitSmallConstantPlaneWrite(builder, dest_ptr, byte_offset, intent);
  } else {
    EmitLargeConstantPlaneWrite(builder, dest_ptr, byte_offset, intent);
  }
}

}  // namespace lyra::lowering::mir_to_llvm
