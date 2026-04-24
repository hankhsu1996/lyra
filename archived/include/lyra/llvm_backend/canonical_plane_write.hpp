#pragma once

#include <cstdint>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

namespace lyra::lowering::mir_to_llvm {

// Classification of constant/pattern values for canonical plane writes.
// Runtime values are excluded by design -- they use separate APIs.
enum class PlaneConstantKind {
  // All-zero fill (or skip if already zeroed).
  kZero,
  // Low-bits-set mask: bits [0, semantic_bit_width) are 1, rest are 0.
  // Used for 4-state unknown-plane X-encoding.
  kLowBitsSetMask,
  // Repeated single-byte pattern fill.
  kRepeatedByte,
};

// Intent descriptor for constant/pattern canonical plane writes.
// Runtime values are not representable in this struct -- they use separate
// APIs. Fields are kind-specific; unused fields are ignored for each kind.
struct PlaneConstantIntent {
  PlaneConstantKind kind = PlaneConstantKind::kZero;
  uint32_t semantic_bit_width = 0;
  uint32_t storage_byte_size = 0;

  // kZero only: if true, destination is already zeroed -- skip store.
  bool already_zeroed = false;

  // kRepeatedByte only: the byte value to fill.
  uint8_t repeated_byte = 0;
};

// Sole gateway for writing constant/pattern data into canonical packed plane
// memory. Owns all lowering policy: small widths (<= 8 bytes) use typed
// stores; large widths (> 8 bytes) use byte-oriented memset/memcpy.
//
// byte_offset: offset from dest_ptr to the target plane. 0 for value plane,
// FourStateUnknownLaneOffset(bit_width) for unknown plane.
//
//
// This is the only public API for writing constant plane values. No other
// public helper may accept large constant plane values as raw llvm::Value*.
void EmitCanonicalPlaneWrite(
    llvm::IRBuilderBase& builder, llvm::Value* dest_ptr, uint64_t byte_offset,
    const PlaneConstantIntent& intent);

}  // namespace lyra::lowering::mir_to_llvm
