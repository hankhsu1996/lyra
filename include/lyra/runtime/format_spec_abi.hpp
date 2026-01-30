#pragma once

#include <cstdint>

namespace lyra::runtime {

// C-ABI stable format specification for runtime calls.
// This is the wire format passed across the LLVM->runtime boundary.
// Sentinel: -1 means "unspecified" for width/precision.
//
// Layout is explicitly fixed for ABI stability between LLVM codegen and
// runtime.
struct LyraFormatSpec {
  int32_t kind;       // FormatKind enum value
  int32_t width;      // -1 = unspecified, 0 = minimal, >0 = explicit
  int32_t precision;  // -1 = unspecified
  uint8_t flags;      // Bit flags (see kFormatFlag* constants)
  // NOLINTNEXTLINE(*-avoid-c-arrays) - C ABI requires C array
  uint8_t reserved[3];  // Padding for future expansion
};

// ABI stability guarantees
static_assert(sizeof(LyraFormatSpec) == 16, "LyraFormatSpec ABI size changed");
static_assert(
    alignof(LyraFormatSpec) == 4, "LyraFormatSpec ABI alignment changed");

// Flag bit positions
constexpr uint8_t kFormatFlagZeroPad = 0x01;
constexpr uint8_t kFormatFlagLeftAlign = 0x02;

}  // namespace lyra::runtime
