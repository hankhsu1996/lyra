#pragma once

namespace lyra {

// Array query functions (IEEE 1800-2023 20.7)
// These may fold to constant or require runtime evaluation.
enum class ArrayQuerySysFnKind {
  // Dimension functions (accept optional dim argument, default = 1)
  kLeft,
  kRight,
  kLow,
  kHigh,
  kIncrement,
  kSize,
  // Dimensions functions (no dim argument, always fold to constant)
  kDimensions,
  kUnpackedDimensions,
};

}  // namespace lyra
