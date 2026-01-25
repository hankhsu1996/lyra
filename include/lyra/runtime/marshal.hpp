#pragma once

#include <cstdint>
#include <string>

#include "lyra/common/format.hpp"
#include "lyra/semantic/format.hpp"
#include "lyra/semantic/value.hpp"

namespace lyra::runtime {

// Format a runtime value from raw parameters.
// This is the shared core for LyraPrintValue and LyraStringFormatValue.
//
// Parameters match the LLVM ABI for value formatting:
//   format: FormatKind enum value
//   data: pointer to raw data (integral, real, or C string for kString)
//   width: bit width for integrals, 32/64 for float/double
//   is_signed: signedness for decimal formatting
//   output_width: field width (-1 = auto, 0 = minimal, >0 = explicit)
//   precision: decimal precision for reals (-1 = default)
//   zero_pad, left_align: formatting flags
//
// Returns the formatted string.
inline auto FormatRuntimeValue(
    FormatKind kind, const void* data, int32_t width, bool is_signed,
    int32_t output_width, int32_t precision, bool zero_pad, bool left_align)
    -> std::string {
  // Handle string specially - data IS the string pointer
  if (kind == FormatKind::kString) {
    return static_cast<const char*>(data);
  }

  // Build FormatSpec from parameters
  semantic::FormatSpec spec{
      .kind = kind,
      .width = output_width >= 0 ? std::optional(output_width) : std::nullopt,
      .precision = precision >= 0 ? std::optional(precision) : std::nullopt,
      .zero_pad = zero_pad,
      .left_align = left_align,
  };

  // Marshal data to RuntimeValue
  semantic::RuntimeValue value;
  if (kind == FormatKind::kReal) {
    if (width == 32) {  // shortreal (float)
      value = semantic::MakeReal(
          static_cast<double>(*static_cast<const float*>(data)));
    } else {
      value = semantic::MakeReal(*static_cast<const double*>(data));
    }
  } else {
    // Integral types: marshal based on width
    uint64_t raw_value = 0;
    if (width <= 8) {
      raw_value = *static_cast<const uint8_t*>(data);
    } else if (width <= 16) {
      raw_value = *static_cast<const uint16_t*>(data);
    } else if (width <= 32) {
      raw_value = *static_cast<const uint32_t*>(data);
    } else {
      raw_value = *static_cast<const uint64_t*>(data);
    }
    // Mask to actual width
    if (width < 64) {
      raw_value &= (1ULL << width) - 1;
    }
    value = semantic::MakeIntegral(raw_value, static_cast<uint32_t>(width));
  }

  return semantic::FormatValue(value, spec, is_signed);
}

}  // namespace lyra::runtime
