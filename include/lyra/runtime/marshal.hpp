#pragma once

#include <cstdint>
#include <string>

#include "lyra/common/format.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/engine.hpp"
#include "lyra/semantic/format.hpp"
#include "lyra/semantic/value.hpp"

namespace lyra::runtime {

// Describes how runtime should interpret raw bytes at the ABI boundary.
// This is orthogonal to FormatKind (which controls rendering after decode).
enum class RuntimeValueKind : int32_t {
  kIntegral = 0,  // Inline 2-state integral, width <= 64 bits (validated)
  kReal64 = 1,    // double (64-bit IEEE 754)
  kReal32 = 2,    // float (32-bit IEEE 754, shortreal)
  kWideIntegral =
      3,  // width > 64, data → uint64_t[n] (little-endian), 8-byte aligned
  // Future: kIntegral4State, kWideIntegral4State, kStringHandle, ...
};

// Format a runtime value from raw parameters.
// This is the shared core for LyraPrintValue and LyraStringFormatValue.
//
// Parameters match the LLVM ABI for value formatting:
//   format: FormatKind enum value (how to format the output)
//   value_kind: RuntimeValueKind enum value (how to interpret the bytes)
//   data: pointer to raw data (integral, real, or C string for kString)
//   width: bit width for integrals (must be <= 64), ignored for reals
//   is_signed: signedness for decimal formatting
//   output_width: field width (-1 = auto, 0 = minimal, >0 = explicit)
//   precision: decimal precision for reals (-1 = default)
//   zero_pad, left_align: formatting flags
//   engine_ptr: pointer to Engine (required for kTime, can be nullptr for
//   others) module_timeunit_power: timeunit of the value (for kTime: e.g., -9
//   for ns)
//
// Returns the formatted string.
inline auto FormatRuntimeValue(
    FormatKind kind, RuntimeValueKind value_kind, const void* data,
    int32_t width, bool is_signed, int32_t output_width, int32_t precision,
    bool zero_pad, bool left_align, void* engine_ptr = nullptr,
    int8_t module_timeunit_power = -9) -> std::string {
  // Handle string specially - data IS the string pointer
  if (kind == FormatKind::kString) {
    return static_cast<const char*>(data);
  }

  // Handle time format - requires engine for formatting state
  if (kind == FormatKind::kTime) {
    auto* engine = static_cast<Engine*>(engine_ptr);
    if (engine == nullptr) {
      throw common::InternalError(
          "FormatRuntimeValue", "kTime format requires non-null engine_ptr");
    }
    auto time_value = *static_cast<const uint64_t*>(data);
    const auto& tf = engine->GetTimeFormat();

    // Width precedence: specifier width overrides $timeformat min_width
    int effective_min_width = (output_width >= 0) ? output_width : tf.min_width;

    // Convert time_value from module's timeunit to display units:
    // time_value is in module_timeunit_power units (e.g., if -9, it's in ns)
    // tf.units is the target unit (e.g., if -12, display in ps)
    // Formula: display_value = time_value * 10^(module_timeunit_power -
    // tf.units)
    return tf.FormatWithWidth(
        time_value, module_timeunit_power, effective_min_width);
  }

  // Build FormatSpec from parameters
  semantic::FormatSpec spec{
      .kind = kind,
      .width = output_width >= 0 ? std::optional(output_width) : std::nullopt,
      .precision = precision >= 0 ? std::optional(precision) : std::nullopt,
      .zero_pad = zero_pad,
      .left_align = left_align,
  };

  // Marshal data to RuntimeValue based on value_kind (byte interpretation),
  // not format specifier (kind). This allows real values to be formatted
  // with integer specifiers (%d with real → convert to int first).
  semantic::RuntimeValue value;
  switch (value_kind) {
    case RuntimeValueKind::kReal64:
      value = semantic::MakeReal(*static_cast<const double*>(data));
      break;
    case RuntimeValueKind::kReal32:
      value = semantic::MakeReal(
          static_cast<double>(*static_cast<const float*>(data)));
      break;
    case RuntimeValueKind::kIntegral: {
      if (width > 64) {
        throw common::InternalError(
            "FormatRuntimeValue", "kIntegral requires width <= 64");
      }
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
      break;
    }
    case RuntimeValueKind::kWideIntegral: {
      if (width <= 64) {
        throw common::InternalError(
            "FormatRuntimeValue", "kWideIntegral requires width > 64");
      }
      if (data == nullptr) {
        throw common::InternalError(
            "FormatRuntimeValue", "kWideIntegral requires non-null data");
      }
      size_t num_words = (static_cast<size_t>(width) + 63) / 64;
      value = semantic::MakeIntegralWide(
          static_cast<const uint64_t*>(data), num_words,
          static_cast<uint32_t>(width));
      break;
    }
  }

  return semantic::FormatValue(value, spec, is_signed);
}

}  // namespace lyra::runtime
