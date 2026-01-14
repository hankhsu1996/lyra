#pragma once

#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/common/display_variant.hpp"
#include "lyra/common/format_string.hpp"
#include "lyra/common/time_format.hpp"
#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::interpreter {

/// Format specifier parsed from a format string.
struct FormatSpec {
  char spec{};
  bool zero_pad = false;
  bool left_align = false;
  std::string width;
  std::string precision;
};

/// Context for time formatting (%t specifier).
struct TimeFormatContext {
  common::TimeFormatState time_format;
  int8_t module_unit_power = 0;  // Module's timeunit (e.g., -9 for 1ns)
  int8_t global_precision_power = 0;
};

/// Format a RuntimeValue according to a format specifier.
/// spec: 'd' = decimal, 'x'/'h' = hex, 'b' = binary, 'o' = octal,
/// 's' = string, 'f' = real
auto FormatValue(const RuntimeValue& value, const FormatSpec& spec)
    -> std::string;

/// Parse SV format string and format arguments.
/// Returns formatted output string.
/// time_ctx: Optional context for %t formatting.
auto FormatDisplay(
    const std::string& fmt_str, const std::vector<RuntimeValue>& args,
    const TimeFormatContext* time_ctx = nullptr) -> std::string;

/// Extract format string info from a RuntimeValue.
/// The is_string_literal flag comes from the instruction's
/// format_string_is_literal field.
auto ExtractFormatString(const RuntimeValue& val, bool is_string_literal)
    -> common::FormatStringInfo;

/// Get format specifier for a single value based on its type.
auto GetFormatSpecifier(const RuntimeValue& val, char default_format) -> char;

/// Build format string from a list of values using default format for
/// integrals.
auto BuildFormatString(
    const std::vector<RuntimeValue>& values, char default_format)
    -> std::string;

/// Unified format message function for display-like system tasks.
/// Handles three cases:
/// 1. String literal with format specifiers: use as format string
/// 2. String literal without format specifiers: prefix + formatted args
/// 3. No string literal: format all args with default_format
auto FormatMessage(
    std::span<const RuntimeValue> arg_values, bool first_is_string_literal,
    char default_format, const TimeFormatContext* time_ctx) -> std::string;

/// Convert integral RuntimeValue to string per LRM 6.16.
/// Each 8 bits forms one character, MSB first, null bytes are skipped.
auto IntegralToString(const RuntimeValue& val) -> std::string;

/// Extract int64 value from RuntimeValue, handling both narrow and wide
/// sources. For wide sources, extracts the low 64 bits.
auto ExtractInt64FromSource(const RuntimeValue& src) -> int64_t;

}  // namespace lyra::interpreter
