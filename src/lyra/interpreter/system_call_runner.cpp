#include "lyra/interpreter/system_call_runner.hpp"

#include <bit>
#include <cassert>
#include <cctype>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <iterator>
#include <memory>
#include <optional>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <fmt/core.h>
#include <fmt/format.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/format_string.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/mem_io.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/time_format.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/wide_bit_ops.hpp"
#include "lyra/interpreter/call_frame.hpp"
#include "lyra/interpreter/instance_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_frame.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/interpreter/temp_table.hpp"
#include "lyra/interpreter/variable_table.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/sdk/plusargs.hpp"
#include "lyra/sdk/time_utils.hpp"

namespace lyra::interpreter {

namespace {

struct FormatSpec {
  char spec{};
  bool zero_pad = false;
  bool left_align = false;
  std::string width;
  std::string precision;
};

// Properties for display/write variants
struct DisplayVariantProps {
  char default_format;  // 'd', 'b', 'o', or 'h'
  bool append_newline;  // true for $display*, false for $write*
};

// Get properties for a display/write/strobe variant
auto GetDisplayVariantProps(std::string_view name) -> DisplayVariantProps {
  if (name == "$write") {
    return {.default_format = 'd', .append_newline = false};
  }
  if (name == "$writeb") {
    return {.default_format = 'b', .append_newline = false};
  }
  if (name == "$writeo") {
    return {.default_format = 'o', .append_newline = false};
  }
  if (name == "$writeh") {
    return {.default_format = 'h', .append_newline = false};
  }
  if (name == "$displayb" || name == "$strobeb" || name == "$monitorb") {
    return {.default_format = 'b', .append_newline = true};
  }
  if (name == "$displayo" || name == "$strobeo" || name == "$monitoro") {
    return {.default_format = 'o', .append_newline = true};
  }
  if (name == "$displayh" || name == "$strobeh" || name == "$monitorh") {
    return {.default_format = 'h', .append_newline = true};
  }
  // $display, $strobe, $monitor (default)
  return {.default_format = 'd', .append_newline = true};
}

// Format a RuntimeValue according to a format specifier.
// spec: 'd' = decimal, 'x'/'h' = hex, 'b' = binary, 'o' = octal,
// 's' = string, 'f' = real
auto FormatValue(const RuntimeValue& value, const FormatSpec& spec)
    -> std::string {
  if (value.IsString()) {
    if (!spec.width.empty()) {
      std::string fmt = "{:";
      // SV default is right-align, but fmt::format default is left-align
      if (!spec.left_align) {
        fmt += ">";  // Right-align (SV default) in fmt::format
      }
      fmt += spec.width;
      fmt += "}";
      return fmt::format(fmt::runtime(fmt), value.AsString());
    }
    return value.AsString();
  }

  if (value.IsReal() || value.IsShortReal()) {
    if (spec.spec != 'f' && spec.spec != 'd' && spec.spec != 's') {
      throw DiagnosticException(
          Diagnostic::Error(
              {}, fmt::format("unsupported format specifier: %{}", spec.spec)));
    }
    // Convert shortreal to double for formatting
    double dval = value.IsReal() ? value.AsDouble()
                                 : static_cast<double>(value.AsFloat());
    if (spec.spec == 'f') {
      std::string fmt = "{:";
      if (spec.zero_pad && !spec.width.empty()) {
        fmt += "0>";
      }
      if (!spec.width.empty()) {
        fmt += spec.width;
      }
      if (!spec.precision.empty()) {
        fmt += ".";
        fmt += spec.precision;
      }
      fmt += "f}";
      return fmt::format(fmt::runtime(fmt), dval);
    }
    return value.ToString();
  }

  // Build format string with optional width for integer types
  auto build_int_format = [&spec](char type_char) -> std::string {
    std::string fmt = "{:";
    if (spec.zero_pad) {
      fmt += "0";
    }
    if (!spec.width.empty()) {
      fmt += spec.width;
    }
    fmt += type_char;
    fmt += "}";
    return fmt;
  };

  // Handle wide values (>64 bits)
  if (value.IsWide()) {
    const auto& wide = value.AsWideBit();
    switch (spec.spec) {
      case 'x':
      case 'h':
        return wide.ToHexString();
      case 'b':
        return wide.ToBinaryString();
      case 'o':
        return wide.ToOctalString();
      case 'd':
        return wide.ToDecimalString();
      default:
        return wide.ToHexString();
    }
  }

  auto narrow = value.AsNarrow();

  switch (spec.spec) {
    case 'x':
    case 'h': {
      // Unsigned for hex
      auto v = narrow.AsUInt64();
      return fmt::format(fmt::runtime(build_int_format('x')), v);
    }
    case 'b': {
      // Unsigned for binary
      auto v = narrow.AsUInt64();
      return fmt::format(fmt::runtime(build_int_format('b')), v);
    }
    case 'o': {
      // Unsigned for octal
      auto v = narrow.AsUInt64();
      return fmt::format(fmt::runtime(build_int_format('o')), v);
    }
    case 'd': {
      // Signed for decimal to preserve negative numbers
      auto v = narrow.AsInt64();
      return fmt::format(fmt::runtime(build_int_format('d')), v);
    }
    default:  // 's'
      if (!spec.width.empty()) {
        std::string fmt = "{:";
        // SV default is right-align, but fmt::format default is left-align
        if (!spec.left_align) {
          fmt += ">";  // Right-align (SV default) in fmt::format
        }
        fmt += spec.width;
        fmt += "}";
        return fmt::format(fmt::runtime(fmt), value.ToString());
      }
      return value.ToString();
  }
}

// Context for time formatting (%t specifier)
struct TimeFormatContext {
  common::TimeFormatState time_format;
  int8_t module_unit_power = 0;  // Module's timeunit (e.g., -9 for 1ns)
  int8_t global_precision_power = 0;
};

// Parse SV format string and format arguments
// Returns formatted output string
// time_ctx: Optional context for %t formatting
auto FormatDisplay(
    const std::string& fmt_str, const std::vector<RuntimeValue>& args,
    const TimeFormatContext* time_ctx = nullptr) -> std::string {
  std::string result;
  size_t arg_idx = 0;
  size_t i = 0;

  while (i < fmt_str.size()) {
    if (fmt_str[i] == '%') {
      if (i + 1 >= fmt_str.size()) {
        throw DiagnosticException(
            Diagnostic::Error({}, "invalid format string: trailing %"));
      }
      if (fmt_str[i + 1] == '%') {
        result += '%';
        i += 2;
      } else {
        FormatSpec spec;
        ++i;  // Consume '%'

        // Check for left-align flag '-'
        if (i < fmt_str.size() && fmt_str[i] == '-') {
          spec.left_align = true;
          ++i;
        }

        if (i < fmt_str.size() && fmt_str[i] == '0') {
          spec.zero_pad = true;
          ++i;
        }

        while (i < fmt_str.size() && (std::isdigit(fmt_str[i]) != 0)) {
          spec.width += fmt_str[i];
          ++i;
        }

        if (i < fmt_str.size() && fmt_str[i] == '.') {
          ++i;
          if (i >= fmt_str.size() || (std::isdigit(fmt_str[i]) == 0)) {
            throw DiagnosticException(
                Diagnostic::Error(
                    {}, "invalid format string: missing precision digits"));
          }
          while (i < fmt_str.size() && (std::isdigit(fmt_str[i]) != 0)) {
            spec.precision += fmt_str[i];
            ++i;
          }
        }

        if (i >= fmt_str.size()) {
          throw DiagnosticException(
              Diagnostic::Error({}, "invalid format string: trailing %"));
        }

        spec.spec = fmt_str[i];
        if (spec.spec != 'd' && spec.spec != 'h' && spec.spec != 'x' &&
            spec.spec != 'b' && spec.spec != 'o' && spec.spec != 's' &&
            spec.spec != 'f' && spec.spec != 't') {
          throw DiagnosticException(
              Diagnostic::Error(
                  {},
                  fmt::format("unsupported format specifier: %{}", spec.spec)));
        }

        // Precision is only valid for %f (floats)
        if (spec.spec != 'f' && !spec.precision.empty()) {
          throw DiagnosticException(
              Diagnostic::Error(
                  {}, fmt::format(
                          "unsupported format specifier: precision not "
                          "supported for "
                          "%{}",
                          spec.spec)));
        }

        if (arg_idx >= args.size()) {
          throw DiagnosticException(
              Diagnostic::Error({}, "not enough arguments for format string"));
        }

        // %t formats time value according to $timeformat settings
        if (spec.spec == 't') {
          if (time_ctx != nullptr) {
            // Format time value using $timeformat settings
            uint64_t time_val = args[arg_idx].AsNarrow().AsUInt64();
            result += time_ctx->time_format.FormatModuleTime(
                time_val, time_ctx->module_unit_power,
                time_ctx->global_precision_power);
          } else {
            // No time context - just format as decimal
            result += FormatValue(args[arg_idx], spec);
          }
          arg_idx++;
          ++i;
          continue;
        }

        result += FormatValue(args[arg_idx], spec);
        arg_idx++;
        ++i;
      }
    } else {
      result += fmt_str[i];
      i++;
    }
  }
  return result;
}

struct MemTargetInfo {
  bool is_unpacked = false;
  size_t element_width = 0;
  size_t element_count = 0;
  int32_t lower_bound = 0;
};

auto GetMemTargetInfo(const RuntimeValue& value, const common::Type& type)
    -> MemTargetInfo {
  if (type.kind == common::Type::Kind::kUnpackedArray) {
    const auto& array_data = std::get<common::UnpackedArrayData>(type.data);
    if (array_data.element_type->kind != common::Type::Kind::kIntegral) {
      throw common::InternalError(
          "interpreter", "readmem/writemem target element must be integral");
    }
    return {
        .is_unpacked = true,
        .element_width = array_data.element_type->GetBitWidth(),
        .element_count = value.AsArray().size(),
        .lower_bound = array_data.lower_bound,
    };
  }

  if (type.kind == common::Type::Kind::kIntegral) {
    const auto& integral = std::get<common::IntegralData>(type.data);
    if (integral.element_type != nullptr) {
      return {
          .is_unpacked = false,
          .element_width = integral.element_type->GetBitWidth(),
          .element_count = integral.element_count,
          .lower_bound = integral.element_lower,
      };
    }
    return {
        .is_unpacked = false,
        .element_width = integral.bit_width,
        .element_count = 1,
        .lower_bound = 0,
    };
  }

  throw common::InternalError(
      "interpreter", "readmem/writemem target must be an array");
}

auto ParseMemTokenToValue(std::string_view token, size_t bit_width, bool is_hex)
    -> RuntimeValue {
  auto words_result =
      common::mem_io::ParseMemTokenToWords(token, bit_width, is_hex);
  if (!words_result) {
    throw std::runtime_error(words_result.error());
  }
  auto words = std::move(*words_result);

  if (bit_width <= 64) {
    return RuntimeValue::IntegralUnsigned(
        words.empty() ? 0 : words[0], bit_width);
  }
  common::WideBit wide(std::move(words));
  return RuntimeValue::IntegralWide(std::move(wide), bit_width);
}

auto ExtractPackedElement(
    const RuntimeValue& value, size_t index, size_t element_width)
    -> RuntimeValue {
  size_t start_bit = index * element_width;
  if (element_width <= 64) {
    uint64_t mask = common::MakeBitMask(static_cast<uint32_t>(element_width));
    uint64_t extracted = 0;
    if (value.IsWide()) {
      auto shifted = value.AsWideBit().ShiftRightLogical(start_bit);
      extracted = shifted.GetWord(0) & mask;
    } else {
      extracted = (value.AsNarrow().AsUInt64() >> start_bit) & mask;
    }
    return RuntimeValue::IntegralUnsigned(extracted, element_width);
  }

  auto wide_value = value.IsWide() ? value.AsWideBit()
                                   : common::WideBit::FromUInt64(
                                         value.AsNarrow().AsUInt64(), 2);
  auto extracted = wide_value.ExtractSlice(start_bit, element_width);
  return RuntimeValue::IntegralWide(std::move(extracted), element_width);
}

auto StorePackedElement(
    const RuntimeValue& current, size_t index, size_t element_width,
    const RuntimeValue& new_value) -> RuntimeValue {
  const auto& current_data = std::get<common::IntegralData>(current.type.data);
  size_t storage_width = current_data.bit_width;
  bool storage_is_wide = current.IsWide();
  bool element_is_wide = element_width > 64;
  size_t shift = index * element_width;

  if (!storage_is_wide && !element_is_wide) {
    uint64_t elem_mask =
        common::MakeBitMask(static_cast<uint32_t>(element_width));
    uint64_t clear_mask = ~(elem_mask << shift);
    uint64_t merged = (current.AsNarrow().AsUInt64() & clear_mask) |
                      ((new_value.AsNarrow().AsUInt64() & elem_mask) << shift);
    return current_data.is_signed
               ? RuntimeValue::IntegralSigned(
                     static_cast<int64_t>(merged), storage_width)
               : RuntimeValue::IntegralUnsigned(merged, storage_width);
  }

  size_t storage_words = common::wide_ops::WordsForBits(storage_width);
  auto current_wide = storage_is_wide
                          ? current.AsWideBit()
                          : common::WideBit::FromUInt64(
                                current.AsNarrow().AsUInt64(), storage_words);
  auto value_wide = element_is_wide
                        ? new_value.AsWideBit()
                        : common::WideBit::FromUInt64(
                              new_value.AsNarrow().AsUInt64(), storage_words);
  auto merged = current_wide.InsertSlice(value_wide, shift, element_width);
  return RuntimeValue::IntegralWide(
      std::move(merged), storage_width, current_data.is_signed);
}

auto FormatMemValue(const RuntimeValue& value, size_t bit_width, bool is_hex)
    -> std::string {
  auto word_count = common::wide_ops::WordsForBits(bit_width);
  std::vector<uint64_t> words(word_count, 0);
  value.MatchIntegral(
      [&](RuntimeValue::NarrowIntegral n) {
        if (!words.empty()) {
          words[0] = n.AsUInt64();
        }
      },
      [&](const common::WideBit& w) {
        for (size_t i = 0; i < words.size(); ++i) {
          words[i] = w.GetWord(i);
        }
      });
  return common::mem_io::FormatMemWords(words, bit_width, is_hex);
}

// Execute unary math function (real -> real)
auto ExecuteMathUnary(std::string_view name, double arg) -> double {
  if (name == "$ln") {
    return std::log(arg);
  }
  if (name == "$log10") {
    return std::log10(arg);
  }
  if (name == "$exp") {
    return std::exp(arg);
  }
  if (name == "$sqrt") {
    return std::sqrt(arg);
  }
  if (name == "$floor") {
    return std::floor(arg);
  }
  if (name == "$ceil") {
    return std::ceil(arg);
  }
  if (name == "$sin") {
    return std::sin(arg);
  }
  if (name == "$cos") {
    return std::cos(arg);
  }
  if (name == "$tan") {
    return std::tan(arg);
  }
  if (name == "$asin") {
    return std::asin(arg);
  }
  if (name == "$acos") {
    return std::acos(arg);
  }
  if (name == "$atan") {
    return std::atan(arg);
  }
  if (name == "$sinh") {
    return std::sinh(arg);
  }
  if (name == "$cosh") {
    return std::cosh(arg);
  }
  if (name == "$tanh") {
    return std::tanh(arg);
  }
  if (name == "$asinh") {
    return std::asinh(arg);
  }
  if (name == "$acosh") {
    return std::acosh(arg);
  }
  if (name == "$atanh") {
    return std::atanh(arg);
  }
  throw common::InternalError(
      "interpreter", std::format("unknown unary math function: {}", name));
}

// Execute binary math function (real, real -> real)
auto ExecuteMathBinary(std::string_view name, double a, double b) -> double {
  if (name == "$pow") {
    return std::pow(a, b);
  }
  if (name == "$atan2") {
    return std::atan2(a, b);
  }
  if (name == "$hypot") {
    return std::hypot(a, b);
  }
  throw common::InternalError(
      "interpreter", std::format("unknown binary math function: {}", name));
}

// Extract int64 value from RuntimeValue, handling both narrow and wide sources.
// For wide sources, extracts the low 64 bits.
auto ExtractInt64FromSource(const RuntimeValue& src) -> int64_t {
  return src.IsWide() ? static_cast<int64_t>(src.AsWideBit().GetWord(0))
                      : src.AsNarrow().AsInt64();
}

// Convert integral RuntimeValue to string per LRM 6.16.
// Each 8 bits forms one character, MSB first, null bytes are skipped.
auto IntegralToString(const RuntimeValue& val) -> std::string {
  std::string result;
  size_t width = std::get<common::IntegralData>(val.type.data).bit_width;

  if (val.IsWide()) {
    const auto& wide = val.AsWideBit();
    // Extract bytes from MSB to LSB
    for (size_t i = width; i >= 8; i -= 8) {
      size_t byte_start = i - 8;
      // Extract 8-bit value by reading bits
      uint8_t ch = 0;
      for (size_t b = 0; b < 8; ++b) {
        ch |= static_cast<uint8_t>(wide.GetBit(byte_start + b) << b);
      }
      if (ch != 0) {
        result += static_cast<char>(ch);
      }
    }
  } else {
    uint64_t bits = val.AsNarrow().AsUInt64();
    for (size_t i = width; i >= 8; i -= 8) {
      auto ch = static_cast<uint8_t>((bits >> (i - 8)) & 0xFF);
      if (ch != 0) {
        result += static_cast<char>(ch);
      }
    }
  }
  return result;
}

// Extract format string info from a RuntimeValue (first arg of $display etc).
// The is_string_literal flag comes from the instruction's
// format_string_is_literal field, computed at MIR-to-LIR time.
auto ExtractFormatString(const RuntimeValue& val, bool is_string_literal)
    -> common::FormatStringInfo {
  common::FormatStringInfo info;
  if (val.IsString()) {
    info.text = val.AsString();
    info.is_string_literal = true;
  } else if (is_string_literal && val.IsTwoState()) {
    info.text = IntegralToString(val);
    info.is_string_literal = true;
  }
  info.has_format_specifiers =
      info.is_string_literal && info.text.find('%') != std::string::npos;
  return info;
}

// Get format specifier for a single value based on its type.
auto GetFormatSpecifier(const RuntimeValue& val, char default_format) -> char {
  if (val.IsString()) {
    return 's';
  }
  if (val.IsReal() || val.IsShortReal()) {
    return 'f';
  }
  return default_format;
}

// Build format string from a list of values using default format for integrals.
auto BuildFormatString(
    const std::vector<RuntimeValue>& values, char default_format)
    -> std::string {
  std::string fmt;
  for (const auto& val : values) {
    fmt += '%';
    fmt += GetFormatSpecifier(val, default_format);
  }
  return fmt;
}

// Unified format message function for display-like system tasks.
// Encapsulates the common pattern: extract format string, collect args, format.
// Returns the formatted message string; caller handles output and newlines.
//
// Handles three cases:
// 1. String literal with format specifiers: use as format string
// 2. String literal without format specifiers: prefix + formatted args
// 3. No string literal: format all args with default_format
auto FormatMessage(
    std::span<const RuntimeValue> arg_values, bool first_is_string_literal,
    char default_format, const TimeFormatContext* time_ctx) -> std::string {
  if (arg_values.empty()) {
    return "";
  }

  // Extract format info from first argument
  auto fmt_info = ExtractFormatString(arg_values[0], first_is_string_literal);

  // Collect format arguments (skip first if it was string literal)
  std::vector<RuntimeValue> format_args;
  size_t first_arg = fmt_info.is_string_literal ? 1 : 0;
  for (size_t i = first_arg; i < arg_values.size(); ++i) {
    format_args.push_back(arg_values[i]);
  }

  std::string result;

  if (fmt_info.has_format_specifiers) {
    // Case 1: String with format specifiers - use as format string
    result = FormatDisplay(fmt_info.text, format_args, time_ctx);
  } else if (fmt_info.is_string_literal) {
    // Case 2: String literal without format specifiers - prefix + formatted
    // args
    result = fmt_info.text;
    if (!format_args.empty()) {
      std::string format_str = BuildFormatString(format_args, default_format);
      result += FormatDisplay(format_str, format_args, time_ctx);
    }
  } else {
    // Case 3: No string literal - format all args
    std::string format_str = BuildFormatString(format_args, default_format);
    result = FormatDisplay(format_str, format_args, time_ctx);
  }

  return result;
}

// Handles $readmem* and $writemem* system tasks.
auto HandleMemIO(
    const lir::Instruction& instr, bool is_read, bool is_hex,
    const std::function<RuntimeValue(const lir::Operand&)>& get_operand_value,
    const std::function<RuntimeValue(const lir::Operand&)>& read_variable,
    const std::function<void(const lir::Operand&, const RuntimeValue&)>&
        store_variable) -> InstructionResult {
  std::string_view task_name = is_read ? "$readmem" : "$writemem";
  if (instr.operands.size() < 2 || instr.operands.size() > 4) {
    throw common::InternalError(
        "interpreter", std::format("{} expects 2-4 operands", task_name));
  }
  bool filename_is_string_literal = instr.format_string_is_literal;
  const auto filename_value = get_operand_value(instr.operands[0]);
  std::string filename;
  if (filename_value.IsString()) {
    filename = filename_value.AsString();
  } else if (filename_is_string_literal && filename_value.IsTwoState()) {
    filename = IntegralToString(filename_value);
  } else {
    throw common::InternalError(
        "interpreter", std::format("{} filename must be a string", task_name));
  }
  if (!instr.operands[1].IsVariable()) {
    throw common::InternalError(
        "interpreter", std::format("{} target must be a variable", task_name));
  }
  auto target_value = read_variable(instr.operands[1]);
  auto info = GetMemTargetInfo(target_value, target_value.type);

  std::optional<int64_t> start_addr;
  std::optional<int64_t> end_addr;
  if (instr.operands.size() >= 3) {
    auto start_val = get_operand_value(instr.operands[2]);
    if (!start_val.IsTwoState() || start_val.IsWide()) {
      throw common::InternalError(
          "interpreter",
          std::format("{} start address must be narrow integral", task_name));
    }
    start_addr = start_val.AsNarrow().AsInt64();
  }
  if (instr.operands.size() == 4) {
    auto end_val = get_operand_value(instr.operands[3]);
    if (!end_val.IsTwoState() || end_val.IsWide()) {
      throw common::InternalError(
          "interpreter",
          std::format("{} end address must be narrow integral", task_name));
    }
    end_addr = end_val.AsNarrow().AsInt64();
  }

  if (info.element_count == 0 || info.element_width == 0) {
    throw common::InternalError(
        "interpreter", std::format("{} target has zero size", task_name));
  }

  int64_t min_addr = info.lower_bound;
  int64_t max_addr =
      info.lower_bound + static_cast<int64_t>(info.element_count) - 1;
  int64_t current_addr = start_addr.value_or(min_addr);
  int64_t final_addr = end_addr.value_or(max_addr);

  if (current_addr < min_addr || current_addr > max_addr) {
    throw common::InternalError(
        "interpreter",
        std::format("{} start address out of bounds", task_name));
  }
  if (final_addr < min_addr || final_addr > max_addr) {
    throw common::InternalError(
        "interpreter", std::format("{} end address out of bounds", task_name));
  }

  auto path = common::mem_io::ResolveMemPath(filename);
  auto handle_read_mem = [&]() -> InstructionResult {
    std::ifstream in(path);
    if (!in) {
      throw common::InternalError(
          "interpreter",
          std::format("failed to open memory file: {}", path.string()));
    }

    std::string content(
        (std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());

    RuntimeValue packed_value = target_value;
    auto write_value = [&](int64_t addr, const RuntimeValue& value) {
      if (addr < min_addr || addr > max_addr) {
        throw common::InternalError(
            "interpreter", std::format("{} address out of bounds", task_name));
      }
      auto index = static_cast<size_t>(addr - min_addr);
      if (info.is_unpacked) {
        target_value.SetElement(index, value);
      } else {
        packed_value =
            StorePackedElement(packed_value, index, info.element_width, value);
      }
    };

    auto parse_result = common::mem_io::ParseMemFile(
        content, is_hex, min_addr, max_addr, current_addr, final_addr,
        task_name, [&](std::string_view token, int64_t addr) {
          auto value = ParseMemTokenToValue(token, info.element_width, is_hex);
          write_value(addr, value);
        });
    if (!parse_result.success) {
      throw std::runtime_error(parse_result.error);
    }

    if (info.is_unpacked) {
      store_variable(instr.operands[1], target_value);
    } else {
      store_variable(instr.operands[1], packed_value);
    }
    return InstructionResult::Continue();
  };

  auto handle_write_mem = [&]() -> InstructionResult {
    std::ofstream out(path);
    if (!out) {
      throw common::InternalError(
          "interpreter",
          std::format(
              "failed to open memory file for write: {}", path.string()));
    }

    if (start_addr.has_value()) {
      out << "@"
          << common::mem_io::FormatMemAddress(
                 static_cast<uint64_t>(*start_addr), is_hex)
          << "\n";
    }

    for (int64_t addr = current_addr; addr <= final_addr; ++addr) {
      auto index = static_cast<size_t>(addr - min_addr);
      RuntimeValue element;
      if (info.is_unpacked) {
        element = target_value.GetElement(index);
      } else {
        element = ExtractPackedElement(target_value, index, info.element_width);
      }
      out << FormatMemValue(element, info.element_width, is_hex) << "\n";
    }
    return InstructionResult::Continue();
  };

  return is_read ? handle_read_mem() : handle_write_mem();
}

}  // namespace

auto RunSystemCall(
    const lir::Instruction& instr, SimulationContext& simulation_context,
    ProcessFrame& frame, ProcessEffect& effect, TempTable& temp_table,
    const std::shared_ptr<InstanceContext>& instance_context)
    -> InstructionResult {
  auto& module_variable_table = simulation_context.variable_table;
  auto& process_variable_table = frame.variable_table;

  // Helper to resolve symbol through instance context port bindings.
  auto resolve_binding = [&instance_context](const auto* symbol)
      -> std::pair<common::SymbolRef, std::shared_ptr<InstanceContext>> {
    if (instance_context != nullptr) {
      return instance_context->ResolveBinding(symbol);
    }
    return {symbol, nullptr};
  };

  auto get_temp = [&](const lir::Operand& operand) -> RuntimeValue {
    assert(operand.IsTemp());
    return temp_table.Read(std::get<lir::TempRef>(operand.value));
  };

  auto read_variable = [&](const lir::Operand& operand) -> RuntimeValue {
    assert(operand.IsVariable());
    const auto* symbol = std::get<lir::SymbolRef>(operand.value);

    // Check function-local variables first (parameters and locals)
    if (!frame.call_stack.empty()) {
      auto& call_frame = frame.call_stack.back();
      auto it = call_frame.local_variables.find(symbol);
      if (it != call_frame.local_variables.end()) {
        return it->second;
      }
    }

    // Check process-local next
    if (process_variable_table.Exists(symbol)) {
      return process_variable_table.Read(symbol);
    }

    // Resolve through port bindings (output port → target signal/instance)
    auto [target_symbol, target_instance] = resolve_binding(symbol);

    // If bound, read from target instance's storage
    if (target_instance != nullptr) {
      return target_instance->Read(target_symbol);
    }

    // Otherwise, read from per-instance storage (local vars, input ports)
    if (instance_context != nullptr && instance_context->Exists(symbol)) {
      return instance_context->Read(symbol);
    }

    // Fallback to global table
    return module_variable_table.Read(symbol);
  };

  auto store_variable = [&](const lir::Operand& operand,
                            const RuntimeValue& value) {
    assert(operand.IsVariable());
    const auto* symbol = std::get<lir::SymbolRef>(operand.value);

    // Deep copy arrays for value semantics
    const RuntimeValue actual_value = value.DeepCopy();

    // Check function-local variables first (parameters and locals)
    if (!frame.call_stack.empty()) {
      auto& call_frame = frame.call_stack.back();
      auto it = call_frame.local_variables.find(symbol);
      if (it != call_frame.local_variables.end()) {
        it->second = actual_value;
        return;
      }
    }

    // Check process-local next
    if (process_variable_table.Exists(symbol)) {
      process_variable_table.Write(symbol, actual_value);
      return;
    }

    // Resolve through port bindings (output port → target signal/instance)
    auto [target_symbol, target_instance] = resolve_binding(symbol);

    // If bound (output port), write to target instance's storage
    if (target_instance != nullptr) {
      target_instance->Write(target_symbol, actual_value);
      effect.RecordVariableModification(target_symbol, target_instance);
      return;
    }

    // Otherwise, write to per-instance storage
    if (instance_context != nullptr) {
      instance_context->Write(symbol, actual_value);
      effect.RecordVariableModification(symbol, instance_context);
      return;
    }

    // Fallback to global table
    module_variable_table.Write(symbol, actual_value);
    effect.RecordVariableModification(symbol);
  };

  auto get_operand_value = [&](const lir::Operand& operand) -> RuntimeValue {
    if (operand.IsTemp()) {
      return get_temp(operand);
    }
    if (operand.IsVariable()) {
      return read_variable(operand);
    }
    if (operand.IsLiteral()) {
      const auto& literal = std::get<lir::LiteralRef>(operand.value);
      return RuntimeValue::FromLiteral(literal);
    }
    throw common::InternalError(
        "interpreter", "unexpected operand kind for system call");
  };

  // Simulation control tasks: $finish, $stop, $exit
  if (instr.system_call_name == "$finish" ||
      instr.system_call_name == "$stop" || instr.system_call_name == "$exit") {
    bool is_stop = (instr.system_call_name == "$stop");

    // Get diagnostic level: 0 = nothing, 1 = time, 2 = time + stats
    // $exit has no argument, treat as level 1
    // $finish and $stop default to 1 if no argument (handled in lowering)
    int level = 1;
    if (!instr.operands.empty()) {
      // Diagnostic level is never wide
      level = static_cast<int>(
          get_operand_value(instr.operands[0]).AsNarrow().AsUInt64());
    }

    // Print diagnostics based on level (VCS style)
    if (level >= 1) {
      simulation_context.display_output
          << instr.system_call_name << " called at time "
          << simulation_context.current_time << "\n";
    }

    return InstructionResult::Finish(is_stop);
  }

  // Severity tasks: $fatal, $error, $warning, $info
  // Source location is in instruction metadata.
  // Arguments: $fatal: [finish_num, format_args...], others:
  // [format_args...]
  if (instr.system_call_name == "$fatal" ||
      instr.system_call_name == "$error" ||
      instr.system_call_name == "$warning" ||
      instr.system_call_name == "$info") {
    // Determine severity string
    std::string severity;
    if (instr.system_call_name == "$fatal") {
      severity = "FATAL";
    } else if (instr.system_call_name == "$error") {
      severity = "ERROR";
    } else if (instr.system_call_name == "$warning") {
      severity = "WARNING";
    } else {
      severity = "INFO";
    }

    // Parse arguments based on task type
    size_t arg_idx = 0;
    int finish_num = 1;  // Default for $fatal

    if (instr.system_call_name == "$fatal" && !instr.operands.empty()) {
      finish_num = static_cast<int>(
          get_operand_value(instr.operands[0]).AsNarrow().AsUInt64());
      arg_idx = 1;
    }

    // Get file and line from instruction metadata
    std::string file_str = instr.source_file.value_or("");
    int line_num = static_cast<int>(instr.source_line.value_or(0));

    // Get hierarchical scope name
    std::string scope = instance_context->instance_path;

    // Build message from remaining arguments (if any)
    std::string message;
    bool has_format = instr.format_operand.has_value();
    if (has_format || arg_idx < instr.operands.size()) {
      // Collect message arguments (prepend format_operand if present)
      std::vector<RuntimeValue> msg_args;
      if (instr.format_operand) {
        msg_args.push_back(get_operand_value(*instr.format_operand));
      }
      for (size_t i = arg_idx; i < instr.operands.size(); ++i) {
        msg_args.push_back(get_operand_value(instr.operands[i]));
      }

      TimeFormatContext time_ctx{
          .time_format = simulation_context.time_format,
          .module_unit_power = simulation_context.timescale
                                   ? simulation_context.timescale->unit_power
                                   : common::TimeScale::kDefaultUnitPower,
          .global_precision_power = simulation_context.global_precision_power};

      message = FormatMessage(
          msg_args, instr.format_string_is_literal, 'd', &time_ctx);
    }

    // Only print if finish_num >= 1 for $fatal (or always for others)
    bool should_print = instr.system_call_name != "$fatal" || finish_num >= 1;

    if (should_print) {
      simulation_context.display_output << severity << ": " << file_str << ":"
                                        << line_num << ": " << scope << " @ "
                                        << simulation_context.current_time;
      if (!message.empty()) {
        simulation_context.display_output << ": " << message;
      }
      simulation_context.display_output << "\n";
    }

    // Only $fatal terminates
    if (instr.system_call_name == "$fatal") {
      return InstructionResult::Finish(true);  // is_stop = true for error exit
    }
    return InstructionResult::Continue();
  }

  if (instr.system_call_name == "$readmemh" ||
      instr.system_call_name == "$readmemb") {
    bool is_hex = instr.system_call_name == "$readmemh";
    return HandleMemIO(
        instr, true, is_hex, get_operand_value, read_variable, store_variable);
  }

  if (instr.system_call_name == "$writememh" ||
      instr.system_call_name == "$writememb") {
    bool is_hex = instr.system_call_name == "$writememh";
    return HandleMemIO(
        instr, false, is_hex, get_operand_value, read_variable, store_variable);
  }

  // Handle all display/write variants
  if (instr.system_call_name == "$display" ||
      instr.system_call_name == "$displayb" ||
      instr.system_call_name == "$displayo" ||
      instr.system_call_name == "$displayh" ||
      instr.system_call_name == "$write" ||
      instr.system_call_name == "$writeb" ||
      instr.system_call_name == "$writeo" ||
      instr.system_call_name == "$writeh") {
    auto props = GetDisplayVariantProps(instr.system_call_name);

    // Collect argument values (prepend format_operand if present)
    std::vector<RuntimeValue> arg_values;
    if (instr.format_operand) {
      arg_values.push_back(get_operand_value(*instr.format_operand));
    }
    for (const auto& operand : instr.operands) {
      arg_values.push_back(get_operand_value(operand));
    }

    // Create time format context for %t specifier
    TimeFormatContext time_ctx{
        .time_format = simulation_context.time_format,
        .module_unit_power = simulation_context.timescale
                                 ? simulation_context.timescale->unit_power
                                 : common::TimeScale::kDefaultUnitPower,
        .global_precision_power = simulation_context.global_precision_power};

    // Format and output message
    std::string message = FormatMessage(
        arg_values, instr.format_string_is_literal, props.default_format,
        &time_ctx);
    simulation_context.display_output << message;

    if (props.append_newline) {
      simulation_context.display_output << "\n";
    }
    return InstructionResult::Continue();
  }

  // Handle strobe variants - same as display but queued to Postponed region
  if (instr.system_call_name == "$strobe" ||
      instr.system_call_name == "$strobeb" ||
      instr.system_call_name == "$strobeo" ||
      instr.system_call_name == "$strobeh") {
    auto props = GetDisplayVariantProps(instr.system_call_name);
    bool first_is_string_literal = instr.format_string_is_literal;

    // Collect argument values now (prepend format_operand if present)
    std::vector<RuntimeValue> arg_values;
    if (instr.format_operand) {
      arg_values.push_back(get_operand_value(*instr.format_operand));
    }
    for (const auto& operand : instr.operands) {
      arg_values.push_back(get_operand_value(operand));
    }

    // Create time format context
    TimeFormatContext time_ctx{
        .time_format = simulation_context.time_format,
        .module_unit_power = simulation_context.timescale
                                 ? simulation_context.timescale->unit_power
                                 : common::TimeScale::kDefaultUnitPower,
        .global_precision_power = simulation_context.global_precision_power};

    // Queue the output for Postponed region
    effect.RecordPostponedAction(
        PostponedAction{
            .action = [&simulation_context, props,
                       arg_values = std::move(arg_values), time_ctx,
                       first_is_string_literal]() {
              std::string message = FormatMessage(
                  arg_values, first_is_string_literal, props.default_format,
                  &time_ctx);
              simulation_context.display_output << message;
              if (props.append_newline) {
                simulation_context.display_output << "\n";
              }
            }});
    return InstructionResult::Continue();
  }

  // Handle monitor variants - register for value change tracking
  if (instr.system_call_name == "$monitor" ||
      instr.system_call_name == "$monitorb" ||
      instr.system_call_name == "$monitoro" ||
      instr.system_call_name == "$monitorh") {
    auto props = GetDisplayVariantProps(instr.system_call_name);

    // Check if format_operand is a string literal
    bool first_is_string_literal = instr.format_string_is_literal;

    // Collect argument values (prepend format_operand if present)
    std::vector<RuntimeValue> arg_values;
    if (instr.format_operand) {
      arg_values.push_back(get_operand_value(*instr.format_operand));
    }
    for (const auto& operand : instr.operands) {
      arg_values.push_back(get_operand_value(operand));
    }

    // Extract format string info from first argument
    common::FormatStringInfo fmt_info;
    if (!arg_values.empty()) {
      fmt_info = ExtractFormatString(arg_values[0], first_is_string_literal);
    }

    // Collect format arguments (skip first if it was a format
    // string/prefix)
    std::vector<RuntimeValue> format_args;
    size_t first_arg_idx = fmt_info.is_string_literal ? 1 : 0;
    for (size_t i = first_arg_idx; i < arg_values.size(); ++i) {
      format_args.push_back(arg_values[i]);
    }

    // Build format string: prefix (if any) + format specifiers
    std::string format_string;
    if (fmt_info.has_format_specifiers) {
      format_string = fmt_info.text;
    } else {
      if (fmt_info.is_string_literal) {
        format_string = fmt_info.text;  // Use as prefix
      }
      format_string += BuildFormatString(format_args, props.default_format);
    }

    // Get time format context
    auto module_unit_power = simulation_context.timescale
                                 ? simulation_context.timescale->unit_power
                                 : common::TimeScale::kDefaultUnitPower;
    TimeFormatContext time_ctx{
        .time_format = simulation_context.time_format,
        .module_unit_power = module_unit_power,
        .global_precision_power = simulation_context.global_precision_power};

    // Print immediately on first call (IEEE 1800 §21.2.3)
    simulation_context.display_output
        << FormatDisplay(format_string, format_args, &time_ctx);
    if (props.append_newline) {
      simulation_context.display_output << "\n";
    }

    // Set up monitor state as a closure with captured prev values.
    // This matches codegen's mutable lambda capture semantics.
    CallFrame closure;
    for (size_t i = 0; i < format_args.size(); ++i) {
      std::string capture_name = "__capture_prev_" + std::to_string(i);
      closure.captures[capture_name] = std::move(format_args[i]);
    }

    simulation_context.active_monitor = MonitorState{
        .enabled = true,
        .instance = instance_context,
        .check_process_name = instr.monitor_check_function_name,
        .closure = std::move(closure)};

    return InstructionResult::Continue();
  }

  // Handle $monitoron - enable monitoring
  if (instr.system_call_name == "$monitoron") {
    if (simulation_context.active_monitor) {
      simulation_context.active_monitor->enabled = true;
    }
    return InstructionResult::Continue();
  }

  // Handle $monitoroff - disable monitoring
  if (instr.system_call_name == "$monitoroff") {
    if (simulation_context.active_monitor) {
      simulation_context.active_monitor->enabled = false;
    }
    return InstructionResult::Continue();
  }

  // Simulation time functions: $time, $stime, $realtime
  // Scale raw simulation time to module's timeunit per LRM
  auto scale_time = [&simulation_context]() -> uint64_t {
    uint64_t raw_time = simulation_context.current_time;
    if (!simulation_context.timescale) {
      return raw_time;
    }
    uint64_t divisor = simulation_context.timescale->TimeDivisor(
        simulation_context.global_precision_power);
    return raw_time / divisor;
  };

  if (instr.system_call_name == "$time") {
    assert(instr.result.has_value());
    // $time returns 64-bit unsigned time in module's timeunit
    auto result = RuntimeValue::IntegralUnsigned(scale_time(), 64);
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  if (instr.system_call_name == "$stime") {
    assert(instr.result.has_value());
    // $stime returns low 32 bits of scaled time as unsigned
    auto result = RuntimeValue::IntegralUnsigned(scale_time() & 0xFFFFFFFF, 32);
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  if (instr.system_call_name == "$realtime") {
    assert(instr.result.has_value());
    // $realtime returns scaled time as real (double)
    // For accurate fractional time, divide raw time as double
    auto scaled_time = static_cast<double>(simulation_context.current_time);
    if (simulation_context.timescale) {
      auto divisor =
          static_cast<double>(simulation_context.timescale->TimeDivisor(
              simulation_context.global_precision_power));
      scaled_time /= divisor;
    }
    auto result = RuntimeValue::Real(scaled_time);
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  if (instr.system_call_name == "$timeformat") {
    // $timeformat(units, precision, suffix, min_width)
    // All arguments are optional; use defaults if not provided
    auto& tf = simulation_context.time_format;

    if (!instr.operands.empty()) {
      tf.units = static_cast<int8_t>(
          get_operand_value(instr.operands[0]).AsNarrow().AsInt64());
    }
    if (instr.operands.size() >= 2) {
      tf.precision = static_cast<int>(
          get_operand_value(instr.operands[1]).AsNarrow().AsInt64());
    }
    if (instr.operands.size() >= 3) {
      tf.suffix = get_operand_value(instr.operands[2]).AsString();
    }
    if (instr.operands.size() >= 4) {
      tf.min_width = static_cast<int>(
          get_operand_value(instr.operands[3]).AsNarrow().AsInt64());
    }

    return InstructionResult::Continue();
  }

  if (instr.system_call_name == "$timeunit") {
    assert(instr.result.has_value());
    // $timeunit returns module's time unit as power of 10
    int8_t unit = simulation_context.timescale
                      ? simulation_context.timescale->unit_power
                      : common::TimeScale::kDefaultUnitPower;
    auto result = RuntimeValue::IntegralSigned(unit, 32);
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  if (instr.system_call_name == "$timeunit_root") {
    assert(instr.result.has_value());
    // $timeunit($root) returns global simulation precision
    int8_t unit = simulation_context.global_precision_power;
    auto result = RuntimeValue::IntegralSigned(unit, 32);
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  if (instr.system_call_name == "$timeprecision") {
    assert(instr.result.has_value());
    // $timeprecision returns module's time precision as power of 10
    int8_t precision = simulation_context.timescale
                           ? simulation_context.timescale->precision_power
                           : common::TimeScale::kDefaultPrecisionPower;
    auto result = RuntimeValue::IntegralSigned(precision, 32);
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  if (instr.system_call_name == "$timeprecision_root") {
    assert(instr.result.has_value());
    // $timeprecision($root) returns global simulation precision
    int8_t precision = simulation_context.global_precision_power;
    auto result = RuntimeValue::IntegralSigned(precision, 32);
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  if (instr.system_call_name == "$printtimescale") {
    // $printtimescale() prints current module's timescale
    auto ts =
        simulation_context.timescale.value_or(common::TimeScale::Default());
    simulation_context.display_output
        << "Time scale of (" << simulation_context.module_name << ") is "
        << ts.ToString() << "\n";
    return InstructionResult::Continue();
  }

  if (instr.system_call_name == "$printtimescale_root") {
    // $printtimescale($root) prints global precision (unit = precision)
    auto gp = simulation_context.global_precision_power;
    auto unit_str = sdk::PowerToString(gp);
    simulation_context.display_output << "Time scale of ($root) is " << unit_str
                                      << " / " << unit_str << "\n";
    return InstructionResult::Continue();
  }

  // $signed, $unsigned: reinterpret signedness, preserving bit pattern
  if (instr.system_call_name == "$signed") {
    assert(instr.result.has_value());
    assert(instr.result_type.has_value());
    assert(instr.operands.size() == 1);
    const auto& src = get_operand_value(instr.operands[0]);
    // $signed: reinterpret bits as signed, preserving bit width
    auto target_data = std::get<common::IntegralData>(instr.result_type->data);
    auto result = RuntimeValue::IntegralSigned(
        ExtractInt64FromSource(src), target_data.bit_width);
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  if (instr.system_call_name == "$unsigned") {
    assert(instr.result.has_value());
    assert(instr.result_type.has_value());
    assert(instr.operands.size() == 1);
    const auto& src = get_operand_value(instr.operands[0]);
    // $unsigned: reinterpret bits as unsigned, preserving bit width
    auto target_data = std::get<common::IntegralData>(instr.result_type->data);
    auto result = RuntimeValue::IntegralUnsigned(
        static_cast<uint64_t>(ExtractInt64FromSource(src)),
        target_data.bit_width);
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  // $itor: convert integer to real
  if (instr.system_call_name == "$itor") {
    assert(instr.result.has_value());
    assert(!instr.operands.empty());
    const auto& src = get_operand_value(instr.operands[0]);
    auto src_data = std::get<common::IntegralData>(src.type.data);
    double real_value = src_data.is_signed
                            ? static_cast<double>(ExtractInt64FromSource(src))
                            : static_cast<double>(static_cast<uint64_t>(
                                  ExtractInt64FromSource(src)));
    temp_table.Write(instr.result.value(), RuntimeValue::Real(real_value));
    return InstructionResult::Continue();
  }

  // $rtoi: convert real to integer by truncation toward zero
  if (instr.system_call_name == "$rtoi") {
    assert(instr.result.has_value());
    assert(instr.result_type.has_value());
    assert(!instr.operands.empty());
    const auto& src = get_operand_value(instr.operands[0]);
    auto target_data = std::get<common::IntegralData>(instr.result_type->data);
    auto raw_value = static_cast<int64_t>(src.AsDouble());
    auto result =
        target_data.is_signed
            ? RuntimeValue::IntegralSigned(raw_value, target_data.bit_width)
            : RuntimeValue::IntegralUnsigned(
                  static_cast<uint64_t>(raw_value), target_data.bit_width);
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  // $realtobits: real → 64-bit IEEE 754 representation
  if (instr.system_call_name == "$realtobits") {
    assert(instr.result.has_value());
    assert(!instr.operands.empty());
    const auto& src = get_operand_value(instr.operands[0]);
    auto bits = std::bit_cast<uint64_t>(src.AsDouble());
    temp_table.Write(
        instr.result.value(), RuntimeValue::IntegralUnsigned(bits, 64));
    return InstructionResult::Continue();
  }

  // $bitstoreal: 64-bit vector → real (IEEE 754)
  if (instr.system_call_name == "$bitstoreal") {
    assert(instr.result.has_value());
    assert(!instr.operands.empty());
    const auto& src = get_operand_value(instr.operands[0]);
    auto real_value = std::bit_cast<double>(src.AsNarrow().AsUInt64());
    temp_table.Write(instr.result.value(), RuntimeValue::Real(real_value));
    return InstructionResult::Continue();
  }

  // $shortrealtobits: shortreal → 32-bit IEEE 754 representation
  if (instr.system_call_name == "$shortrealtobits") {
    assert(instr.result.has_value());
    assert(!instr.operands.empty());
    const auto& src = get_operand_value(instr.operands[0]);
    auto bits = std::bit_cast<uint32_t>(src.AsFloat());
    temp_table.Write(
        instr.result.value(), RuntimeValue::IntegralUnsigned(bits, 32));
    return InstructionResult::Continue();
  }

  // $bitstoshortreal: 32-bit vector → shortreal (IEEE 754)
  if (instr.system_call_name == "$bitstoshortreal") {
    assert(instr.result.has_value());
    assert(!instr.operands.empty());
    const auto& src = get_operand_value(instr.operands[0]);
    auto bits = static_cast<uint32_t>(src.AsNarrow().AsUInt64());
    auto shortreal_value = std::bit_cast<float>(bits);
    temp_table.Write(
        instr.result.value(), RuntimeValue::ShortReal(shortreal_value));
    return InstructionResult::Continue();
  }

  // $clog2: ceiling of log base 2 (arg treated as unsigned, 0 → 0)
  if (instr.system_call_name == "$clog2") {
    assert(instr.result.has_value());
    assert(!instr.operands.empty());
    const auto& src = get_operand_value(instr.operands[0]);
    uint64_t n = src.AsNarrow().AsUInt64();
    int result = (n == 0) ? 0 : std::bit_width(n - 1);
    temp_table.Write(
        instr.result.value(), RuntimeValue::IntegralSigned(result, 32));
    return InstructionResult::Continue();
  }

  // Math functions: use category-based dispatch from registry
  const auto* func_info = common::FindSystemFunction(instr.system_call_name);
  if (func_info != nullptr) {
    using Category = common::SystemFunctionCategory;
    if (func_info->category == Category::kMathUnary) {
      assert(instr.result.has_value());
      assert(instr.operands.size() == 1);
      double arg = get_operand_value(instr.operands[0]).AsDouble();
      double result = ExecuteMathUnary(instr.system_call_name, arg);
      temp_table.Write(instr.result.value(), RuntimeValue::Real(result));
      return InstructionResult::Continue();
    }
    if (func_info->category == Category::kMathBinary) {
      assert(instr.result.has_value());
      assert(instr.operands.size() == 2);
      double a = get_operand_value(instr.operands[0]).AsDouble();
      double b = get_operand_value(instr.operands[1]).AsDouble();
      double result = ExecuteMathBinary(instr.system_call_name, a, b);
      temp_table.Write(instr.result.value(), RuntimeValue::Real(result));
      return InstructionResult::Continue();
    }
    if (func_info->category == Category::kPlusargs) {
      sdk::PlusargsQuery query(simulation_context.plusargs);

      // Helper to get string from operand (handles hex-encoded literals)
      auto get_string_operand = [&](const RuntimeValue& val) -> std::string {
        if (val.IsString()) {
          return val.AsString();
        }
        // String literals are hex-encoded as integrals
        return IntegralToString(val);
      };

      if (instr.system_call_name == "$test$plusargs") {
        assert(instr.result.has_value());
        assert(instr.operands.size() == 1);
        const auto& query_val = get_operand_value(instr.operands[0]);
        std::string query_str = get_string_operand(query_val);
        int32_t result = query.TestPlusargs(query_str);
        temp_table.Write(
            instr.result.value(), RuntimeValue::IntegralSigned(result, 32));
        return InstructionResult::Continue();
      }

      if (instr.system_call_name == "$value$plusargs") {
        assert(instr.result.has_value());
        assert(instr.operands.size() == 1);
        assert(instr.output_targets.size() == 1);

        const auto& format_val = get_operand_value(instr.operands[0]);
        std::string format = get_string_operand(format_val);

        // Determine format specifier to select appropriate query
        auto pos = format.find('%');
        char spec = (pos != std::string::npos && pos + 1 < format.size())
                        ? format[pos + 1]
                        : '\0';
        // Handle %0d -> %d
        if (spec == '0' && pos + 2 < format.size()) {
          spec = format[pos + 2];
        }

        int32_t matched = 0;
        if (spec == 'd' || spec == 'D') {
          auto result = query.ValuePlusargsInt(format);
          if (result.matched) {
            matched = 1;
            auto target_operand =
                lir::Operand::Variable(instr.output_targets[0]);
            store_variable(
                target_operand, RuntimeValue::IntegralSigned(result.value, 32));
          }
        } else if (spec == 's' || spec == 'S') {
          auto result = query.ValuePlusargsString(format);
          if (result.matched) {
            matched = 1;
            auto target_operand =
                lir::Operand::Variable(instr.output_targets[0]);
            store_variable(target_operand, RuntimeValue::String(result.value));
          }
        }

        temp_table.Write(
            instr.result.value(), RuntimeValue::IntegralSigned(matched, 32));
        return InstructionResult::Continue();
      }
    }
  }

  // Supported system calls are validated in AST→MIR
  assert(false && "unsupported system call should be rejected in AST→MIR");

  assert(false && "unsupported system call");
  return InstructionResult::Continue();
}

}  // namespace lyra::interpreter
