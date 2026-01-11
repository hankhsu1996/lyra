#include "lyra/interpreter/system_call/format.hpp"

#include <cctype>
#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include <fmt/core.h>
#include <fmt/format.h>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/type.hpp"

namespace lyra::interpreter {

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

auto FormatDisplay(
    const std::string& fmt_str, const std::vector<RuntimeValue>& args,
    const TimeFormatContext* time_ctx) -> std::string {
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

auto GetFormatSpecifier(const RuntimeValue& val, char default_format) -> char {
  if (val.IsString()) {
    return 's';
  }
  if (val.IsReal() || val.IsShortReal()) {
    return 'f';
  }
  return default_format;
}

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

auto ExtractInt64FromSource(const RuntimeValue& src) -> int64_t {
  return src.IsWide() ? static_cast<int64_t>(src.AsWideBit().GetWord(0))
                      : src.AsNarrow().AsInt64();
}

}  // namespace lyra::interpreter
