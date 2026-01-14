#pragma once

#include <string>
#include <string_view>

#include "lyra/common/constant.hpp"
#include "lyra/common/format_string.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::compiler::codegen {

// Properties for display/write variants ($display, $write, etc.)
struct DisplayVariantProps {
  char default_format;  // 'd', 'b', 'o', or 'h' (for std::format)
  bool use_println;     // true for $display*, false for $write*
};

// Get properties for a display/write variant
inline auto GetDisplayVariantProps(std::string_view name)
    -> DisplayVariantProps {
  // $write and $fwrite (no newline)
  if (name == "$write" || name == "$fwrite") {
    return {.default_format = 'd', .use_println = false};
  }
  if (name == "$writeb" || name == "$fwriteb") {
    return {.default_format = 'b', .use_println = false};
  }
  if (name == "$writeo" || name == "$fwriteo") {
    return {.default_format = 'o', .use_println = false};
  }
  if (name == "$writeh" || name == "$fwriteh") {
    return {.default_format = 'x', .use_println = false};
  }
  // String formatting ($sformatf, $sformat, $swrite*) - no newline
  if (name == "$sformatf" || name == "$sformat" || name == "$swrite") {
    return {.default_format = 'd', .use_println = false};
  }
  if (name == "$swriteb") {
    return {.default_format = 'b', .use_println = false};
  }
  if (name == "$swriteo") {
    return {.default_format = 'o', .use_println = false};
  }
  if (name == "$swriteh") {
    return {.default_format = 'x', .use_println = false};
  }
  // $display and $fdisplay (with newline)
  if (name == "$displayb" || name == "$fdisplayb") {
    return {.default_format = 'b', .use_println = true};
  }
  if (name == "$displayo" || name == "$fdisplayo") {
    return {.default_format = 'o', .use_println = true};
  }
  if (name == "$displayh" || name == "$fdisplayh") {
    return {.default_format = 'x', .use_println = true};
  }
  // $strobe and $fstrobe (with newline)
  if (name == "$strobeb" || name == "$fstrobeb") {
    return {.default_format = 'b', .use_println = true};
  }
  if (name == "$strobeo" || name == "$fstrobeo") {
    return {.default_format = 'o', .use_println = true};
  }
  if (name == "$strobeh" || name == "$fstrobeh") {
    return {.default_format = 'x', .use_println = true};
  }
  // $monitor and $fmonitor (with newline)
  if (name == "$monitorb" || name == "$fmonitorb") {
    return {.default_format = 'b', .use_println = true};
  }
  if (name == "$monitoro" || name == "$fmonitoro") {
    return {.default_format = 'o', .use_println = true};
  }
  if (name == "$monitorh" || name == "$fmonitorh") {
    return {.default_format = 'x', .use_println = true};
  }
  // $display, $fdisplay, $strobe, $fstrobe, $monitor, $fmonitor (default)
  return {.default_format = 'd', .use_println = true};
}

// Convert an integral constant to string (for bit-packed format strings).
// Each 8 bits forms one character, MSB first, null bytes are skipped.
inline auto IntegralConstantToString(const common::Constant& c) -> std::string {
  std::string result;
  if (c.type.kind != common::Type::Kind::kIntegral) {
    return result;
  }
  auto data = std::get<common::IntegralData>(c.type.data);
  size_t width = data.bit_width;

  if (c.value.IsInt64()) {
    auto bits = static_cast<uint64_t>(c.value.AsInt64());
    for (size_t i = width; i >= 8; i -= 8) {
      auto ch = static_cast<uint8_t>((bits >> (i - 8)) & 0xFF);
      if (ch != 0) {
        result += static_cast<char>(ch);
      }
    }
  } else if (c.value.IsWideBit()) {
    const auto& wide = c.value.AsWideBit();
    for (size_t i = width; i >= 8; i -= 8) {
      size_t byte_start = i - 8;
      uint8_t ch = 0;
      for (size_t b = 0; b < 8; ++b) {
        ch |= static_cast<uint8_t>(wide.GetBit(byte_start + b) << b);
      }
      if (ch != 0) {
        result += static_cast<char>(ch);
      }
    }
  }
  return result;
}

// Extract format string info from a MIR expression.
// Handles string literals and bit-packed strings (via is_string_literal flag).
inline auto ExtractFormatString(const mir::Expression& expr)
    -> common::FormatStringInfo {
  common::FormatStringInfo info;

  if (expr.kind != mir::Expression::Kind::kConstant) {
    return info;
  }
  const auto& lit = mir::As<mir::ConstantExpression>(expr);
  if (lit.constant.type.kind == common::Type::Kind::kString) {
    info.text = lit.constant.value.AsString();
    info.is_string_literal = true;
  } else if (lit.constant.is_string_literal) {
    // Bit-packed string: slang typed it as integral but it came from a string
    // literal. Convert back to string for format detection.
    info.text = IntegralConstantToString(lit.constant);
    info.is_string_literal = true;
  }
  info.has_format_specifiers =
      info.is_string_literal && info.text.find('%') != std::string::npos;
  return info;
}

}  // namespace lyra::compiler::codegen
