#pragma once

#include <string>
#include <string_view>

#include "lyra/common/format_string.hpp"
#include "lyra/common/literal.hpp"
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
  if (name == "$write") {
    return {.default_format = 'd', .use_println = false};
  }
  if (name == "$writeb") {
    return {.default_format = 'b', .use_println = false};
  }
  if (name == "$writeo") {
    return {.default_format = 'o', .use_println = false};
  }
  if (name == "$writeh") {
    return {.default_format = 'x', .use_println = false};
  }
  if (name == "$displayb") {
    return {.default_format = 'b', .use_println = true};
  }
  if (name == "$displayo") {
    return {.default_format = 'o', .use_println = true};
  }
  if (name == "$displayh") {
    return {.default_format = 'x', .use_println = true};
  }
  if (name == "$strobeb") {
    return {.default_format = 'b', .use_println = true};
  }
  if (name == "$strobeo") {
    return {.default_format = 'o', .use_println = true};
  }
  if (name == "$strobeh") {
    return {.default_format = 'x', .use_println = true};
  }
  if (name == "$monitorb") {
    return {.default_format = 'b', .use_println = true};
  }
  if (name == "$monitoro") {
    return {.default_format = 'o', .use_println = true};
  }
  if (name == "$monitorh") {
    return {.default_format = 'x', .use_println = true};
  }
  // $display, $strobe, $monitor (default)
  return {.default_format = 'd', .use_println = true};
}

// Convert an integral literal to string (for bit-packed format strings).
// Each 8 bits forms one character, MSB first, null bytes are skipped.
inline auto IntegralLiteralToString(const common::Literal& lit) -> std::string {
  std::string result;
  if (lit.type.kind != common::Type::Kind::kIntegral) {
    return result;
  }
  auto data = std::get<common::IntegralData>(lit.type.data);
  size_t width = data.bit_width;

  if (lit.value.IsInt64()) {
    auto bits = static_cast<uint64_t>(lit.value.AsInt64());
    for (size_t i = width; i >= 8; i -= 8) {
      auto ch = static_cast<uint8_t>((bits >> (i - 8)) & 0xFF);
      if (ch != 0) {
        result += static_cast<char>(ch);
      }
    }
  } else if (lit.value.IsWideBit()) {
    const auto& wide = lit.value.AsWideBit();
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

  if (expr.kind != mir::Expression::Kind::kLiteral) {
    return info;
  }
  const auto& lit = mir::As<mir::LiteralExpression>(expr);
  if (lit.literal.type.kind == common::Type::Kind::kString) {
    info.text = lit.literal.value.AsString();
    info.is_string_literal = true;
  } else if (lit.literal.is_string_literal) {
    // Bit-packed string: slang typed it as integral but it came from a string
    // literal. Convert back to string for format detection.
    info.text = IntegralLiteralToString(lit.literal);
    info.is_string_literal = true;
  }
  info.has_format_specifiers =
      info.is_string_literal && info.text.find('%') != std::string::npos;
  return info;
}

}  // namespace lyra::compiler::codegen
