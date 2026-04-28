#include "lyra/support/format.hpp"

#include <cctype>
#include <cstddef>
#include <cstdint>
#include <format>
#include <limits>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"

namespace lyra::support {

namespace {

auto SpecCharToKind(char c) -> std::optional<FormatDirectiveKind> {
  switch (c) {
    case 'd':
    case 'D':
      return FormatDirectiveKind::kDecimal;
    case 'h':
    case 'H':
    case 'x':
    case 'X':
      return FormatDirectiveKind::kHex;
    case 'b':
    case 'B':
      return FormatDirectiveKind::kBinary;
    case 'o':
    case 'O':
      return FormatDirectiveKind::kOctal;
    case 's':
    case 'S':
      return FormatDirectiveKind::kString;
    case 'f':
    case 'F':
    case 'e':
    case 'E':
    case 'g':
    case 'G':
      return FormatDirectiveKind::kReal;
    case 't':
    case 'T':
      return FormatDirectiveKind::kTime;
    case 'c':
    case 'C':
      return FormatDirectiveKind::kChar;
    case 'm':
    case 'M':
      return FormatDirectiveKind::kModulePath;
    default:
      return std::nullopt;
  }
}

// Parses a digit run as a non-negative int32. Returns std::nullopt on overflow.
auto ParseInt32(std::string_view text, std::size_t& pos)
    -> std::optional<std::int32_t> {
  std::int64_t value = 0;
  bool any_digits = false;
  while (pos < text.size() &&
         std::isdigit(static_cast<unsigned char>(text[pos])) != 0) {
    value = value * 10 + (text[pos] - '0');
    if (value > std::numeric_limits<std::int32_t>::max()) {
      return std::nullopt;
    }
    ++pos;
    any_digits = true;
  }
  if (!any_digits) return -1;  // sentinel: no digits parsed
  return static_cast<std::int32_t>(value);
}

}  // namespace

auto FormatDirectiveNeedsValue(FormatDirectiveKind kind) -> bool {
  switch (kind) {
    case FormatDirectiveKind::kLiteral:
    case FormatDirectiveKind::kModulePath:
      return false;
    case FormatDirectiveKind::kDecimal:
    case FormatDirectiveKind::kHex:
    case FormatDirectiveKind::kBinary:
    case FormatDirectiveKind::kOctal:
    case FormatDirectiveKind::kString:
    case FormatDirectiveKind::kReal:
    case FormatDirectiveKind::kTime:
    case FormatDirectiveKind::kChar:
      return true;
  }
  return true;
}

auto ParseLiteralFormatString(
    std::string_view text, diag::SourceSpan format_span)
    -> diag::Result<std::vector<ParsedFormatDirective>> {
  std::vector<ParsedFormatDirective> out;
  std::string literal;
  std::size_t literal_start_offset = 0;
  std::size_t pos = 0;

  auto flush_literal = [&] {
    if (!literal.empty()) {
      out.push_back(
          ParsedFormatDirective{
              .kind = FormatDirectiveKind::kLiteral,
              .literal = std::move(literal),
              .modifiers = {},
              .source_offset = literal_start_offset});
      literal.clear();
    }
  };

  while (pos < text.size()) {
    const char c = text[pos];

    if (c == '%') {
      // %% -> literal %
      if (pos + 1 < text.size() && text[pos + 1] == '%') {
        if (literal.empty()) literal_start_offset = pos;
        literal.push_back('%');
        pos += 2;
        continue;
      }
      flush_literal();
      const std::size_t directive_start = pos;
      ++pos;  // consume '%'

      FormatDirectiveModifiers mods;

      if (pos < text.size() && text[pos] == '-') {
        mods.left_align = true;
        ++pos;
      }
      if (pos < text.size() && text[pos] == '0' && pos + 1 < text.size() &&
          std::isdigit(static_cast<unsigned char>(text[pos + 1])) != 0) {
        mods.zero_pad = true;
        ++pos;
      } else if (
          pos < text.size() && text[pos] == '0' &&
          (pos + 1 >= text.size() ||
           std::isdigit(static_cast<unsigned char>(text[pos + 1])) == 0)) {
        // %0X (no following digit run) -- LRM-style "minimal width" marker.
        mods.zero_pad = true;
        ++pos;
        mods.width = 0;
        // Fall through to spec char read below; do not parse a width run.
        if (pos >= text.size()) {
          return diag::Error(
              format_span, diag::DiagCode::kFormatStringMissingSpecifier,
              "format directive '%' is missing its specifier character");
        }
      }

      if (mods.width != 0) {
        auto width_or = ParseInt32(text, pos);
        if (!width_or.has_value()) {
          return diag::Error(
              format_span, diag::DiagCode::kFormatStringWidthOverflow,
              "format directive width does not fit in int32");
        }
        if (*width_or != -1) {
          mods.width = *width_or;
        }
      }

      if (pos < text.size() && text[pos] == '.') {
        ++pos;
        auto prec_or = ParseInt32(text, pos);
        if (!prec_or.has_value()) {
          return diag::Error(
              format_span, diag::DiagCode::kFormatStringWidthOverflow,
              "format directive precision does not fit in int32");
        }
        if (*prec_or == -1) {
          return diag::Error(
              format_span, diag::DiagCode::kFormatStringMissingSpecifier,
              "format directive '.' is missing precision digits");
        }
        mods.precision = *prec_or;
      }

      if (pos >= text.size()) {
        return diag::Error(
            format_span, diag::DiagCode::kFormatStringTrailingPercent,
            "format string ends with unfinished '%' directive");
      }

      const char spec_char = text[pos];
      auto kind_or = SpecCharToKind(spec_char);
      if (!kind_or.has_value()) {
        return diag::Error(
            format_span, diag::DiagCode::kFormatStringUnknownSpecifier,
            std::format("unknown format specifier '%{}'", spec_char));
      }
      ++pos;

      out.push_back(
          ParsedFormatDirective{
              .kind = *kind_or,
              .literal = "",
              .modifiers = mods,
              .source_offset = directive_start});
      continue;
    }

    if (c == '\\' && pos + 1 < text.size()) {
      const char esc = text[pos + 1];
      char rendered = '\0';
      bool handled = true;
      switch (esc) {
        case 'n':
          rendered = '\n';
          break;
        case 't':
          rendered = '\t';
          break;
        case '\\':
          rendered = '\\';
          break;
        case '"':
          rendered = '"';
          break;
        default:
          handled = false;
          break;
      }
      if (handled) {
        if (literal.empty()) literal_start_offset = pos;
        literal.push_back(rendered);
        pos += 2;
        continue;
      }
    }

    if (literal.empty()) literal_start_offset = pos;
    literal.push_back(c);
    ++pos;
  }
  flush_literal();
  return out;
}

}  // namespace lyra::support
