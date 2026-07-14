#include "lyra/value/format_parse.hpp"

#include <cctype>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <optional>
#include <string_view>
#include <utility>

#include "lyra/value/format.hpp"

namespace lyra::value {

namespace {

struct SpecKind {
  FormatDirective::Role role;
  FormatKind kind;
};

auto ValueSpec(FormatKind kind) -> SpecKind {
  return SpecKind{.role = FormatDirective::Role::kValue, .kind = kind};
}

auto SpecCharToKind(char c) -> std::optional<SpecKind> {
  switch (c) {
    case 'd':
    case 'D':
      return ValueSpec(FormatKind::kDecimal);
    case 'h':
    case 'H':
    case 'x':
    case 'X':
      return ValueSpec(FormatKind::kHex);
    case 'b':
    case 'B':
      return ValueSpec(FormatKind::kBinary);
    case 'o':
    case 'O':
      return ValueSpec(FormatKind::kOctal);
    case 's':
    case 'S':
      return ValueSpec(FormatKind::kString);
    case 'f':
    case 'F':
      return ValueSpec(FormatKind::kRealDecimal);
    case 'e':
    case 'E':
      return ValueSpec(FormatKind::kRealExponential);
    case 'g':
    case 'G':
      return ValueSpec(FormatKind::kRealGeneral);
    case 't':
    case 'T':
      return ValueSpec(FormatKind::kTime);
    case 'c':
    case 'C':
      return ValueSpec(FormatKind::kChar);
    case 'm':
    case 'M':
      return SpecKind{
          .role = FormatDirective::Role::kModulePath,
          .kind = FormatKind::kString};
    case 'p':
    case 'P':
      return ValueSpec(FormatKind::kAssignmentPattern);
    default:
      return std::nullopt;
  }
}

// Parses a digit run as a non-negative int32. Returns std::nullopt on overflow,
// and -1 (through the value) when no digit is present.
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
  if (!any_digits) return -1;
  return static_cast<std::int32_t>(value);
}

}  // namespace

auto ParseFormatString(std::string_view text) -> FormatParseResult {
  FormatParseResult result;
  std::string literal;
  std::size_t pos = 0;

  auto flush_literal = [&] {
    if (!literal.empty()) {
      result.directives.push_back(
          FormatDirective{
              .role = FormatDirective::Role::kLiteral,
              .literal = std::move(literal),
              .kind = FormatKind::kDecimal,
              .modifiers = {}});
      literal.clear();
    }
  };

  auto fail = [&](FormatParseError error, std::size_t offset,
                  char spec = '\0') -> FormatParseResult {
    return FormatParseResult{
        .directives = {},
        .error = error,
        .error_offset = offset,
        .spec_char = spec};
  };

  while (pos < text.size()) {
    const char c = text[pos];

    if (c == '%') {
      if (pos + 1 < text.size() && text[pos + 1] == '%') {
        literal.push_back('%');
        pos += 2;
        continue;
      }
      flush_literal();
      const std::size_t directive_start = pos;
      ++pos;

      FormatModifiers mods;

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
        // LRM 21.2.1.1 minimal-width marker `%0X`: zero pad with no width run.
        mods.zero_pad = true;
        ++pos;
        mods.width = 0;
        if (pos >= text.size()) {
          return fail(FormatParseError::kMissingSpecifier, directive_start);
        }
      }

      if (mods.width != 0) {
        auto width_or = ParseInt32(text, pos);
        if (!width_or.has_value()) {
          return fail(FormatParseError::kWidthOverflow, directive_start);
        }
        if (*width_or != -1) {
          mods.width = *width_or;
        }
      }

      if (pos < text.size() && text[pos] == '.') {
        ++pos;
        auto prec_or = ParseInt32(text, pos);
        if (!prec_or.has_value()) {
          return fail(FormatParseError::kPrecisionOverflow, directive_start);
        }
        if (*prec_or == -1) {
          return fail(FormatParseError::kMissingPrecision, directive_start);
        }
        mods.precision = *prec_or;
      }

      if (pos >= text.size()) {
        return fail(FormatParseError::kTrailingPercent, directive_start);
      }

      const char spec_char = text[pos];
      auto spec_or = SpecCharToKind(spec_char);
      if (!spec_or.has_value()) {
        return fail(
            FormatParseError::kUnknownSpecifier, directive_start, spec_char);
      }
      ++pos;

      result.directives.push_back(
          FormatDirective{
              .role = spec_or->role,
              .literal = "",
              .kind = spec_or->kind,
              .modifiers = mods});
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
        literal.push_back(rendered);
        pos += 2;
        continue;
      }
    }

    literal.push_back(c);
    ++pos;
  }
  flush_literal();
  return result;
}

}  // namespace lyra::value
