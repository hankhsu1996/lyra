#include "lyra/value/format_parse.hpp"

#include <cctype>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <optional>
#include <string>
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
    case 'l':
    case 'L':
      return SpecKind{
          .role = FormatDirective::Role::kLibraryBinding,
          .kind = FormatKind::kString};
    case 'p':
    case 'P':
      return ValueSpec(FormatKind::kAssignmentPattern);
    default:
      return std::nullopt;
  }
}

// LRM 21.2.1.1 Table 21-2 hands the real conversions C's whole formatting
// capability. Everywhere else LRM 21.2.1.2 fixes the fill and the justification
// itself and admits only a field width, so this is what says which of the two
// grammars a directive was written in.
auto IsRealConversion(FormatKind kind) -> bool {
  switch (kind) {
    case FormatKind::kRealDecimal:
    case FormatKind::kRealExponential:
    case FormatKind::kRealGeneral:
      return true;
    case FormatKind::kDecimal:
    case FormatKind::kHex:
    case FormatKind::kBinary:
    case FormatKind::kOctal:
    case FormatKind::kString:
    case FormatKind::kChar:
    case FormatKind::kAssignmentPattern:
    case FormatKind::kTime:
      return false;
  }
  return false;
}

// Whether a conversion fills a field, which a width sizes and a `-` justifies:
// the radix, real, string and time conversions of a value. A character, an
// assignment pattern and the two names `%m` and `%l` print their text as it is,
// though an assignment pattern takes a leading zero.
auto TakesAWidth(const SpecKind& spec) -> bool {
  if (spec.role != FormatDirective::Role::kValue) return false;
  switch (spec.kind) {
    case FormatKind::kDecimal:
    case FormatKind::kHex:
    case FormatKind::kBinary:
    case FormatKind::kOctal:
    case FormatKind::kString:
    case FormatKind::kTime:
    case FormatKind::kRealDecimal:
    case FormatKind::kRealExponential:
    case FormatKind::kRealGeneral:
      return true;
    case FormatKind::kChar:
    case FormatKind::kAssignmentPattern:
      return false;
  }
  return false;
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

      // Which grammar this directive follows is settled by its conversion
      // character, which is read last, so the modifier text is taken as written
      // here and read against that character below. A justification and a
      // leading zero may each be written once, in either order, before the
      // digits of a width. The zero is the one place the same text means
      // different things rather than existing in only one grammar: under C it
      // asks for zero fill, and under LRM 21.2.1.2 it begins the field width.
      bool minus_written = false;
      bool zero_written = false;
      while (pos < text.size()) {
        if (text[pos] == '-' && !minus_written) {
          minus_written = true;
        } else if (text[pos] == '0' && !zero_written) {
          zero_written = true;
        } else {
          break;
        }
        ++pos;
      }

      auto width_or = ParseInt32(text, pos);
      if (!width_or.has_value()) {
        return fail(FormatParseError::kWidthOverflow, directive_start);
      }
      mods.width = *width_or;

      // A point with no digits after it asks for a precision of zero.
      const bool precision_written = pos < text.size() && text[pos] == '.';
      if (precision_written) {
        ++pos;
        auto prec_or = ParseInt32(text, pos);
        if (!prec_or.has_value()) {
          return fail(FormatParseError::kPrecisionOverflow, directive_start);
        }
        mods.precision = *prec_or == -1 ? 0 : *prec_or;
      }

      // A `%` with no conversion after it prints a `%`, and the modifiers
      // written after it nothing.
      if (pos >= text.size()) {
        literal.push_back('%');
        break;
      }

      const char spec_char = text[pos];
      auto spec_or = SpecCharToKind(spec_char);
      if (!spec_or.has_value()) {
        // LRM 21.2.1.1 Table 21-1 defines `%u`, `%z` and `%v`, which are not
        // yet carried out.
        const bool defined = std::string_view{"uUzZvV"}.contains(spec_char);
        return fail(
            defined ? FormatParseError::kUnsupportedSpecifier
                    : FormatParseError::kUnknownSpecifier,
            directive_start, spec_char);
      }
      ++pos;

      if (IsRealConversion(spec_or->kind)) {
        mods.zero_pad = zero_written;
      } else if (
          precision_written ||
          (!TakesAWidth(*spec_or) &&
           (minus_written || mods.width >= 0 ||
            (zero_written &&
             spec_or->kind != FormatKind::kAssignmentPattern)))) {
        return fail(
            FormatParseError::kModifierNotPermitted, directive_start,
            spec_char);
      } else if (zero_written && mods.width < 0) {
        // A lone zero is the whole field width.
        mods.width = 0;
      }
      mods.left_align = minus_written;

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
