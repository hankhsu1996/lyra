#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"

namespace lyra::support {

enum class FormatDirectiveKind : std::uint8_t {
  kLiteral,
  kDecimal,
  kHex,
  kBinary,
  kOctal,
  kString,
  kReal,
  kTime,
  kChar,
  kModulePath,
};

struct FormatDirectiveModifiers {
  std::int32_t width = -1;
  std::int32_t precision = -1;
  bool zero_pad = false;
  bool left_align = false;
};

struct ParsedFormatDirective {
  FormatDirectiveKind kind;
  std::string literal;
  FormatDirectiveModifiers modifiers;
  std::size_t source_offset = 0;
};

// Parse an SV format string literal into directives. Errors are reported with
// the supplied source span (typically the format-string argument's span).
[[nodiscard]] auto ParseLiteralFormatString(
    std::string_view text, diag::SourceSpan format_span)
    -> diag::Result<std::vector<ParsedFormatDirective>>;

[[nodiscard]] auto FormatDirectiveNeedsValue(FormatDirectiveKind kind) -> bool;

}  // namespace lyra::support
