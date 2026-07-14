#pragma once

#include <cstddef>
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/value/format.hpp"

namespace lyra::value {

// The width / precision / justification modifiers an LRM 21.2.1.1 format
// directive may carry ahead of its conversion character. `-1` on width or
// precision means the directive left it unset, so the formatter uses its
// per-kind default.
struct FormatModifiers {
  std::int32_t width = -1;
  std::int32_t precision = -1;
  bool zero_pad = false;
  bool left_align = false;
};

// One piece of a parsed format string. A directive is a run of literal text, a
// `%m` hierarchical-name directive that consumes no operand (LRM 21.2.1.1), or
// a value directive that consumes one operand and formats it under `kind`.
struct FormatDirective {
  enum class Role : std::uint8_t {
    kLiteral,
    kModulePath,
    kValue,
  };
  Role role = Role::kLiteral;
  std::string literal;
  FormatKind kind = FormatKind::kDecimal;
  FormatModifiers modifiers;
};

// The ways a directive can be malformed. Reported as a plain value rather than
// a diagnostic because the same parse runs at simulation time, where the
// compile-time diagnostic vocabulary does not exist.
enum class FormatParseError : std::uint8_t {
  kNone,
  kMissingSpecifier,
  kTrailingPercent,
  kUnknownSpecifier,
  kWidthOverflow,
  kPrecisionOverflow,
  kMissingPrecision,
};

// Either the directives or the first malformed one, never both. `error_offset`
// locates the offending `%` within the format text, so a compile-time caller
// can resolve it against the format string's source span; `spec_char` carries
// the conversion character an unknown-specifier error rejected.
struct FormatParseResult {
  std::vector<FormatDirective> directives;
  FormatParseError error = FormatParseError::kNone;
  std::size_t error_offset = 0;
  char spec_char = '\0';
};

// Parse an SV format string into directives (LRM 21.2.1.1). The grammar has no
// dependency on the compile-time diagnostic layer, so one parse serves both a
// literal format string, whose directives are known at compile time, and one
// carried as a value and known only at simulation time (LRM 21.3.3).
[[nodiscard]] auto ParseFormatString(std::string_view text)
    -> FormatParseResult;

}  // namespace lyra::value
