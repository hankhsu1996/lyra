#pragma once

#include <cstdint>
#include <optional>

namespace lyra {

// Format kind for display/write operations.
// Determines how a value should be formatted for output.
enum class FormatKind : int32_t {
  kLiteral = -1,  // Literal string (uses LyraPrintLiteral, not LyraPrintValue)
  kDecimal = 0,   // %d
  kHex = 1,       // %h
  kBinary = 2,    // %b
  kOctal = 3,     // %o
  kString = 4,    // %s
  kReal = 5,      // %f
  kTime = 6,      // %t
};

// Print kind for display/write operations.
// Determines whether a newline is appended after output.
enum class PrintKind : int32_t {
  kDisplay = 0,  // $display - appends newline
  kWrite = 1,    // $write - no newline
};

// Format modifiers shared across HIR, MIR, and interpreter.
// Extracted to prevent drift between layers.
struct FormatModifiers {
  std::optional<int> width;      // field width (0 = minimal)
  std::optional<int> precision;  // decimal precision for %f
  bool zero_pad = false;         // %0d style padding
  bool left_align = false;       // %-d left alignment

  auto operator==(const FormatModifiers&) const -> bool = default;
};

// Convert FormatKind to format specifier character.
// Returns '\0' for kLiteral (no spec char).
constexpr auto FormatKindToSpecChar(FormatKind kind) -> char {
  switch (kind) {
    case FormatKind::kDecimal:
      return 'd';
    case FormatKind::kHex:
      return 'h';
    case FormatKind::kBinary:
      return 'b';
    case FormatKind::kOctal:
      return 'o';
    case FormatKind::kString:
      return 's';
    case FormatKind::kReal:
      return 'f';
    case FormatKind::kTime:
      return 't';
    case FormatKind::kLiteral:
      return '\0';
  }
  return '\0';
}

}  // namespace lyra
