#pragma once

#include <cstddef>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/common/format.hpp"
#include "lyra/semantic/value.hpp"

namespace lyra::semantic {

// Radix for SV literal formatting
enum class LiteralRadix { kBinary, kHex };

// Format specification for SV literal output (e.g., "8'bx01z0011")
struct SvLiteralFormatSpec {
  LiteralRadix radix = LiteralRadix::kBinary;
};

// Format RuntimeValue as SV literal (e.g., "8'bx01z0011" or "8'hF5")
// This is the ONLY producer of N'... literals - no caller should
// concatenate width + 'b/'h manually.
// For integrals: returns N'b... or N'h... syntax
// For reals: returns plain decimal representation (no N'... prefix)
auto FormatAsSvLiteral(const RuntimeValue& value, SvLiteralFormatSpec spec = {})
    -> std::string;

// Unified format specification for all formatting operations.
// This is the semantic layer's definition - both MIR interpreter and LLVM
// runtime delegate to this.
struct FormatSpec {
  FormatKind kind = FormatKind::kDecimal;
  std::optional<int> width;      // nullopt=auto, 0=minimal, >0=explicit
  std::optional<int> precision;  // for %f
  bool zero_pad = false;         // %0d style padding
  bool left_align = false;       // %-d left alignment
};

// Use std::string as byte buffer (can hold \0, efficient appends).
// Semantic contract: raw bytes, not UTF-8 text.
using ByteBuffer = std::string;

// Argument for formatting - bundles value with signedness info.
struct FormatArg {
  RuntimeValue value;  // Owns the value (move semantics)
  bool is_signed = false;
};

// Error from format operations.
struct FormatError {
  std::string message;
  size_t position;  // Position in format string where error occurred
};

// Result from FormatMessage.
struct FormatResult {
  ByteBuffer output;
  size_t args_consumed;  // Caller handles remaining args
};

// Format a single value with explicit spec.
// This is the core formatting routine that produces the actual output.
auto FormatValue(
    const RuntimeValue& value, const FormatSpec& spec, bool is_signed)
    -> ByteBuffer;

// Format a single value with default format (for $display(x) without format
// string). Uses type-appropriate default: decimal for integrals, etc.
auto FormatDefault(const RuntimeValue& value, bool is_signed) -> ByteBuffer;

// Format a message with format string (pure spec-driven).
// Returns output + number of args consumed. Caller handles extra args.
// Error handling:
// - Too few args: return FormatError at the unmatched % position
// - Too many args: return success with args_consumed < args.size()
// - Unknown spec: return FormatError
// - Invalid % sequence: return FormatError
auto FormatMessage(std::string_view fmt, std::span<const FormatArg> args)
    -> std::expected<FormatResult, FormatError>;

// Convert packed integral to ASCII string (byte extraction).
// LRM 21.2.1.7: Interpret bits as 8-bit ASCII codes, MSB-first.
// - Leading zero bytes are skipped; subsequent zeros become NUL chars.
// - X/Z handling: If ANY bit is X/Z, returns single "x" or "z" string.
//   This is a simplification; the LRM doesn't specify per-byte X/Z handling
//   for string casts, so we collapse to a single unknown indicator.
auto PackedToStringBytes(const RuntimeIntegral& val) -> std::string;

namespace detail {

// Token from parsing a format string.
// INTERNAL ONLY - not part of public API.
// Callers must use FormatMessage, not parse tokens directly.
struct FormatToken {
  std::variant<std::string_view, FormatSpec> content;
};

// Parse a format string into tokens.
// INTERNAL ONLY - exposed for testing only.
auto ParseFormatString(std::string_view fmt) -> std::vector<FormatToken>;

}  // namespace detail

}  // namespace lyra::semantic
