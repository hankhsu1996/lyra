#include "lyra/semantic/format.hpp"

#include <cctype>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/common/format.hpp"
#include "lyra/semantic/value.hpp"

namespace lyra::semantic {

namespace {

// Parse a format specifier starting after '%'.
// Returns the spec and the number of characters consumed.
// SV format: %[-][0][width][.precision]specifier
// Note: "%0h" means width=0 (minimal), not "zero-pad with no width"
auto ParseFormatSpec(std::string_view str) -> std::pair<FormatSpec, size_t> {
  FormatSpec spec;
  size_t pos = 0;

  // Parse '-' flag for left-align
  while (pos < str.size() && str[pos] == '-') {
    spec.left_align = true;
    ++pos;
  }

  // Parse optional '0' prefix and width
  // "%0h" → width=0 (minimal)
  // "%08h" → zero_pad + width=8
  // "%8h" → width=8
  if (pos < str.size() && std::isdigit(str[pos]) != 0) {
    if (str[pos] == '0') {
      ++pos;
      if (pos < str.size() && std::isdigit(str[pos]) != 0) {
        // "0N..." where N is a digit → zero_pad + width=N...
        spec.zero_pad = true;
        int width = 0;
        while (pos < str.size() && std::isdigit(str[pos]) != 0) {
          width = width * 10 + (str[pos] - '0');
          ++pos;
        }
        spec.width = width;
      } else {
        // Just "0" followed by specifier → width=0 (minimal)
        spec.width = 0;
      }
    } else {
      // Non-zero digit → width
      int width = 0;
      while (pos < str.size() && std::isdigit(str[pos]) != 0) {
        width = width * 10 + (str[pos] - '0');
        ++pos;
      }
      spec.width = width;
    }
  }

  // Parse precision (.digits)
  if (pos < str.size() && str[pos] == '.') {
    ++pos;
    int precision = 0;
    while (pos < str.size() && std::isdigit(str[pos]) != 0) {
      precision = precision * 10 + (str[pos] - '0');
      ++pos;
    }
    spec.precision = precision;
  }

  // Parse specifier character
  if (pos < str.size()) {
    char spec_char = str[pos];
    ++pos;

    // Map specifier character to FormatKind
    switch (spec_char) {
      case 'd':
        spec.kind = FormatKind::kDecimal;
        break;
      case 'h':
      case 'x':
      case 'H':
      case 'X':
        spec.kind = FormatKind::kHex;
        break;
      case 'b':
      case 'B':
        spec.kind = FormatKind::kBinary;
        break;
      case 'o':
      case 'O':
        spec.kind = FormatKind::kOctal;
        break;
      case 's':
        spec.kind = FormatKind::kString;
        break;
      case 'f':
      case 'e':
      case 'g':
        spec.kind = FormatKind::kReal;
        break;
      default:
        // Unknown specifier - will be caught by caller
        spec.kind = FormatKind::kDecimal;
        break;
    }
  }

  return {spec, pos};
}

// Apply width and alignment to a formatted string with custom padding
// character.
auto ApplyWidthWithChar(
    std::string value, const FormatSpec& spec, char pad_char) -> std::string {
  if (!spec.width || *spec.width <= 0) {
    return value;
  }

  auto width = static_cast<size_t>(*spec.width);
  if (value.size() >= width) {
    return value;
  }
  size_t pad_count = width - value.size();

  if (spec.left_align) {
    return value + std::string(pad_count, ' ');
  }

  // For zero-padding negative numbers, put sign before the zeros
  // e.g., %05d on -5 should be "-0005", not "00-5"
  if (pad_char == '0' && !value.empty() && value[0] == '-') {
    return "-" + std::string(pad_count, '0') + value.substr(1);
  }

  return std::string(pad_count, pad_char) + value;
}

// Apply width and alignment to a formatted string.
auto ApplyWidth(std::string value, const FormatSpec& spec) -> std::string {
  char pad_char = spec.zero_pad ? '0' : ' ';
  return ApplyWidthWithChar(std::move(value), spec, pad_char);
}

// Calculate auto-sizing width for hex/binary/octal based on bit width.
// Returns nullopt if no auto-sizing needed.
auto CalculateAutoWidth(FormatKind kind, uint32_t bit_width)
    -> std::optional<int> {
  switch (kind) {
    case FormatKind::kHex:
      // Hex: ceil(bit_width / 4)
      return static_cast<int>((bit_width + 3) / 4);
    case FormatKind::kBinary:
      // Binary: exact bit width
      return static_cast<int>(bit_width);
    case FormatKind::kOctal:
      // Octal: ceil(bit_width / 3)
      return static_cast<int>((bit_width + 2) / 3);
    default:
      return std::nullopt;
  }
}

// Determine the padding character for auto-sizing based on X/Z state.
// LRM 21.2.1.3: For all-X, pad with 'x'; for all-Z, pad with 'z'; otherwise
// '0'.
auto GetAutoPadChar(const RuntimeIntegral& val) -> char {
  if (val.IsX()) {
    return 'x';
  }
  if (val.IsZ()) {
    return 'z';
  }
  return '0';
}

// Format an integral value according to spec.
auto FormatIntegral(
    const RuntimeIntegral& val, const FormatSpec& spec, bool is_signed)
    -> std::string {
  std::string result;

  switch (spec.kind) {
    case FormatKind::kDecimal:
      // %d uses type-derived signedness
      result = ToDecimalString(val, is_signed);
      break;
    case FormatKind::kHex:
      result = ToHexString(val);
      break;
    case FormatKind::kBinary:
      result = ToBinaryString(val);
      break;
    case FormatKind::kOctal:
      result = ToOctalString(val);
      break;
    case FormatKind::kString:
      // LRM 21.2.1.7: %s interprets bits as ASCII characters
      result = PackedToStringBytes(val);
      break;
    case FormatKind::kReal:
    case FormatKind::kLiteral:
      result = ToDecimalString(val, false);
      break;
  }

  // Apply width: either explicit or auto-sized
  FormatSpec effective_spec = spec;
  char pad_char = spec.zero_pad ? '0' : ' ';
  if (!spec.width.has_value()) {
    // No explicit width - apply auto-sizing for hex/binary/octal
    auto auto_width = CalculateAutoWidth(spec.kind, val.bit_width);
    if (auto_width.has_value()) {
      effective_spec.width = *auto_width;
      effective_spec.zero_pad = true;
      // For X/Z values, pad with 'x' or 'z' instead of '0'
      pad_char = GetAutoPadChar(val);
    }
  }

  return ApplyWidthWithChar(std::move(result), effective_spec, pad_char);
}

// Format a real value according to spec.
auto FormatReal(const RuntimeReal& val, const FormatSpec& spec) -> std::string {
  int precision = spec.precision.value_or(6);  // Default precision for %f
  std::string result = std::format("{:.{}f}", val.value, precision);
  return ApplyWidth(std::move(result), spec);
}

// Format a string value according to spec.
auto FormatString(const RuntimeString& val, const FormatSpec& spec)
    -> std::string {
  return ApplyWidth(val.value, spec);
}

// Auto-format an integral value for default display.
auto AutoFormatIntegral(const RuntimeIntegral& val, bool is_signed)
    -> std::string {
  return ToDecimalString(val, is_signed);
}

}  // namespace

auto PackedToStringBytes(const RuntimeIntegral& val) -> std::string {
  if (val.IsX() || val.IsZ()) {
    // X/Z values can't be meaningfully converted to ASCII
    return val.IsX() ? "x" : "z";
  }

  std::string result;
  // Process bytes from MSB to LSB, skip leading zeros
  bool found_nonzero = false;

  // Calculate number of bytes needed
  size_t num_bytes = (val.bit_width + 7) / 8;

  for (size_t byte_idx = num_bytes; byte_idx > 0; --byte_idx) {
    // Byte extraction never crosses 64-bit word boundaries because bit_offset
    // is always a multiple of 8, making bit_in_word one of {0,8,16,...,56}.
    size_t bit_offset = (byte_idx - 1) * 8;
    size_t word_idx = bit_offset / 64;
    size_t bit_in_word = bit_offset % 64;

    uint8_t byte_val = 0;
    if (word_idx < val.a.size()) {
      byte_val = static_cast<uint8_t>((val.a[word_idx] >> bit_in_word) & 0xFF);
    }

    if (byte_val != 0 || found_nonzero) {
      found_nonzero = true;
      result += static_cast<char>(byte_val);
    }
  }

  return result;
}

auto FormatValue(
    const RuntimeValue& value, const FormatSpec& spec, bool is_signed)
    -> ByteBuffer {
  if (IsIntegral(value)) {
    return FormatIntegral(AsIntegral(value), spec, is_signed);
  }

  if (IsReal(value)) {
    return FormatReal(AsReal(value), spec);
  }

  if (IsShortReal(value)) {
    // Format shortreal as double for display
    RuntimeReal r{.value = static_cast<double>(AsShortReal(value).value)};
    return FormatReal(r, spec);
  }

  if (IsString(value)) {
    return FormatString(AsString(value), spec);
  }

  // Fallback for other types
  return ApplyWidth(ToString(value), spec);
}

auto FormatDefault(const RuntimeValue& value, bool is_signed) -> ByteBuffer {
  if (IsIntegral(value)) {
    return AutoFormatIntegral(AsIntegral(value), is_signed);
  }

  if (IsReal(value)) {
    return std::format("{:g}", AsReal(value).value);
  }

  if (IsShortReal(value)) {
    return std::format("{:g}", static_cast<double>(AsShortReal(value).value));
  }

  if (IsString(value)) {
    return AsString(value).value;
  }

  return ToString(value);
}

auto FormatMessage(std::string_view fmt, std::span<const FormatArg> args)
    -> std::expected<FormatResult, FormatError> {
  ByteBuffer result;
  size_t arg_idx = 0;
  size_t pos = 0;

  while (pos < fmt.size()) {
    if (fmt[pos] != '%') {
      result += fmt[pos];
      ++pos;
      continue;
    }

    // Found '%'
    size_t percent_pos = pos;
    if (pos + 1 >= fmt.size()) {
      // Trailing '%' with nothing after
      result += '%';
      ++pos;
      continue;
    }

    // Check for %%
    if (fmt[pos + 1] == '%') {
      result += '%';
      pos += 2;
      continue;
    }

    // Parse the format specifier
    auto [spec, consumed] = ParseFormatSpec(fmt.substr(pos + 1));
    pos += 1 + consumed;

    // Consume an argument if available
    if (arg_idx < args.size()) {
      const auto& arg = args[arg_idx];
      result += FormatValue(arg.value, spec, arg.is_signed);
      ++arg_idx;
    } else {
      // Not enough arguments
      return std::unexpected(
          FormatError{
              .message = "Not enough arguments for format specifier",
              .position = percent_pos,
          });
    }
  }

  return FormatResult{
      .output = std::move(result),
      .args_consumed = arg_idx,
  };
}

namespace detail {

auto ParseFormatString(std::string_view fmt) -> std::vector<FormatToken> {
  std::vector<FormatToken> tokens;
  size_t pos = 0;
  size_t literal_start = 0;

  while (pos < fmt.size()) {
    if (fmt[pos] != '%') {
      ++pos;
      continue;
    }

    // Emit literal if we have one
    if (pos > literal_start) {
      tokens.push_back(
          FormatToken{
              .content = fmt.substr(literal_start, pos - literal_start)});
    }

    if (pos + 1 >= fmt.size()) {
      // Trailing '%' with nothing after - treat as literal
      tokens.push_back(FormatToken{.content = fmt.substr(pos)});
      break;
    }

    // Check for %%
    if (fmt[pos + 1] == '%') {
      tokens.push_back(FormatToken{.content = std::string_view("%")});
      pos += 2;
      literal_start = pos;
      continue;
    }

    // Parse the format specifier
    auto [spec, consumed] = ParseFormatSpec(fmt.substr(pos + 1));
    tokens.push_back(FormatToken{.content = spec});
    pos += 1 + consumed;
    literal_start = pos;
  }

  // Emit trailing literal if we have one
  if (pos > literal_start) {
    tokens.push_back(
        FormatToken{.content = fmt.substr(literal_start, pos - literal_start)});
  }

  return tokens;
}

}  // namespace detail

}  // namespace lyra::semantic
