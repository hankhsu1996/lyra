#include "lyra/mir/interp/format.hpp"

#include <cctype>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

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
    spec.spec = str[pos];
    ++pos;
  }

  return {spec, pos};
}

// Apply width and alignment to a formatted string with custom padding character
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

// Apply width and alignment to a formatted string
auto ApplyWidth(std::string value, const FormatSpec& spec) -> std::string {
  char pad_char = spec.zero_pad ? '0' : ' ';
  return ApplyWidthWithChar(std::move(value), spec, pad_char);
}

// Check if a type is signed (handles kIntegral and packed array types).
auto IsSignedIntegral(TypeId type, const TypeArena& types) -> bool {
  if (!type) {
    return false;
  }
  const auto& ty = types[type];
  if (ty.Kind() == TypeKind::kIntegral) {
    return ty.AsIntegral().is_signed;
  }
  if (IsPacked(ty)) {
    return IsPackedSigned(ty, types);
  }
  return false;
}

// Calculate auto-sizing width for hex/binary/octal based on bit width.
// Returns nullopt if no auto-sizing needed.
auto CalculateAutoWidth(char spec, uint32_t bit_width) -> std::optional<int> {
  switch (spec) {
    case 'h':
    case 'x':
      // Hex: ceil(bit_width / 4)
      return static_cast<int>((bit_width + 3) / 4);
    case 'b':
      // Binary: exact bit width
      return static_cast<int>(bit_width);
    case 'o':
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

// Convert integral to ASCII string for %s format.
// LRM 21.2.1.7: Interpret bits as 8-bit ASCII codes, LSB-aligned.
// Leading zero bytes are skipped.
auto IntegralToAscii(const RuntimeIntegral& val) -> std::string {
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
    size_t bit_offset = (byte_idx - 1) * 8;
    size_t word_idx = bit_offset / 64;
    size_t bit_in_word = bit_offset % 64;

    uint8_t byte_val = 0;
    if (word_idx < val.value.size()) {
      byte_val =
          static_cast<uint8_t>((val.value[word_idx] >> bit_in_word) & 0xFF);
    }

    if (byte_val != 0 || found_nonzero) {
      found_nonzero = true;
      result += static_cast<char>(byte_val);
    }
  }

  return result;
}

// Format an integral value according to spec.
// For %d, consults type to determine signedness.
// For %h/%b/%o without explicit width, auto-sizes to full bit width.
auto FormatIntegral(
    const RuntimeIntegral& val, const FormatSpec& spec, bool is_signed)
    -> std::string {
  std::string result;

  switch (spec.spec) {
    case 'd':
      // %d uses type-derived signedness
      result = ToDecimalString(val, is_signed);
      break;
    case 'h':
    case 'x':
      result = ToHexString(val);
      break;
    case 'b':
      result = ToBinaryString(val);
      break;
    case 'o':
      result = ToOctalString(val);
      break;
    case 's':
      // LRM 21.2.1.7: %s interprets bits as ASCII characters
      result = IntegralToAscii(val);
      break;
    default:
      result = ToDecimalString(val, false);
      break;
  }

  // Apply width: either explicit or auto-sized
  FormatSpec effective_spec = spec;
  char pad_char = spec.zero_pad ? '0' : ' ';
  if (!spec.width.has_value()) {
    // No explicit width - apply auto-sizing for hex/binary/octal
    auto auto_width = CalculateAutoWidth(spec.spec, val.bit_width);
    if (auto_width.has_value()) {
      effective_spec.width = *auto_width;
      effective_spec.zero_pad = true;
      // For X/Z values, pad with 'x' or 'z' instead of '0'
      pad_char = GetAutoPadChar(val);
    }
  }

  return ApplyWidthWithChar(std::move(result), effective_spec, pad_char);
}

// Format a real value according to spec
auto FormatReal(const RuntimeReal& val, const FormatSpec& spec) -> std::string {
  std::string result;

  int precision = spec.precision.value_or(6);  // Default precision for %f

  switch (spec.spec) {
    case 'f':
      result = std::format("{:.{}f}", val.value, precision);
      break;
    case 'e':
      result = std::format("{:.{}e}", val.value, precision);
      break;
    case 'g':
      result = std::format("{:.{}g}", val.value, precision);
      break;
    default:
      result = std::format("{:.{}f}", val.value, precision);
      break;
  }

  return ApplyWidth(std::move(result), spec);
}

// Format a string value according to spec
auto FormatString(const RuntimeString& val, const FormatSpec& spec)
    -> std::string {
  return ApplyWidth(val.value, spec);
}

// Auto-format a typed value using the default format character.
// For integrals, uses type-derived signedness when default_format is 'd'.
// For hex/binary/octal, applies LRM auto-sizing (full bit width with leading
// zeros).
auto AutoFormat(
    const TypedValue& arg, char default_format, const TypeArena& types)
    -> std::string {
  const auto& value = arg.value;

  if (IsString(value)) {
    return AsString(value).value;
  }

  if (IsReal(value)) {
    return std::format("{:g}", AsReal(value).value);
  }

  if (IsIntegral(value)) {
    const auto& integral = AsIntegral(value);
    bool is_signed = IsSignedIntegral(arg.type, types);
    std::string result;

    switch (default_format) {
      case 'd':
        return ToDecimalString(integral, is_signed);
      case 'h':
      case 'x':
        result = ToHexString(integral);
        break;
      case 'b':
        result = ToBinaryString(integral);
        break;
      case 'o':
        result = ToOctalString(integral);
        break;
      default:
        return ToDecimalString(integral, false);
    }

    // Apply auto-sizing for hex/binary/octal
    auto auto_width = CalculateAutoWidth(default_format, integral.bit_width);
    if (auto_width.has_value()) {
      auto width = static_cast<size_t>(*auto_width);
      if (result.size() < width) {
        // For X/Z values, pad with 'x' or 'z' instead of '0'
        char pad_char = GetAutoPadChar(integral);
        result = std::string(width - result.size(), pad_char) + result;
      }
    }
    return result;
  }

  return ToString(value);
}

}  // namespace

auto FormatValue(
    const TypedValue& arg, const FormatSpec& spec, const TypeArena& types,
    const FormatContext& /*ctx*/) -> std::string {
  const auto& value = arg.value;

  if (IsIntegral(value)) {
    bool is_signed = IsSignedIntegral(arg.type, types);
    return FormatIntegral(AsIntegral(value), spec, is_signed);
  }

  if (IsReal(value)) {
    return FormatReal(AsReal(value), spec);
  }

  if (IsString(value)) {
    return FormatString(AsString(value), spec);
  }

  // Fallback for other types
  return ApplyWidth(ToString(value), spec);
}

auto FormatDisplay(
    std::string_view fmt, std::span<const TypedValue> args, char default_format,
    const TypeArena& types, const FormatContext& ctx) -> std::string {
  std::string result;
  size_t arg_idx = 0;
  size_t pos = 0;

  while (pos < fmt.size()) {
    if (fmt[pos] != '%') {
      result += fmt[pos];
      ++pos;
      continue;
    }

    // Found '%'
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
      result += FormatValue(args[arg_idx], spec, types, ctx);
      ++arg_idx;
    } else {
      // Not enough arguments: keep specifier verbatim
      result += '%';
      result += fmt.substr(pos - consumed, consumed);
    }
  }

  // Append remaining arguments using auto-format (no separator per LRM)
  while (arg_idx < args.size()) {
    result += AutoFormat(args[arg_idx], default_format, types);
    ++arg_idx;
  }

  return result;
}

auto FormatMessage(
    std::span<const TypedValue> args, char default_format,
    const TypeArena& types, const FormatContext& ctx) -> std::string {
  if (args.empty()) {
    return "";
  }

  // Check if first argument is a string
  if (IsString(args[0].value)) {
    const auto& first_str = AsString(args[0].value).value;

    // If string contains any '%', treat as format string
    if (first_str.find('%') != std::string::npos) {
      return FormatDisplay(
          first_str, args.subspan(1), default_format, types, ctx);
    }

    // String without '%': output as prefix + auto-format rest (no separator)
    std::string result = first_str;
    for (size_t i = 1; i < args.size(); ++i) {
      result += AutoFormat(args[i], default_format, types);
    }
    return result;
  }

  // First arg is not a string: auto-format all (no separator)
  std::string result;
  for (const auto& arg : args) {
    result += AutoFormat(arg, default_format, types);
  }
  return result;
}

}  // namespace lyra::mir::interp
