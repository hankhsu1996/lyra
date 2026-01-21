#include "lyra/runtime/io.hpp"

#include <cstdint>
#include <print>
#include <string>

namespace {

// Format an integral value according to the format kind.
// For now, only handles 2-state values (x_mask and z_mask are null).
auto FormatValue(
    lyra::FormatKind format, const void* data, int32_t width, bool is_signed)
    -> std::string {
  // Read the value - for now, assume it fits in 64 bits
  uint64_t value = 0;
  if (width <= 8) {
    value = *static_cast<const uint8_t*>(data);
  } else if (width <= 16) {
    value = *static_cast<const uint16_t*>(data);
  } else if (width <= 32) {
    value = *static_cast<const uint32_t*>(data);
  } else {
    value = *static_cast<const uint64_t*>(data);
  }

  // Mask to actual width
  if (width < 64) {
    value &= (1ULL << width) - 1;
  }

  switch (format) {
    case lyra::FormatKind::kDecimal:
      if (is_signed && width > 0) {
        // Sign extend if needed
        if (width < 64 && ((value >> (width - 1)) & 1) != 0) {
          value |= ~((1ULL << width) - 1);
        }
        return std::format("{}", static_cast<int64_t>(value));
      }
      return std::format("{}", value);

    case lyra::FormatKind::kHex: {
      int hex_width = (width + 3) / 4;
      return std::format("{:0{}x}", value, hex_width);
    }

    case lyra::FormatKind::kBinary: {
      std::string result;
      result.reserve(width);
      for (int i = width - 1; i >= 0; --i) {
        result += ((value >> i) & 1) != 0 ? '1' : '0';
      }
      return result;
    }

    case lyra::FormatKind::kOctal: {
      int octal_width = (width + 2) / 3;
      return std::format("{:0{}o}", value, octal_width);
    }

    case lyra::FormatKind::kString:
      // For strings, data is the string pointer itself (passed directly)
      // This case is handled specially in LyraPrintValue, not here
      return "";

    case lyra::FormatKind::kLiteral:
      // Should not reach here - literals use LyraPrintLiteral
      return "";
  }
  return "";
}

}  // namespace

extern "C" void LyraPrintLiteral(const char* str) {
  std::print("{}", str);
}

extern "C" void LyraPrintValue(
    int32_t format, const void* data, int32_t width, bool is_signed,
    const void* /*x_mask*/, const void* /*z_mask*/) {
  auto kind = static_cast<lyra::FormatKind>(format);

  if (kind == lyra::FormatKind::kString) {
    // For strings, data IS the string pointer (not a pointer to data)
    std::print("{}", static_cast<const char*>(data));
    return;
  }

  std::string formatted = FormatValue(kind, data, width, is_signed);
  std::print("{}", formatted);
}

extern "C" void LyraPrintEnd(int32_t kind) {
  if (kind == static_cast<int32_t>(lyra::PrintKind::kDisplay)) {
    std::print("\n");
  }
  // No flush here - flush only at simulation end
}
