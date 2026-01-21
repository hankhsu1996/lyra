#include "lyra/runtime/io.hpp"

#include <cstdint>
#include <cstring>
#include <limits>
#include <print>
#include <string>
#include <vector>

#include "lyra/common/format.hpp"

namespace {

enum class VarKind : int32_t {
  kIntegral = 0,
  kReal = 1,
};

struct VarEntry {
  std::string name;
  void* addr;
  VarKind kind;
  int32_t width;
  bool is_signed;
};

// NOLINTBEGIN(cppcoreguidelines-avoid-non-const-global-variables)
std::vector<VarEntry> g_registered_vars;
// NOLINTEND(cppcoreguidelines-avoid-non-const-global-variables)

void SnapshotIntegral(const VarEntry& var) {
  uint64_t raw = 0;
  std::memcpy(&raw, var.addr, static_cast<size_t>((var.width + 7) / 8));

  // Mask to actual width
  if (var.width < 64) {
    raw &= (1ULL << var.width) - 1;
  }

  if (var.is_signed) {
    // Sign-extend if MSB is set
    auto value = static_cast<int64_t>(raw);
    if (var.width < 64) {
      uint64_t sign_bit = 1ULL << (var.width - 1);
      if ((raw & sign_bit) != 0) {
        value = static_cast<int64_t>(raw | ~((1ULL << var.width) - 1));
      }
    }
    std::print("__LYRA_VAR:i:{}={}\n", var.name, value);
  } else {
    std::print("__LYRA_VAR:i:{}={}\n", var.name, raw);
  }
}

void SnapshotReal(const VarEntry& var) {
  double value = 0.0;
  std::memcpy(&value, var.addr, sizeof(double));
  // Use max_digits10 (17 for double) to ensure round-trip precision
  std::print(
      "__LYRA_VAR:r:{}={:.{}g}\n", var.name, value,
      std::numeric_limits<double>::max_digits10);
}

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

    case lyra::FormatKind::kReal:
      // Real values - data points to a double
      return std::format("{:f}", *static_cast<const double*>(data));

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

extern "C" void LyraRegisterVar(
    const char* name, void* addr, int32_t kind, int32_t width, bool is_signed) {
  g_registered_vars.push_back(
      {name, addr, static_cast<VarKind>(kind), width, is_signed});
}

extern "C" void LyraSnapshotVars() {
  for (const auto& var : g_registered_vars) {
    switch (var.kind) {
      case VarKind::kIntegral:
        SnapshotIntegral(var);
        break;
      case VarKind::kReal:
        SnapshotReal(var);
        break;
    }
  }
  g_registered_vars.clear();
}
