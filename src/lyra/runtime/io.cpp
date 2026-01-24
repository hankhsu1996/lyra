#include "lyra/runtime/io.hpp"

#include <cstdint>
#include <cstring>
#include <limits>
#include <optional>
#include <print>
#include <string>
#include <vector>

#include "lyra/common/format.hpp"
#include "lyra/semantic/format.hpp"
#include "lyra/semantic/value.hpp"

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

void SnapshotWideIntegral(const VarEntry& var) {
  const size_t num_words = (static_cast<size_t>(var.width) + 63) / 64;
  std::vector<uint64_t> words(num_words);

  // memcpy avoids alignment/aliasing UB
  std::memcpy(words.data(), var.addr, num_words * sizeof(uint64_t));

  // Mask MSW to semantic width (high bits may be garbage)
  const uint32_t rem = static_cast<uint32_t>(var.width) % 64;
  if (rem != 0) {
    words.back() &= (uint64_t{1} << rem) - 1;
  }

  // Format as hex (MSB first, matching MIR's IntegralToHex)
  std::string hex;
  bool leading = true;
  for (size_t i = num_words; i-- > 0;) {
    if (leading && words[i] == 0 && i > 0) {
      continue;
    }
    if (leading) {
      hex += std::format("{:x}", words[i]);
      leading = false;
    } else {
      hex += std::format("{:016x}", words[i]);
    }
  }
  if (hex.empty()) {
    hex = "0";
  }
  std::print("__LYRA_VAR:h:{}={}\n", var.name, hex);
}

void SnapshotIntegral(const VarEntry& var) {
  if (var.width > 64) {
    SnapshotWideIntegral(var);
    return;
  }

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

// Marshal raw data to RuntimeValue for integral types.
// For now, only handles 2-state values (x_mask and z_mask are null).
auto MarshalIntegral(const void* data, int32_t width)
    -> lyra::semantic::RuntimeValue {
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

  return lyra::semantic::MakeIntegral(value, static_cast<uint32_t>(width));
}

// Convert output_width parameter to FormatSpec width.
// LLVM backend uses: -1 = auto-size, 0 = minimal, >0 = explicit width
auto ConvertWidth(int32_t output_width) -> std::optional<int> {
  if (output_width < 0) {
    // Auto-size: nullopt means semantic layer applies auto-sizing
    return std::nullopt;
  }
  // Minimal (0) or explicit width (>0)
  return output_width;
}

}  // namespace

extern "C" void LyraPrintLiteral(const char* str) {
  std::print("{}", str);
}

extern "C" void LyraPrintValue(
    int32_t format, const void* data, int32_t width, bool is_signed,
    int32_t output_width, int32_t precision, bool zero_pad, bool left_align,
    const void* /*x_mask*/, const void* /*z_mask*/) {
  auto kind = static_cast<lyra::FormatKind>(format);

  // Handle string specially - data IS the string pointer
  if (kind == lyra::FormatKind::kString) {
    std::print("{}", static_cast<const char*>(data));
    return;
  }

  // Build FormatSpec from parameters
  lyra::semantic::FormatSpec spec{
      .kind = kind,
      .width = ConvertWidth(output_width),
      .precision = precision >= 0 ? std::optional(precision) : std::nullopt,
      .zero_pad = zero_pad,
      .left_align = left_align,
  };

  // Marshal data to RuntimeValue and format
  lyra::semantic::RuntimeValue value;
  if (kind == lyra::FormatKind::kReal) {
    if (width == 32) {  // shortreal (float)
      value = lyra::semantic::MakeReal(
          static_cast<double>(*static_cast<const float*>(data)));
    } else {
      value = lyra::semantic::MakeReal(*static_cast<const double*>(data));
    }
  } else {
    value = MarshalIntegral(data, width);
  }

  std::string formatted = lyra::semantic::FormatValue(value, spec, is_signed);
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
