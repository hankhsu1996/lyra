#include "lyra/runtime/io.hpp"

#include <cstdint>
#include <cstring>
#include <format>
#include <limits>
#include <print>
#include <string>
#include <vector>

#include "lyra/common/format.hpp"
#include "lyra/runtime/marshal.hpp"

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

}  // namespace

extern "C" void LyraPrintLiteral(const char* str) {
  std::print("{}", str);
}

extern "C" void LyraPrintValue(
    int32_t format, const void* data, int32_t width, bool is_signed,
    int32_t output_width, int32_t precision, bool zero_pad, bool left_align,
    const void* /*x_mask*/, const void* /*z_mask*/) {
  std::string formatted = lyra::runtime::FormatRuntimeValue(
      static_cast<lyra::FormatKind>(format), data, width, is_signed,
      output_width, precision, zero_pad, left_align);
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
