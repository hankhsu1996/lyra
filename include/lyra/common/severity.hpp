#pragma once

#include <cstdint>

namespace lyra {

// Severity levels for $info, $warning, $error system tasks.
// $fatal is handled separately as a terminating statement.
enum class Severity : uint8_t {
  kInfo,
  kWarning,
  kError,
};

// Returns null-terminated prefix string for severity level.
// Safe for both interpreter (string_view) and LLVM (CreateGlobalStringPtr).
// Total function: all enum values covered, compiler warns on missing case.
constexpr auto SeverityPrefixCStr(Severity level) -> const char* {
  switch (level) {
    case Severity::kInfo:
      return "info: ";
    case Severity::kWarning:
      return "warning: ";
    case Severity::kError:
      return "error: ";
  }
  // No default - compiler warns on missing enum case
  // Unreachable in valid code, but needed for non-void return
  return "";
}

// Default format radix for severity messages (decimal)
constexpr char kSeverityDefaultRadix = 'd';

// Default format radix for display messages (hex)
constexpr char kDisplayDefaultRadix = 'h';

}  // namespace lyra
