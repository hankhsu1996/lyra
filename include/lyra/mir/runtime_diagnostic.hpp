#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/mir/runtime_print.hpp"

namespace lyra::mir {

enum class DiagnosticSeverity : std::uint8_t {
  kInfo,
  kWarning,
  kError,
};

// Resolved source location of the diagnostic-emitting call. Carries plain
// file:line:col so the backend can render a runtime literal without needing
// the SourceManager at emit time. Absent when the lowering layer cannot
// resolve the call's location.
struct DiagnosticOrigin {
  std::string file;
  std::uint32_t line = 0;
  std::uint32_t col = 0;
};

struct RuntimeDiagnosticCall {
  DiagnosticSeverity severity;
  std::optional<DiagnosticOrigin> origin;
  std::vector<RuntimePrintItem> items;

  RuntimeDiagnosticCall(
      DiagnosticSeverity s, std::optional<DiagnosticOrigin> o,
      std::vector<RuntimePrintItem> i)
      : severity(s), origin(std::move(o)), items(std::move(i)) {
  }
};

}  // namespace lyra::mir
