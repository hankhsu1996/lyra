#pragma once

#include <vector>

#include "lyra/common/diagnostic/diagnostic.hpp"

namespace lyra {

// Collects diagnostics during compilation. Not thread-safe.
// Diagnostics are stored in order of reporting; callers may rely on this.
class DiagnosticSink {
 public:
  void Report(Diagnostic diag) {
    if (diag.severity == DiagnosticSeverity::kError) {
      has_errors_ = true;
    }
    diagnostics_.push_back(std::move(diag));
  }

  void Error(SourceSpan loc, std::string msg) {
    Report(Diagnostic::Error(loc, std::move(msg)));
  }

  void Warning(SourceSpan loc, std::string msg) {
    Report(Diagnostic::Warning(loc, std::move(msg)));
  }

  void Note(SourceSpan loc, std::string msg) {
    Report(Diagnostic::Note(loc, std::move(msg)));
  }

  [[nodiscard]] auto HasErrors() const -> bool {
    return has_errors_;
  }

  [[nodiscard]] auto GetDiagnostics() const -> const std::vector<Diagnostic>& {
    return diagnostics_;
  }

  void Clear() {
    diagnostics_.clear();
    has_errors_ = false;
  }

 private:
  std::vector<Diagnostic> diagnostics_;
  bool has_errors_ = false;
};

}  // namespace lyra
