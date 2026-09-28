#pragma once

#include <utility>
#include <vector>

#include "lyra/diag/diagnostic.hpp"

namespace lyra::diag {

class DiagnosticSink {
 public:
  void Report(Diagnostic diag) {
    switch (diag.primary.kind) {
      case DiagKind::kError:
      case DiagKind::kUnsupported:
      case DiagKind::kHostError:
        has_errors_ = true;
        break;
      case DiagKind::kWarning:
      case DiagKind::kNote:
      case DiagKind::kRemark:
        break;
    }
    diagnostics_.push_back(std::move(diag));
  }

  [[nodiscard]] auto HasErrors() const -> bool {
    return has_errors_;
  }

  [[nodiscard]] auto Diagnostics() const -> const std::vector<Diagnostic>& {
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

}  // namespace lyra::diag
