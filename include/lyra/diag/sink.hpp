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
      case DiagKind::kInternalError:
        has_errors_ = true;
        has_internal_errors_ = true;
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

  // Whether any of the errors was the compiler's own failure, which a caller
  // answers with a different exit status than a design it refused.
  [[nodiscard]] auto HasInternalErrors() const -> bool {
    return has_internal_errors_;
  }

  [[nodiscard]] auto Diagnostics() const -> const std::vector<Diagnostic>& {
    return diagnostics_;
  }

  void Clear() {
    diagnostics_.clear();
    has_errors_ = false;
    has_internal_errors_ = false;
  }

 private:
  std::vector<Diagnostic> diagnostics_;
  bool has_errors_ = false;
  bool has_internal_errors_ = false;
};

}  // namespace lyra::diag
