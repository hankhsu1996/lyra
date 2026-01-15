#pragma once

#include <exception>
#include <expected>
#include <string>

#include "lyra/common/source_span.hpp"

namespace lyra {

enum class DiagnosticSeverity { kNote, kWarning, kError };

struct Diagnostic {
  DiagnosticSeverity severity;
  SourceSpan location;
  std::string message;

  static auto Error(SourceSpan loc, std::string msg) -> Diagnostic {
    return Diagnostic{
        .severity = DiagnosticSeverity::kError,
        .location = loc,
        .message = std::move(msg)};
  }

  static auto Warning(SourceSpan loc, std::string msg) -> Diagnostic {
    return Diagnostic{
        .severity = DiagnosticSeverity::kWarning,
        .location = loc,
        .message = std::move(msg)};
  }

  static auto Note(SourceSpan loc, std::string msg) -> Diagnostic {
    return Diagnostic{
        .severity = DiagnosticSeverity::kNote,
        .location = loc,
        .message = std::move(msg)};
  }
};

template <typename T>
using Result = std::expected<T, Diagnostic>;

class DiagnosticException : public std::exception {
 public:
  explicit DiagnosticException(Diagnostic diag) : diag_(std::move(diag)) {
  }

  [[nodiscard]] auto GetDiagnostic() const -> const Diagnostic& {
    return diag_;
  }
  [[nodiscard]] auto what() const noexcept -> const char* override {
    return diag_.message.c_str();
  }

 private:
  Diagnostic diag_;
};

}  // namespace lyra
