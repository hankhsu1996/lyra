#pragma once

#include <exception>
#include <expected>
#include <string>

#include <slang/text/SourceLocation.h>
#include <slang/text/SourceManager.h>

namespace lyra {

enum class DiagnosticSeverity { kNote, kWarning, kError };

struct Diagnostic {
  DiagnosticSeverity severity;
  slang::SourceRange location;
  std::string message;

  static auto Error(slang::SourceRange loc, std::string msg) -> Diagnostic {
    return Diagnostic{
        .severity = DiagnosticSeverity::kError,
        .location = loc,
        .message = std::move(msg)};
  }

  static auto Warning(slang::SourceRange loc, std::string msg) -> Diagnostic {
    return Diagnostic{
        .severity = DiagnosticSeverity::kWarning,
        .location = loc,
        .message = std::move(msg)};
  }

  static auto Note(slang::SourceRange loc, std::string msg) -> Diagnostic {
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

// Print diagnostic with source context (requires SourceManager)
void PrintDiagnostic(
    const Diagnostic& diag, const slang::SourceManager& sm, bool colors = true);

// Print diagnostic without source context (for errors without source location)
void PrintDiagnostic(const Diagnostic& diag, bool colors = true);

}  // namespace lyra
