#pragma once

#include <cstdint>
#include <exception>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/source_span.hpp"

namespace lyra {

// Type of diagnostic message
enum class DiagKind : uint8_t {
  kError,        // Invalid SV / semantic error
  kUnsupported,  // Valid SV, not implemented
  kHostError,    // I/O, malformed external input
  kWarning,      // Non-fatal
  kNote,         // Auxiliary message
};

// Category of unsupported feature (only valid when kind == kUnsupported)
enum class UnsupportedCategory : uint8_t {
  kType,       // Unsupported type (e.g., wide integers, 4-state)
  kOperation,  // Unsupported operation (e.g., binary op variant)
  kFeature,    // Unsupported language feature (e.g., packed structs)
};

// Represents missing source span (for host errors or when span unavailable)
struct UnknownSpan {
  auto operator==(const UnknownSpan&) const -> bool = default;
};

// A diagnostic span: either a resolved SourceSpan or UnknownSpan
using DiagSpan = std::variant<SourceSpan, UnknownSpan>;

// Single diagnostic item (primary or note)
struct DiagItem {
  DiagKind kind;
  DiagSpan span;
  std::string message;
  std::optional<UnsupportedCategory> category;  // has_value() iff kind ==
                                                // kUnsupported

  auto operator==(const DiagItem&) const -> bool = default;
};

// Complete diagnostic with primary message and optional notes
struct Diagnostic {
  DiagItem primary;
  std::vector<DiagItem> notes;

  auto operator==(const Diagnostic&) const -> bool = default;

  // Factory: semantic error in SV source
  static auto Error(SourceSpan span, std::string msg) -> Diagnostic {
    return Diagnostic{
        .primary =
            {.kind = DiagKind::kError,
             .span = span,
             .message = std::move(msg),
             .category = std::nullopt},
        .notes = {},
    };
  }

  // Factory: valid SV but not yet implemented
  static auto Unsupported(
      SourceSpan span, std::string msg, UnsupportedCategory cat) -> Diagnostic {
    return Diagnostic{
        .primary =
            {.kind = DiagKind::kUnsupported,
             .span = span,
             .message = std::move(msg),
             .category = cat},
        .notes = {},
    };
  }

  // Factory: host error without source location
  static auto HostError(std::string msg) -> Diagnostic {
    return Diagnostic{
        .primary =
            {.kind = DiagKind::kHostError,
             .span = UnknownSpan{},
             .message = std::move(msg),
             .category = std::nullopt},
        .notes = {},
    };
  }

  // Factory: host error with source location (e.g., $readmemh call site)
  static auto HostError(SourceSpan span, std::string msg) -> Diagnostic {
    return Diagnostic{
        .primary =
            {.kind = DiagKind::kHostError,
             .span = span,
             .message = std::move(msg),
             .category = std::nullopt},
        .notes = {},
    };
  }

  // Factory: warning
  static auto Warning(SourceSpan span, std::string msg) -> Diagnostic {
    return Diagnostic{
        .primary =
            {.kind = DiagKind::kWarning,
             .span = span,
             .message = std::move(msg),
             .category = std::nullopt},
        .notes = {},
    };
  }

  // Add a note with source location
  auto WithNote(SourceSpan span, std::string msg) && -> Diagnostic {
    notes.push_back(
        DiagItem{
            .kind = DiagKind::kNote,
            .span = span,
            .message = std::move(msg),
            .category = std::nullopt,
        });
    return std::move(*this);
  }

  // Add a note without source location
  auto WithNote(std::string msg) && -> Diagnostic {
    notes.push_back(
        DiagItem{
            .kind = DiagKind::kNote,
            .span = UnknownSpan{},
            .message = std::move(msg),
            .category = std::nullopt,
        });
    return std::move(*this);
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
    return diag_.primary.message.c_str();
  }

 private:
  Diagnostic diag_;
};

}  // namespace lyra
