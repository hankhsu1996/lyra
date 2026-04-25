#pragma once

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/diag/source_span.hpp"

namespace lyra::diag {

enum class DiagKind : std::uint8_t {
  kError,
  kUnsupported,
  kHostError,
  kWarning,
  kNote,
};

// Set iff DiagItem::kind == kUnsupported.
enum class UnsupportedCategory : std::uint8_t {
  kType,
  kOperation,
  kFeature,
};

struct UnknownSpan {
  auto operator==(const UnknownSpan&) const -> bool = default;
};

using DiagSpan = std::variant<SourceSpan, UnknownSpan>;

struct DiagItem {
  DiagKind kind;
  DiagSpan span;
  std::string message;
  std::optional<UnsupportedCategory> category;

  auto operator==(const DiagItem&) const -> bool = default;
};

struct Diagnostic {
  DiagItem primary;
  std::vector<DiagItem> notes;

  auto operator==(const Diagnostic&) const -> bool = default;

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

  static auto Unsupported(std::string msg, UnsupportedCategory cat)
      -> Diagnostic {
    return Diagnostic{
        .primary =
            {.kind = DiagKind::kUnsupported,
             .span = UnknownSpan{},
             .message = std::move(msg),
             .category = cat},
        .notes = {},
    };
  }

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

// Construction helpers: build a std::unexpected<Diagnostic> directly so
// lowering call sites read `return diag::Unsupported(...)` instead of
// `return std::unexpected(diag::Diagnostic::Unsupported(...))`.
inline auto Unsupported(
    SourceSpan span, std::string msg, UnsupportedCategory cat)
    -> std::unexpected<Diagnostic> {
  return std::unexpected(Diagnostic::Unsupported(span, std::move(msg), cat));
}

inline auto Unsupported(std::string msg, UnsupportedCategory cat)
    -> std::unexpected<Diagnostic> {
  return std::unexpected(Diagnostic::Unsupported(std::move(msg), cat));
}

inline auto Error(SourceSpan span, std::string msg)
    -> std::unexpected<Diagnostic> {
  return std::unexpected(Diagnostic::Error(span, std::move(msg)));
}

inline auto HostError(std::string msg) -> std::unexpected<Diagnostic> {
  return std::unexpected(Diagnostic::HostError(std::move(msg)));
}

inline auto HostError(SourceSpan span, std::string msg)
    -> std::unexpected<Diagnostic> {
  return std::unexpected(Diagnostic::HostError(span, std::move(msg)));
}

}  // namespace lyra::diag
