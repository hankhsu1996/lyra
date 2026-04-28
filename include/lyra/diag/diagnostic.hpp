#pragma once

#include <expected>
#include <format>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/diag/source_span.hpp"

namespace lyra::diag {

namespace detail {

inline void RequireKind(DiagCode code, DiagKind expected) {
  if (DiagCodeKind(code) != expected) {
    throw InternalError(
        std::format(
            "Diagnostic factory: code '{}' is not of the expected kind",
            DiagCodeName(code)));
  }
}

inline void RequireCategory(
    DiagCode code, std::optional<UnsupportedCategory> expected) {
  if (DiagCodeCategory(code) != expected) {
    throw InternalError(
        std::format(
            "Diagnostic factory: code '{}' has a different category than the "
            "factory call site claims",
            DiagCodeName(code)));
  }
}

}  // namespace detail

struct UnknownSpan {
  auto operator==(const UnknownSpan&) const -> bool = default;
};

using DiagSpan = std::variant<SourceSpan, UnknownSpan>;

// A primary diagnostic body. Always has a stable DiagCode identity.
struct PrimaryDiagItem {
  DiagKind kind;
  DiagSpan span;
  DiagCode code;
  std::string message;
  std::optional<UnsupportedCategory> category;

  auto operator==(const PrimaryDiagItem&) const -> bool = default;
};

// A note attached to a primary diagnostic. Notes have no independent
// identity; they exist solely as supplementary text/location.
struct NoteDiagItem {
  DiagSpan span;
  std::string message;

  auto operator==(const NoteDiagItem&) const -> bool = default;
};

struct Diagnostic {
  PrimaryDiagItem primary;
  std::vector<NoteDiagItem> notes;

  auto operator==(const Diagnostic&) const -> bool = default;

  static auto Error(SourceSpan span, DiagCode code, std::string msg)
      -> Diagnostic {
    detail::RequireKind(code, DiagKind::kError);
    return Diagnostic{
        .primary =
            {.kind = DiagKind::kError,
             .span = span,
             .code = code,
             .message = std::move(msg),
             .category = std::nullopt},
        .notes = {},
    };
  }

  static auto Unsupported(
      SourceSpan span, DiagCode code, std::string msg, UnsupportedCategory cat)
      -> Diagnostic {
    detail::RequireKind(code, DiagKind::kUnsupported);
    detail::RequireCategory(code, cat);
    return Diagnostic{
        .primary =
            {.kind = DiagKind::kUnsupported,
             .span = span,
             .code = code,
             .message = std::move(msg),
             .category = cat},
        .notes = {},
    };
  }

  static auto Unsupported(
      DiagCode code, std::string msg, UnsupportedCategory cat) -> Diagnostic {
    detail::RequireKind(code, DiagKind::kUnsupported);
    detail::RequireCategory(code, cat);
    return Diagnostic{
        .primary =
            {.kind = DiagKind::kUnsupported,
             .span = UnknownSpan{},
             .code = code,
             .message = std::move(msg),
             .category = cat},
        .notes = {},
    };
  }

  static auto HostError(DiagCode code, std::string msg) -> Diagnostic {
    detail::RequireKind(code, DiagKind::kHostError);
    return Diagnostic{
        .primary =
            {.kind = DiagKind::kHostError,
             .span = UnknownSpan{},
             .code = code,
             .message = std::move(msg),
             .category = std::nullopt},
        .notes = {},
    };
  }

  static auto HostError(SourceSpan span, DiagCode code, std::string msg)
      -> Diagnostic {
    detail::RequireKind(code, DiagKind::kHostError);
    return Diagnostic{
        .primary =
            {.kind = DiagKind::kHostError,
             .span = span,
             .code = code,
             .message = std::move(msg),
             .category = std::nullopt},
        .notes = {},
    };
  }

  static auto Warning(SourceSpan span, DiagCode code, std::string msg)
      -> Diagnostic {
    detail::RequireKind(code, DiagKind::kWarning);
    return Diagnostic{
        .primary =
            {.kind = DiagKind::kWarning,
             .span = span,
             .code = code,
             .message = std::move(msg),
             .category = std::nullopt},
        .notes = {},
    };
  }

  auto WithNote(SourceSpan span, std::string msg) && -> Diagnostic {
    notes.push_back(NoteDiagItem{.span = span, .message = std::move(msg)});
    return std::move(*this);
  }

  auto WithNote(std::string msg) && -> Diagnostic {
    notes.push_back(
        NoteDiagItem{.span = UnknownSpan{}, .message = std::move(msg)});
    return std::move(*this);
  }
};

template <typename T>
using Result = std::expected<T, Diagnostic>;

// Construction helpers: build a std::unexpected<Diagnostic> directly so
// lowering call sites read `return diag::Unsupported(...)` instead of
// `return std::unexpected(diag::Diagnostic::Unsupported(...))`.
inline auto Unsupported(
    SourceSpan span, DiagCode code, std::string msg, UnsupportedCategory cat)
    -> std::unexpected<Diagnostic> {
  return std::unexpected(
      Diagnostic::Unsupported(span, code, std::move(msg), cat));
}

inline auto Unsupported(DiagCode code, std::string msg, UnsupportedCategory cat)
    -> std::unexpected<Diagnostic> {
  return std::unexpected(Diagnostic::Unsupported(code, std::move(msg), cat));
}

inline auto Error(SourceSpan span, DiagCode code, std::string msg)
    -> std::unexpected<Diagnostic> {
  return std::unexpected(Diagnostic::Error(span, code, std::move(msg)));
}

inline auto HostError(DiagCode code, std::string msg)
    -> std::unexpected<Diagnostic> {
  return std::unexpected(Diagnostic::HostError(code, std::move(msg)));
}

inline auto HostError(SourceSpan span, DiagCode code, std::string msg)
    -> std::unexpected<Diagnostic> {
  return std::unexpected(Diagnostic::HostError(span, code, std::move(msg)));
}

}  // namespace lyra::diag
