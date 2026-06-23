#pragma once

#include <expected>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/diag/source_span.hpp"

namespace lyra::diag {

struct UnknownSpan {
  auto operator==(const UnknownSpan&) const -> bool = default;
};

using DiagSpan = std::variant<SourceSpan, UnknownSpan>;

// A primary diagnostic body. Always has a stable DiagCode identity. Its kind is
// a property of the code, read from the code table at construction; no caller
// supplies or restates it.
struct PrimaryDiagItem {
  DiagKind kind;
  DiagSpan span;
  DiagCode code;
  std::string message;

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

// Builds a Diagnostic for the report-and-continue path (a warning, or a
// diagnostic handed straight to a sink). The kind comes from the code.
inline auto Make(DiagSpan span, DiagCode code, std::string msg) -> Diagnostic {
  return Diagnostic{
      .primary =
          {.kind = DiagCodeKind(code),
           .span = span,
           .code = code,
           .message = std::move(msg)},
      .notes = {},
  };
}

inline auto Make(DiagCode code, std::string msg) -> Diagnostic {
  return Make(UnknownSpan{}, code, std::move(msg));
}

// Builds a std::unexpected<Diagnostic> for the recoverable-failure path, so a
// lowering call site reads `return diag::Fail(...)` instead of
// `return std::unexpected(diag::Make(...))`.
inline auto Fail(DiagSpan span, DiagCode code, std::string msg)
    -> std::unexpected<Diagnostic> {
  return std::unexpected(Make(span, code, std::move(msg)));
}

inline auto Fail(DiagCode code, std::string msg)
    -> std::unexpected<Diagnostic> {
  return std::unexpected(Make(UnknownSpan{}, code, std::move(msg)));
}

}  // namespace lyra::diag
