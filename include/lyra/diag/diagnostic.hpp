#pragma once

#include <expected>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/overloaded.hpp"
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

  auto WithNote(SourceSpan span, base::DiagMessage msg) && -> Diagnostic {
    notes.push_back(
        NoteDiagItem{.span = span, .message = std::move(msg).TakeText()});
    return std::move(*this);
  }

  auto WithNote(base::DiagMessage msg) && -> Diagnostic {
    notes.push_back(
        NoteDiagItem{
            .span = UnknownSpan{}, .message = std::move(msg).TakeText()});
    return std::move(*this);
  }

  template <typename... Args>
  auto WithNote(
      SourceSpan span, base::DiagFormatFor<Args...> format,
      Args&&... args) && -> Diagnostic {
    return std::move(*this).WithNote(
        span, base::DiagMessage(format, std::forward<Args>(args)...));
  }

  template <typename... Args>
  auto WithNote(
      base::DiagFormatFor<Args...> format, Args&&... args) && -> Diagnostic {
    return std::move(*this).WithNote(
        base::DiagMessage(format, std::forward<Args>(args)...));
  }
};

template <typename T>
using Result = std::expected<T, Diagnostic>;

// `diagnostic`, standing at `span` where it names no place of its own: the
// construct whose lowering or translation it was raised in, which a site deeper
// in that work does not hold.
inline auto PlacedAt(Diagnostic diagnostic, SourceSpan span) -> Diagnostic {
  const bool placed = std::visit(
      Overloaded{
          [](const SourceSpan& at) { return at != SourceSpan{}; },
          [](const UnknownSpan&) { return false; }},
      diagnostic.primary.span);
  if (!placed) {
    diagnostic.primary.span = span;
  }
  return diagnostic;
}

// The same for what a piece of that work answered, where it failed.
template <typename T>
auto PlacedAt(Result<T> result, SourceSpan span) -> Result<T> {
  if (!result) {
    return std::unexpected(PlacedAt(std::move(result.error()), span));
  }
  return result;
}

// Builds a Diagnostic for the report-and-continue path (a warning, or a
// diagnostic handed straight to a sink). The kind comes from the code.
inline auto Make(DiagSpan span, DiagCode code, base::DiagMessage msg)
    -> Diagnostic {
  return Diagnostic{
      .primary =
          {.kind = DiagCodeKind(code),
           .span = span,
           .code = code,
           .message = std::move(msg).TakeText()},
      .notes = {},
  };
}

inline auto Make(DiagCode code, base::DiagMessage msg) -> Diagnostic {
  return Make(UnknownSpan{}, code, std::move(msg));
}

template <typename... Args>
auto Make(
    DiagSpan span, DiagCode code, base::DiagFormatFor<Args...> format,
    Args&&... args) -> Diagnostic {
  return Make(
      span, code, base::DiagMessage(format, std::forward<Args>(args)...));
}

template <typename... Args>
auto Make(DiagCode code, base::DiagFormatFor<Args...> format, Args&&... args)
    -> Diagnostic {
  return Make(
      UnknownSpan{}, code,
      base::DiagMessage(format, std::forward<Args>(args)...));
}

// Builds a std::unexpected<Diagnostic> for the recoverable-failure path, so a
// lowering call site reads `return diag::Fail(...)` instead of
// `return std::unexpected(diag::Make(...))`.
inline auto Fail(DiagSpan span, DiagCode code, base::DiagMessage msg)
    -> std::unexpected<Diagnostic> {
  return std::unexpected(Make(span, code, std::move(msg)));
}

inline auto Fail(DiagCode code, base::DiagMessage msg)
    -> std::unexpected<Diagnostic> {
  return std::unexpected(Make(UnknownSpan{}, code, std::move(msg)));
}

template <typename... Args>
auto Fail(
    DiagSpan span, DiagCode code, base::DiagFormatFor<Args...> format,
    Args&&... args) -> std::unexpected<Diagnostic> {
  return std::unexpected(
      Make(span, code, base::DiagMessage(format, std::forward<Args>(args)...)));
}

template <typename... Args>
auto Fail(DiagCode code, base::DiagFormatFor<Args...> format, Args&&... args)
    -> std::unexpected<Diagnostic> {
  return std::unexpected(Make(
      UnknownSpan{}, code,
      base::DiagMessage(format, std::forward<Args>(args)...)));
}

}  // namespace lyra::diag
