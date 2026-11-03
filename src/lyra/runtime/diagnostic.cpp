#include "lyra/runtime/diagnostic.hpp"

#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

// The word a severity task's own line opens with: the severity it names (LRM
// 20.10).
auto SeverityText(Severity s) -> std::string_view {
  switch (s) {
    case Severity::kInfo:
      return "info";
    case Severity::kWarning:
      return "warning";
    case Severity::kError:
      return "error";
    case Severity::kFatal:
      return "fatal";
  }
  return "info";
}

// The word a report of the tool's own opens with, one of the three every
// compiler's message opens with. A fatal one ends the run as `$fatal` does, and
// says so as an error.
auto ToolSeverityText(Severity s) -> std::string_view {
  switch (s) {
    case Severity::kInfo:
      return "note";
    case Severity::kWarning:
      return "warning";
    case Severity::kError:
    case Severity::kFatal:
      return "error";
  }
  return "note";
}

}  // namespace

DiagnosticDispatcher::DiagnosticDispatcher(FileTable& files) : files_(&files) {
}

void DiagnosticDispatcher::Write(std::string_view text) {
  files_->WriteToStandardError(text);
}

void DiagnosticDispatcher::SetContextSource(ContextSource context) {
  context_ = std::move(context);
}

void DiagnosticDispatcher::EmitInfo(
    const lyra::value::String& text, base::SourceLocation at) {
  Emit(Severity::kInfo, SeverityText(Severity::kInfo), at, text.View());
}

void DiagnosticDispatcher::EmitWarning(
    const lyra::value::String& text, base::SourceLocation at) {
  Emit(Severity::kWarning, SeverityText(Severity::kWarning), at, text.View());
}

void DiagnosticDispatcher::EmitError(
    const lyra::value::String& text, base::SourceLocation at) {
  Emit(Severity::kError, SeverityText(Severity::kError), at, text.View());
}

void DiagnosticDispatcher::EmitFatal(
    const lyra::value::String& text, base::SourceLocation at) {
  Emit(Severity::kFatal, SeverityText(Severity::kFatal), at, text.View());
}

void DiagnosticDispatcher::Report(
    Severity severity, const base::DiagMessage& message,
    std::span<const base::DiagMessage> notes) {
  Emit(
      severity, ToolSeverityText(severity),
      base::BoundSourceLocation(message.Text()), message.Text(), notes);
}

void DiagnosticDispatcher::Report(
    base::NoSourceLocation no_location, Severity severity,
    const base::DiagMessage& message,
    std::span<const base::DiagMessage> notes) {
  Emit(
      severity, ToolSeverityText(severity),
      static_cast<base::SourceLocation>(no_location), message.Text(), notes);
}

void DiagnosticDispatcher::ReportAt(
    base::SourceLocation at, Severity severity,
    const base::DiagMessage& message) {
  Emit(severity, ToolSeverityText(severity), at, message.Text());
}

void DiagnosticDispatcher::Line(std::string_view line) {
  Write(std::format("{}\n", line));
}

auto DiagnosticDispatcher::ReportedFatal() const -> bool {
  return reported_fatal_;
}

void DiagnosticDispatcher::Emit(
    Severity severity, std::string_view word, base::SourceLocation at,
    std::string_view body, std::span<const base::DiagMessage> notes) {
  if (severity == Severity::kFatal) {
    reported_fatal_ = true;
  }
  // A report is placed where the operation it is about was written. One about
  // no place -- the tool failed the design under no statement of its own -- is
  // placed where the procedure it is made under is written; which instance
  // and what time are context, so they are a note of their own.
  const std::optional<Context> context =
      context_ ? std::optional{context_()} : std::nullopt;
  const std::string place =
      SourceLocationText(at.IsValid() || !context ? at : context->written_at);

  std::string text;
  if (!place.empty()) {
    text += std::format("{}: ", place);
  }
  text += std::format("{}: {}\n", word, body);
  if (context) {
    text += std::format("note: {}\n", context->where);
  }
  for (const base::DiagMessage& note : notes) {
    text += std::format("note: {}\n", note.Text());
  }
  Write(text);
}

auto SourceLocationText(base::SourceLocation at) -> std::string {
  if (!at.IsValid()) {
    return {};
  }
  return std::format("{}:{}:{}", at.File(), at.Line(), at.Column());
}

}  // namespace lyra::runtime
