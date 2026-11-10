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
    const lyra::value::String& text, const lyra::value::String& scope,
    base::SourceLocation at) {
  EmitInScope(Severity::kInfo, text.View(), scope.View(), at);
}

void DiagnosticDispatcher::EmitWarning(
    const lyra::value::String& text, const lyra::value::String& scope,
    base::SourceLocation at) {
  EmitInScope(Severity::kWarning, text.View(), scope.View(), at);
}

void DiagnosticDispatcher::EmitError(
    const lyra::value::String& text, const lyra::value::String& scope,
    base::SourceLocation at) {
  EmitInScope(Severity::kError, text.View(), scope.View(), at);
}

void DiagnosticDispatcher::EmitFatal(
    const lyra::value::String& text, const lyra::value::String& scope,
    base::SourceLocation at) {
  EmitInScope(Severity::kFatal, text.View(), scope.View(), at);
}

void DiagnosticDispatcher::EmitInfoAtCaller(
    const lyra::value::String& text, base::SourceLocation at) {
  EmitUnderProcedure(
      Severity::kInfo, SeverityText(Severity::kInfo), at, text.View(), {});
}

void DiagnosticDispatcher::EmitWarningAtCaller(
    const lyra::value::String& text, base::SourceLocation at) {
  EmitUnderProcedure(
      Severity::kWarning, SeverityText(Severity::kWarning), at, text.View(),
      {});
}

void DiagnosticDispatcher::EmitErrorAtCaller(
    const lyra::value::String& text, base::SourceLocation at) {
  EmitUnderProcedure(
      Severity::kError, SeverityText(Severity::kError), at, text.View(), {});
}

void DiagnosticDispatcher::EmitFatalAtCaller(
    const lyra::value::String& text, base::SourceLocation at) {
  EmitUnderProcedure(
      Severity::kFatal, SeverityText(Severity::kFatal), at, text.View(), {});
}

void DiagnosticDispatcher::Report(
    Severity severity, const base::DiagMessage& message,
    std::span<const base::DiagMessage> notes) {
  EmitUnderProcedure(
      severity, ToolSeverityText(severity),
      base::BoundSourceLocation(message.Text()), message.Text(), notes);
}

void DiagnosticDispatcher::Report(
    base::NoSourceLocation no_location, Severity severity,
    const base::DiagMessage& message,
    std::span<const base::DiagMessage> notes) {
  EmitUnderProcedure(
      severity, ToolSeverityText(severity),
      static_cast<base::SourceLocation>(no_location), message.Text(), notes);
}

void DiagnosticDispatcher::ReportAt(
    base::SourceLocation at, Severity severity,
    const base::DiagMessage& message) {
  EmitUnderProcedure(
      severity, ToolSeverityText(severity), at, message.Text(), {});
}

void DiagnosticDispatcher::Line(std::string_view line) {
  Write(std::format("{}\n", line));
}

auto DiagnosticDispatcher::ReportedFatal() const -> bool {
  return reported_fatal_;
}

// The report the design's code makes names the scope that code is written in,
// which is the call's to state (LRM 20.10), and the time, which is the run's.
void DiagnosticDispatcher::EmitInScope(
    Severity severity, std::string_view body, std::string_view scope,
    base::SourceLocation at) {
  Emit(
      severity, SeverityText(severity), at, body, scope,
      context_ ? context_().time : 0, {});
}

// A report of the tool's own is placed where the operation it is about was
// written. One about no place -- the tool failed the design under no statement
// of its own -- is placed where the procedure it is made under is written.
void DiagnosticDispatcher::EmitUnderProcedure(
    Severity severity, std::string_view word, base::SourceLocation at,
    std::string_view body, std::span<const base::DiagMessage> notes) {
  const Context context = context_ ? context_() : Context{};
  Emit(
      severity, word, at.IsValid() ? at : context.written_at, body,
      context.scope, context.time, notes);
}

void DiagnosticDispatcher::Emit(
    Severity severity, std::string_view word, base::SourceLocation at,
    std::string_view body, std::optional<std::string_view> scope, SimTime time,
    std::span<const base::DiagMessage> notes) {
  if (severity == Severity::kFatal) {
    reported_fatal_ = true;
  }
  const std::string place = SourceLocationText(at);

  // Which scope and what time are context, so they are a note of their own.
  std::string text;
  if (!place.empty()) {
    text += std::format("{}: ", place);
  }
  text += std::format("{}: {}\n", word, body);
  if (context_) {
    text += scope ? std::format("note: in '{}' at time {}\n", *scope, time)
                  : std::format("note: at time {}\n", time);
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
