#include "lyra/diag/render.hpp"

#include <cstdint>
#include <format>
#include <string>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"

namespace lyra::diag {

namespace {

// What a message says, with what its kind adds that the word it opens with
// does not carry: that the failure is the compiler's own.
auto Said(DiagKind kind, const std::string& message) -> std::string {
  switch (kind) {
    case DiagKind::kInternalError:
      return "internal error: " + message;
    case DiagKind::kError:
    case DiagKind::kUnsupported:
    case DiagKind::kHostError:
    case DiagKind::kWarning:
    case DiagKind::kNote:
    case DiagKind::kRemark:
      return message;
  }
  throw InternalError("diag::Said: invalid DiagKind");
}

auto IsShown(DiagKind kind, const RenderOptions& opts) -> bool {
  switch (kind) {
    case DiagKind::kError:
    case DiagKind::kUnsupported:
    case DiagKind::kHostError:
    case DiagKind::kInternalError:
    case DiagKind::kNote:
      return true;
    case DiagKind::kWarning:
      return opts.show_warnings;
    case DiagKind::kRemark:
      return opts.show_remarks;
  }
  throw InternalError("diag::IsShown: invalid DiagKind");
}

// The place a message is about, where no place is the span that is none.
auto PlaceOf(const DiagSpan& diag_span) -> SourceSpan {
  return std::visit(
      Overloaded{
          [](const SourceSpan& span) { return span; },
          [](UnknownSpan) { return SourceSpan{}; },
      },
      diag_span);
}

}  // namespace

auto RenderDiagnostic(
    const Diagnostic& diag, const SourceManager& source_manager,
    const RenderOptions& opts) -> std::string {
  std::string out;
  if (!IsShown(diag.primary.kind, opts)) {
    return out;
  }
  out += source_manager.Show(
      diag.primary.kind, PlaceOf(diag.primary.span),
      Said(diag.primary.kind, diag.primary.message), opts.use_color);
  for (const auto& note : diag.notes) {
    out += source_manager.Show(
        DiagKind::kNote, PlaceOf(note.span), note.message, opts.use_color);
  }
  return out;
}

auto RenderDiagnostics(
    const DiagnosticSink& sink, const SourceManager& source_manager,
    const RenderOptions& opts) -> std::string {
  std::string out;
  std::uint32_t error_count = 0;
  std::uint32_t warning_count = 0;
  bool asks_for_report = false;
  for (const auto& d : sink.Diagnostics()) {
    if (!IsShown(d.primary.kind, opts)) {
      continue;
    }
    switch (d.primary.kind) {
      case DiagKind::kError:
      case DiagKind::kUnsupported:
      case DiagKind::kHostError:
        ++error_count;
        break;
      case DiagKind::kInternalError:
        ++error_count;
        asks_for_report = true;
        break;
      case DiagKind::kWarning:
        ++warning_count;
        break;
      case DiagKind::kNote:
      case DiagKind::kRemark:
        break;
    }
    out += RenderDiagnostic(d, source_manager, opts);
  }
  if (error_count == 0 && warning_count == 0) {
    return out;
  }
  std::string summary;
  if (warning_count > 0) {
    summary += std::format(
        "{} warning{}", warning_count, warning_count == 1 ? "" : "s");
  }
  if (warning_count > 0 && error_count > 0) {
    summary += " and ";
  }
  if (error_count > 0) {
    summary +=
        std::format("{} error{}", error_count, error_count == 1 ? "" : "s");
  }
  out += std::format("{} generated.\n", summary);
  // Asked once for the run, however many of its failures were the compiler's
  // own.
  if (asks_for_report) {
    out += std::format("{}\n", BugReportRequest());
  }
  return out;
}

}  // namespace lyra::diag
