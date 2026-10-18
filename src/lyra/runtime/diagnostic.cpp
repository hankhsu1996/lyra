#include "lyra/runtime/diagnostic.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <optional>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/base/diag_message.hpp"
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

auto DiagnosticDispatcher::CountKeyHash::operator()(
    const CountKey& k) const noexcept -> std::size_t {
  const std::size_t place_hash = std::hash<std::string>{}(k.place);
  const std::size_t severity_hash =
      std::hash<std::uint8_t>{}(static_cast<std::uint8_t>(k.severity));
  return place_hash ^ (severity_hash << 1U);
}

DiagnosticDispatcher::DiagnosticDispatcher(
    DiagnosticSink sink, std::uint32_t rate_limit)
    : sink_(std::move(sink)), rate_limit_(rate_limit) {
}

void DiagnosticDispatcher::SetContextSource(ContextSource context) {
  context_ = std::move(context);
}

void DiagnosticDispatcher::EmitInfo(
    const lyra::value::String& origin, const lyra::value::String& text) {
  Emit(
      Severity::kInfo, SeverityText(Severity::kInfo), origin.View(),
      text.View());
}

void DiagnosticDispatcher::EmitWarning(
    const lyra::value::String& origin, const lyra::value::String& text) {
  Emit(
      Severity::kWarning, SeverityText(Severity::kWarning), origin.View(),
      text.View());
}

void DiagnosticDispatcher::EmitError(
    const lyra::value::String& origin, const lyra::value::String& text) {
  Emit(
      Severity::kError, SeverityText(Severity::kError), origin.View(),
      text.View());
}

void DiagnosticDispatcher::EmitFatal(
    const lyra::value::String& origin, const lyra::value::String& text) {
  Emit(
      Severity::kFatal, SeverityText(Severity::kFatal), origin.View(),
      text.View());
}

void DiagnosticDispatcher::Report(
    Severity severity, const base::DiagMessage& message) {
  Emit(severity, ToolSeverityText(severity), {}, message.Text());
}

void DiagnosticDispatcher::Line(std::string_view line) {
  sink_(std::format("{}\n", line));
}

auto DiagnosticDispatcher::ReportedFatal() const -> bool {
  return reported_fatal_;
}

void DiagnosticDispatcher::Emit(
    Severity severity, std::string_view word, std::string_view origin,
    std::string_view body) {
  if (severity == Severity::kFatal) {
    reported_fatal_ = true;
  }
  // A report with no call site of its own is placed where the procedure it is
  // made under is written; which instance and what time are context, so they
  // are a note of their own. The place is also what a repeated report is
  // counted by.
  const std::optional<Context> context =
      context_ ? std::optional{context_()} : std::nullopt;
  const std::string_view place =
      !origin.empty() || !context ? origin : context->procedure;
  if (rate_limit_ > 0) {
    const CountKey key{.place = std::string{place}, .severity = severity};
    auto& count = emit_counts_[key];
    if (count >= rate_limit_) {
      if (count == rate_limit_) {
        ++count;
        sink_(
            std::format(
                "{}{}note: further {} reports from here are not shown after "
                "{} of them\n",
                place, place.empty() ? "" : ": ", word, rate_limit_));
      } else {
        ++count;
      }
      return;
    }
    ++count;
  }

  std::string text;
  if (!place.empty()) {
    text += std::format("{}: ", place);
  }
  text += std::format("{}: {}\n", word, body);
  if (context) {
    text += std::format("note: {}\n", context->where);
  }
  sink_(text);
}

}  // namespace lyra::runtime
