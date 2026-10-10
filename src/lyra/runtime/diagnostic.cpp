#include "lyra/runtime/diagnostic.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

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

}  // namespace

auto DiagnosticDispatcher::CountKeyHash::operator()(
    const CountKey& k) const noexcept -> std::size_t {
  const std::size_t origin_hash = std::hash<std::string>{}(k.origin);
  const std::size_t severity_hash =
      std::hash<std::uint8_t>{}(static_cast<std::uint8_t>(k.severity));
  return origin_hash ^ (severity_hash << 1U);
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
  Emit(Severity::kInfo, origin.View(), text.View());
}

void DiagnosticDispatcher::EmitWarning(
    const lyra::value::String& origin, const lyra::value::String& text) {
  Emit(Severity::kWarning, origin.View(), text.View());
}

void DiagnosticDispatcher::EmitError(
    const lyra::value::String& origin, const lyra::value::String& text) {
  Emit(Severity::kError, origin.View(), text.View());
}

void DiagnosticDispatcher::EmitFatal(
    const lyra::value::String& origin, const lyra::value::String& text) {
  Emit(Severity::kFatal, origin.View(), text.View());
}

void DiagnosticDispatcher::Report(Severity severity, std::string_view body) {
  Emit(severity, {}, body);
}

void DiagnosticDispatcher::Note(std::string_view text) {
  sink_(std::format("{}\n", text));
}

auto DiagnosticDispatcher::ReportedFatal() const -> bool {
  return reported_fatal_;
}

void DiagnosticDispatcher::Emit(
    Severity severity, std::string_view origin, std::string_view body) {
  if (severity == Severity::kFatal) {
    reported_fatal_ = true;
  }
  if (rate_limit_ > 0) {
    const CountKey key{.origin = std::string{origin}, .severity = severity};
    auto& count = emit_counts_[key];
    if (count >= rate_limit_) {
      if (count == rate_limit_) {
        ++count;
        sink_(
            std::format(
                "lyra: {}: further messages from this site suppressed after {} "
                "occurrences\n",
                SeverityText(severity), rate_limit_));
      } else {
        ++count;
      }
      return;
    }
    ++count;
  }

  std::string line;
  if (!origin.empty()) {
    line += origin;
    line += ": ";
  }
  line += SeverityText(severity);
  line += ": ";
  line += body;
  if (context_) {
    const Context context = context_();
    line += " (";
    line += context.scope_and_time;
    // A report with a call site of its own already says where it was made.
    if (origin.empty() && !context.procedure.empty()) {
      line += ", in the procedure at ";
      line += context.procedure;
    }
    line += ")";
  }
  line += "\n";
  sink_(line);
}

}  // namespace lyra::runtime
