#include "lyra/runtime/diagnostic.hpp"

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

void DiagnosticDispatcher::EmitInfo(
    const lyra::value::String& origin, const lyra::value::String& text) {
  Emit(
      DiagnosticRecord{
          .severity = Severity::kInfo,
          .origin = std::string{origin.View()},
          .body = std::string{text.View()}});
}

void DiagnosticDispatcher::EmitWarning(
    const lyra::value::String& origin, const lyra::value::String& text) {
  Emit(
      DiagnosticRecord{
          .severity = Severity::kWarning,
          .origin = std::string{origin.View()},
          .body = std::string{text.View()}});
}

void DiagnosticDispatcher::EmitError(
    const lyra::value::String& origin, const lyra::value::String& text) {
  Emit(
      DiagnosticRecord{
          .severity = Severity::kError,
          .origin = std::string{origin.View()},
          .body = std::string{text.View()}});
}

void DiagnosticDispatcher::Emit(DiagnosticRecord record) {
  if (rate_limit_ > 0) {
    const CountKey key{.origin = record.origin, .severity = record.severity};
    auto& count = emit_counts_[key];
    if (count >= rate_limit_) {
      if (count == rate_limit_) {
        ++count;
        const std::string note = std::format(
            "lyra: {}: further messages from this site suppressed after {} "
            "occurrences\n",
            SeverityText(record.severity), rate_limit_);
        sink_(note);
      } else {
        ++count;
      }
      return;
    }
    ++count;
  }

  std::string line;
  if (!record.origin.empty()) {
    line += record.origin;
    line += ": ";
  }
  line += SeverityText(record.severity);
  line += ": ";
  line += record.body;
  line += "\n";
  sink_(line);
}

}  // namespace lyra::runtime
