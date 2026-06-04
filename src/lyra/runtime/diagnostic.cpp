#include "lyra/runtime/diagnostic.hpp"

#include <cstdint>
#include <format>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/value/format.hpp"

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

auto FormatOrigin(const SourceLocation& loc) -> std::string {
  return std::format("{}:{}:{}", loc.file, loc.line, loc.col);
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

void DiagnosticDispatcher::Emit(DiagnosticRecord record) {
  const std::string origin_text =
      record.origin.has_value() ? FormatOrigin(*record.origin) : std::string{};

  if (rate_limit_ > 0) {
    const CountKey key{.origin = origin_text, .severity = record.severity};
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
  if (!origin_text.empty()) {
    line += origin_text;
    line += ": ";
  }
  line += SeverityText(record.severity);
  line += ": ";
  line += record.body;
  line += "\n";
  sink_(line);
}

void LyraDiagnostic(
    RuntimeServices& services, Severity severity,
    std::optional<SourceLocation> origin,
    std::span<const value::PrintItem> items) {
  std::string body;
  for (const value::PrintItem& item : items) {
    std::visit(
        Overloaded{
            [&](const value::PrintLiteralItem& lit) {
              body.append(std::string_view{lit.data, lit.size});
            },
            [&](const value::PrintValueItem& v) {
              if (v.spec.kind == value::FormatKind::kTime) {
                body.append(
                    value::FormatTime(v.spec, v.value, services.TimeFormat()));
                return;
              }
              body.append(value::FormatValue(v.spec, v.value));
            },
        },
        item);
  }
  services.Diagnostic().Emit(
      DiagnosticRecord{
          .severity = severity,
          .origin = std::move(origin),
          .body = std::move(body)});
}

}  // namespace lyra::runtime
