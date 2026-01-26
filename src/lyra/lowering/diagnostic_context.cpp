#include "lyra/lowering/diagnostic_context.hpp"

#include <string>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/common/origin_id.hpp"
#include "lyra/common/source_span.hpp"

namespace lyra::lowering {

auto DiagnosticContext::MakeUnsupported(
    common::OriginId origin, std::string msg, UnsupportedCategory cat) const
    -> Diagnostic {
  if (origin.IsValid()) {
    auto span = resolve_fn_(origin);
    if (span) {
      return Diagnostic::Unsupported(*span, std::move(msg), cat);
    }
  }

  // Missing origin is a compiler bug, but we recover for user UX.
  // The user still sees an error message, just without source location.
  // Note: We don't throw here because that would change control flow.
  // A future telemetry system could record this for investigation.
  return Diagnostic{
      .primary =
          {.kind = DiagKind::kUnsupported,
           .span = UnknownSpan{},
           .message = std::move(msg),
           .category = cat},
      .notes = {},
  };
}

auto DiagnosticContext::MakeError(
    common::OriginId origin, std::string msg) const -> Diagnostic {
  if (origin.IsValid()) {
    auto span = resolve_fn_(origin);
    if (span) {
      return Diagnostic::Error(*span, std::move(msg));
    }
  }

  // Same recovery logic as MakeUnsupported
  return Diagnostic{
      .primary =
          {.kind = DiagKind::kError,
           .span = UnknownSpan{},
           .message = std::move(msg),
           .category = std::nullopt},
      .notes = {},
  };
}

}  // namespace lyra::lowering
