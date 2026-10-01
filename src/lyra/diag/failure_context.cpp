#include "lyra/diag/failure_context.hpp"

#include <exception>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"

namespace lyra::diag {

namespace {

// What an exception left behind on its way out: the innermost place, and the
// names from the innermost outward, already joined as a reader sees them.
struct Trail {
  std::optional<SourceSpan> place;
  std::string names;
};

struct ThreadWork {
  int in_progress = 0;
  Trail trail;
};

auto Work() -> ThreadWork& {
  thread_local ThreadWork work;
  return work;
}

}  // namespace

// Work that starts with nothing around it and nothing being thrown is a new
// piece of work, so what an earlier exception left behind is not about it. That
// happens when something caught the exception and went on without asking what
// it had passed through.
FailureContext::FailureContext(Said said)
    : said_(said), exceptions_in_flight_(std::uncaught_exceptions()) {
  ThreadWork& work = Work();
  if (work.in_progress == 0 && exceptions_in_flight_ == 0) {
    work.trail = Trail{};
  }
  ++work.in_progress;
}

FailureContext::FailureContext(SourceSpan place)
    : FailureContext(Said{Place{.at = place}}) {
}

FailureContext::FailureContext(std::string_view what, std::string_view name)
    : FailureContext(Said{Named{.what = what, .name = name}}) {
}

auto FailureContext::InUnit(std::string_view name) -> FailureContext {
  return {"in unit", name};
}

auto FailureContext::InFunction(std::string_view name) -> FailureContext {
  return {"in function", name};
}

FailureContext::~FailureContext() {
  ThreadWork& work = Work();
  --work.in_progress;
  if (std::uncaught_exceptions() <= exceptions_in_flight_) {
    return;
  }
  Trail& trail = work.trail;
  std::visit(
      Overloaded{
          [&](const Place& place) {
            if (!trail.place.has_value()) {
              trail.place = place.at;
            }
          },
          [&](const Named& named) {
            if (!trail.names.empty()) {
              trail.names += ", ";
            }
            trail.names += std::format("{} '{}'", named.what, named.name);
          }},
      said_);
}

auto InternalFailure(const std::exception& failure) -> Diagnostic {
  // An exception of any other type was raised by a library, and is as much a
  // defect to report as an invariant the compiler checked itself.
  const auto* checked = dynamic_cast<const InternalError*>(&failure);
  const Trail trail = std::exchange(Work().trail, Trail{});
  std::string message =
      checked != nullptr
          ? std::string(checked->Invariant())
          : std::format("an exception nothing handles: {}", failure.what());
  if (!trail.names.empty()) {
    message += std::format(" ({})", trail.names);
  }
  return Make(
      trail.place.has_value() ? DiagSpan{*trail.place}
                              : DiagSpan{UnknownSpan{}},
      DiagCode::kInternalFailure, std::move(message));
}

}  // namespace lyra::diag
