#include "lyra/diag/failure_context.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <exception>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
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
// names from the innermost outward, already joined as a reader sees them, as
// far as they fit. A trail is written while the exception unwinds, so writing
// it allocates nothing, which an exception thrown because memory ran out could
// not survive; and it is trivially destructible, so no thread-exit destructor
// is registered with the C library, a registration that ends the process
// outright where memory has run out. The names a trail keeps are the
// innermost, which place the failure.
struct Trail {
  std::optional<SourceSpan> place;
  std::array<char, 1024> names{};
  std::size_t names_length = 0;

  [[nodiscard]] auto Names() const -> std::string_view {
    return {names.data(), names_length};
  }
  template <typename... Args>
  void Append(std::format_string<Args...> format, Args&&... args) {
    const auto room = static_cast<std::ptrdiff_t>(names.size() - names_length);
    const auto written = std::format_to_n(
        names.begin() + static_cast<std::ptrdiff_t>(names_length), room, format,
        std::forward<Args>(args)...);
    names_length += static_cast<std::size_t>(std::min(written.size, room));
  }
};

struct ThreadWork {
  int in_progress = 0;
  Trail trail;
};
static_assert(std::is_trivially_destructible_v<ThreadWork>);

auto Work() -> ThreadWork& {
  constinit thread_local ThreadWork work;
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
            if (!trail.place.has_value() && place.at != SourceSpan{}) {
              trail.place = place.at;
            }
          },
          [&](const Named& named) {
            if (trail.names_length != 0) {
              trail.Append(", ");
            }
            trail.Append("{} '{}'", named.what, named.name);
          }},
      said_);
}

auto InternalFailure(const std::exception& failure) -> Diagnostic {
  // An exception of any other type was raised by a library, and is as much a
  // defect to report as an invariant the compiler checked itself.
  const auto* checked = dynamic_cast<const InternalError*>(&failure);
  const Trail trail = std::exchange(Work().trail, Trail{});
  const DiagSpan place = trail.place.has_value() ? DiagSpan{*trail.place}
                                                 : DiagSpan{UnknownSpan{}};
  const std::string names = trail.names_length == 0
                                ? std::string{}
                                : std::format(" ({})", trail.Names());
  if (checked != nullptr) {
    return Make(
        place, DiagCode::kInternalFailure, "{}{}", checked->Invariant(), names);
  }
  return Make(
      place, DiagCode::kInternalFailure, "an exception nothing handles ({}){}",
      failure.what(), names);
}

}  // namespace lyra::diag
