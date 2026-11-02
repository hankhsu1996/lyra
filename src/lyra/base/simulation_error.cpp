#include "lyra/base/simulation_error.hpp"

#include <format>
#include <string_view>
#include <utility>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/source_location.hpp"

namespace lyra {

namespace base {

auto BoundSourceLocation(std::string_view what) -> SourceLocation {
  const SourceLocation at = CurrentSourceLocation();
  if (!at.IsValid()) {
    throw InternalError(
        std::format(
            "a failure of the design is raised under no operation of the "
            "design, so nothing bound where it was written: {}",
            what));
  }
  return at;
}

}  // namespace base

SimulationError::SimulationError(base::DiagMessage message)
    : std::runtime_error(std::move(message).TakeText()),
      at_(base::BoundSourceLocation(what())) {
}

SimulationError::SimulationError(
    base::NoSourceLocation, base::DiagMessage message)
    : std::runtime_error(std::move(message).TakeText()) {
}

SimulationError::~SimulationError() = default;

}  // namespace lyra
