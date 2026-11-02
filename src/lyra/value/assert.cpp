#include "lyra/value/assert.hpp"

#include <string_view>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/base/source_location.hpp"

namespace lyra::value {

void Assert(bool holds, std::string_view message, base::SourceLocation at) {
  if (!holds) {
    const base::SourceLocationScope bound(at);
    throw SimulationError(base::DiagMessage("{}", message));
  }
}

}  // namespace lyra::value
