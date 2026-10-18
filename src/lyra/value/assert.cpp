#include "lyra/value/assert.hpp"

#include <string_view>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/simulation_error.hpp"

namespace lyra::value {

void Assert(bool holds, std::string_view message) {
  if (!holds) {
    throw SimulationError(base::DiagMessage("{}", message));
  }
}

}  // namespace lyra::value
