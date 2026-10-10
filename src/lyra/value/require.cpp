#include "lyra/value/require.hpp"

#include <string>
#include <string_view>

#include "lyra/base/simulation_error.hpp"

namespace lyra::value {

void RequireCondition(bool holds, std::string_view message) {
  if (!holds) {
    throw SimulationError(std::string{message});
  }
}

}  // namespace lyra::value
