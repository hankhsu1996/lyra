#include "lyra/value/require.hpp"

#include <string>
#include <string_view>

#include "lyra/base/simulation_error.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

void RequireCondition(const PackedArray& condition, std::string_view message) {
  if (condition.HasUnknown() || condition.ToInt64() == 0) {
    throw SimulationError(std::string{message});
  }
}

}  // namespace lyra::value
