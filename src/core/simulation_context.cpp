#include "core/simulation_context.hpp"

namespace volans {

void SimulationContext::AdvanceTime() {
  ++currentTime_;
}

auto SimulationContext::GetCurrentTime() const -> SimulationTime {
  return currentTime_;
}

}  // namespace volans
