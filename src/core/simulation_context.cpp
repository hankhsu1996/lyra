#include "core/simulation_context.hpp"

namespace lyra {

void SimulationContext::AdvanceTime() {
  ++currentTime_;
}

auto SimulationContext::GetCurrentTime() const -> SimulationTime {
  return currentTime_;
}

}  // namespace lyra
