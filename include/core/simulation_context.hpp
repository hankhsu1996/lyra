#pragma once

#include <cstdint>

namespace volans {

using SimulationTime = uint64_t;

class SimulationContext {
public:
  void AdvanceTime();
  [[nodiscard]] auto GetCurrentTime() const -> SimulationTime;

private:
  SimulationTime currentTime_ = 0;
};

} // namespace volans
