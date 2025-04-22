#pragma once

#include "simulation_context.hpp"
#include <functional>
#include <map>
#include <queue>

namespace volans {

class SimulationScheduler {
public:
  using ScheduledProcess = std::function<void()>;

  void ScheduleAt(SimulationTime time, ScheduledProcess proc);
  void ScheduleImmediately(ScheduledProcess proc);

  void RunScheduled();

private:
  std::map<SimulationTime, std::queue<ScheduledProcess>> schedule_;
};

} // namespace volans
