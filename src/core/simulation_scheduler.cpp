#include "core/simulation_scheduler.hpp"

namespace volans {

void SimulationScheduler::ScheduleAt(
    SimulationTime time, ScheduledProcess proc) {
  schedule_[time].push(std::move(proc));
}

void SimulationScheduler::ScheduleImmediately(ScheduledProcess proc) {
  // We consider current time to be 0 for immediate scheduling
  schedule_[0].push(std::move(proc));
}

void SimulationScheduler::RunScheduled() {
  if (schedule_.empty()) {
    return;  // Nothing to execute
  }

  // Execute all processes for the earliest time
  auto earliest_time_it = schedule_.begin();
  auto &process_queue = earliest_time_it->second;

  while (!process_queue.empty()) {
    auto proc = process_queue.front();
    process_queue.pop();
    proc();  // Execute the process
  }

  // Remove the executed time slot if empty
  if (process_queue.empty()) {
    schedule_.erase(earliest_time_it);
  }
}

}  // namespace volans
