#include "lyra/runtime/engine.hpp"

namespace lyra::runtime {

void Engine::ScheduleInitial(ProcessHandle handle) {
  active_queue_.push_back(
      ScheduledEvent{
          .handle = handle,
          .resume = ResumePoint{.block_index = 0, .instruction_index = 0},
      });
}

void Engine::Delay(ProcessHandle handle, ResumePoint resume, SimTime ticks) {
  SimTime wake_time = current_time_ + ticks;
  delay_queue_[wake_time].push_back(
      ScheduledEvent{
          .handle = handle,
          .resume = resume,
      });
}

void Engine::DelayZero(ProcessHandle handle, ResumePoint resume) {
  inactive_queue_.push(
      ScheduledEvent{
          .handle = handle,
          .resume = resume,
      });
}

void Engine::Subscribe(
    ProcessHandle handle, ResumePoint resume, SignalId signal, EdgeKind edge) {
  waiters_[signal].push_back(
      Waiter{
          .handle = handle,
          .resume = resume,
          .edge = edge,
      });
}

void Engine::NotifyChange(SignalId signal, bool old_lsb, bool new_lsb) {
  auto it = waiters_.find(signal);
  if (it == waiters_.end()) {
    return;
  }

  // Check each waiter's edge condition
  std::vector<Waiter> remaining;
  for (const auto& waiter : it->second) {
    bool triggered = false;
    switch (waiter.edge) {
      case EdgeKind::kPosedge:
        triggered = !old_lsb && new_lsb;  // 0 → 1
        break;
      case EdgeKind::kNegedge:
        triggered = old_lsb && !new_lsb;  // 1 → 0
        break;
      case EdgeKind::kAnyChange:
        triggered = old_lsb != new_lsb;
        break;
    }

    if (triggered) {
      // Move to active queue
      active_queue_.push_back(
          ScheduledEvent{
              .handle = waiter.handle,
              .resume = waiter.resume,
          });
    } else {
      // Keep waiting
      remaining.push_back(waiter);
    }
  }

  if (remaining.empty()) {
    waiters_.erase(it);
  } else {
    it->second = std::move(remaining);
  }
}

void Engine::ExecuteRegion(Region region) {
  switch (region) {
    case Region::kActive: {
      // Execute all active events (may add to inactive/nba queues)
      while (!active_queue_.empty()) {
        auto events = std::move(active_queue_);
        active_queue_.clear();
        for (const auto& event : events) {
          // Run process until it suspends
          runner_(*this, event.handle, event.resume);
        }
      }
      break;
    }
    case Region::kInactive: {
      // Move all inactive events to active queue
      while (!inactive_queue_.empty()) {
        active_queue_.push_back(inactive_queue_.front());
        inactive_queue_.pop();
      }
      break;
    }
    case Region::kNBA: {
      // TODO(hankhsu): NBA updates will be handled here
      // For now, nothing to do
      break;
    }
  }
}

void Engine::ExecuteTimeSlot() {
  // IEEE 1800 stratified event scheduler:
  // Loop until Active + Inactive + NBA are all empty
  while (!active_queue_.empty() || !inactive_queue_.empty()) {
    // Active region: execute all ready processes
    ExecuteRegion(Region::kActive);

    // Inactive region: move #0 delays to active
    if (!inactive_queue_.empty()) {
      ExecuteRegion(Region::kInactive);
      continue;  // Re-execute active region
    }

    // NBA region: commit nonblocking updates
    ExecuteRegion(Region::kNBA);
  }
}

auto Engine::Run(SimTime max_time) -> SimTime {
  while (current_time_ <= max_time) {
    // Execute current time slot
    ExecuteTimeSlot();

    // Advance to next scheduled time
    if (delay_queue_.empty()) {
      break;  // No more events
    }

    auto it = delay_queue_.begin();
    current_time_ = it->first;

    if (current_time_ > max_time) {
      break;
    }

    // Move scheduled events to active queue
    for (auto& event : it->second) {
      active_queue_.push_back(std::move(event));
    }
    delay_queue_.erase(it);
  }

  return current_time_;
}

}  // namespace lyra::runtime
