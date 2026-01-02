#pragma once

#include <coroutine>
#include <cstdint>
#include <map>
#include <vector>

#include "lyra/sdk/delay.hpp"
#include "lyra/sdk/module.hpp"
#include "lyra/sdk/task.hpp"

namespace lyra::sdk {

class Scheduler {
 public:
  void RegisterModule(Module* module) { modules_.push_back(module); }

  void Run() {
    // Set thread-local scheduler for coroutines to access BEFORE creating tasks
    current_scheduler = this;

    // Collect all initial blocks as tasks (they start immediately)
    for (auto* module : modules_) {
      for (auto& method : module->initial_methods_) {
        tasks_.push_back(method());
      }
    }

    // Process delayed tasks
    while (!delay_queue_.empty()) {
      // Get the next time point
      auto it = delay_queue_.begin();
      current_time_ = it->first;

      // Move handles to process (avoid iterator invalidation)
      auto handles = std::move(it->second);
      delay_queue_.erase(it);

      // Resume all handles at this time
      for (auto handle : handles) {
        if (!handle.done()) {
          handle.resume();
        }
      }
    }

    current_scheduler = nullptr;
  }

  void ScheduleDelay(std::coroutine_handle<> handle, uint64_t delay) {
    uint64_t resume_time = current_time_ + delay;
    delay_queue_[resume_time].push_back(handle);
  }

  [[nodiscard]] auto CurrentTime() const -> uint64_t { return current_time_; }

  // Process all delayed tasks until queue is empty
  void ProcessDelayedTasks() {
    while (!delay_queue_.empty()) {
      auto it = delay_queue_.begin();
      current_time_ = it->first;
      auto handles = std::move(it->second);
      delay_queue_.erase(it);
      for (auto handle : handles) {
        if (!handle.done()) {
          handle.resume();
        }
      }
    }
  }

 private:
  std::vector<Module*> modules_;
  std::vector<Task> tasks_;
  uint64_t current_time_ = 0;
  std::map<uint64_t, std::vector<std::coroutine_handle<>>> delay_queue_;
};

// Implementation of Delay::await_suspend (needs Scheduler definition)
inline void Delay::await_suspend(std::coroutine_handle<> handle) const {
  if (current_scheduler != nullptr) {
    current_scheduler->ScheduleDelay(handle, amount_);
  }
}

// Implementation of Module::RunInitials (needs Scheduler definition)
inline auto Module::RunInitials() -> uint64_t {
  Scheduler scheduler;
  // Set scheduler BEFORE creating tasks (they start immediately)
  current_scheduler = &scheduler;
  std::vector<Task> tasks;
  for (auto& method : initial_methods_) {
    tasks.push_back(method());
  }
  // Process any delayed tasks
  scheduler.ProcessDelayedTasks();
  current_scheduler = nullptr;
  return scheduler.CurrentTime();
}

}  // namespace lyra::sdk
