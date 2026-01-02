#pragma once

#include <coroutine>
#include <cstdint>
#include <map>
#include <unordered_map>
#include <vector>

#include "lyra/sdk/delay.hpp"
#include "lyra/sdk/module.hpp"
#include "lyra/sdk/task.hpp"
#include "lyra/sdk/wait_event.hpp"

namespace lyra::sdk {

enum class EdgeKind { kPosedge, kNegedge };

struct EdgeWaiter {
  std::coroutine_handle<> handle;
  bool* var;
  EdgeKind edge;
  bool previous_value;
};

class Scheduler {
 public:
  void RegisterModule(Module* module) {
    modules_.push_back(module);
  }

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

  void RegisterEdgeWait(
      std::coroutine_handle<> handle, bool* var, EdgeKind edge) {
    edge_waiters_.push_back(
        EdgeWaiter{
            .handle = handle,
            .var = var,
            .edge = edge,
            .previous_value = *var});
  }

  [[nodiscard]] auto CurrentTime() const -> uint64_t {
    return current_time_;
  }

  // Process all delayed tasks until queue is empty or simulation finishes
  void ProcessDelayedTasks() {
    while (!simulation_finished &&
           (!delay_queue_.empty() || !edge_waiters_.empty())) {
      // Process delays at next time point
      if (!delay_queue_.empty()) {
        auto it = delay_queue_.begin();
        current_time_ = it->first;
        auto handles = std::move(it->second);
        delay_queue_.erase(it);

        // Resume all handles at this time
        for (auto handle : handles) {
          if (!handle.done() && !simulation_finished) {
            handle.resume();
          }
        }

        // Flush NBA after active region
        if (current_module != nullptr) {
          current_module->FlushNba();
        }

        // After active region, check edge triggers
        if (!simulation_finished) {
          CheckEdgeTriggers();
        }
      } else {
        // No more delays but edge waiters exist - this means simulation stalled
        // (no events to trigger edges). This shouldn't happen in well-formed
        // SV.
        break;
      }
    }
  }

 private:
  void CheckEdgeTriggers() {
    std::vector<std::coroutine_handle<>> to_resume;

    // Check each edge waiter
    auto it = edge_waiters_.begin();
    while (it != edge_waiters_.end()) {
      bool current_value = *it->var;
      bool triggered = false;

      if (it->edge == EdgeKind::kPosedge) {
        // Posedge: 0 -> 1
        triggered = !it->previous_value && current_value;
      } else {
        // Negedge: 1 -> 0
        triggered = it->previous_value && !current_value;
      }

      if (triggered) {
        to_resume.push_back(it->handle);
        it = edge_waiters_.erase(it);
      } else {
        // Update previous value for next check
        it->previous_value = current_value;
        ++it;
      }
    }

    // Resume triggered coroutines
    for (auto handle : to_resume) {
      if (!handle.done() && !simulation_finished) {
        handle.resume();
      }
    }

    // Flush NBA after edge-triggered processes run
    if (!to_resume.empty() && current_module != nullptr) {
      current_module->FlushNba();
    }

    // If any coroutines resumed, they might have modified variables
    // that trigger more edge waiters - check again
    if (!to_resume.empty() && !simulation_finished) {
      CheckEdgeTriggers();
    }
  }

  std::vector<Module*> modules_;
  std::vector<Task> tasks_;
  uint64_t current_time_ = 0;
  std::map<uint64_t, std::vector<std::coroutine_handle<>>> delay_queue_;
  std::vector<EdgeWaiter> edge_waiters_;
};

// Implementation of Delay::await_suspend (needs Scheduler definition)
inline void Delay::await_suspend(std::coroutine_handle<> handle) const {
  if (current_scheduler != nullptr) {
    current_scheduler->ScheduleDelay(handle, amount_);
  }
}

// Implementation of WaitPosedge::await_suspend (needs Scheduler definition)
inline void WaitPosedge::await_suspend(std::coroutine_handle<> handle) const {
  if (current_scheduler != nullptr) {
    current_scheduler->RegisterEdgeWait(handle, var_, EdgeKind::kPosedge);
  }
}

// Implementation of WaitNegedge::await_suspend (needs Scheduler definition)
inline void WaitNegedge::await_suspend(std::coroutine_handle<> handle) const {
  if (current_scheduler != nullptr) {
    current_scheduler->RegisterEdgeWait(handle, var_, EdgeKind::kNegedge);
  }
}

// Implementation of Module::RunInitials (needs Scheduler definition)
inline auto Module::RunInitials() -> uint64_t {
  Scheduler scheduler;
  // Set scheduler and module BEFORE creating tasks (they start immediately)
  current_scheduler = &scheduler;
  current_module = this;
  simulation_finished = false;
  std::vector<Task> tasks;

  // Start all initial blocks
  for (auto& method : initial_methods_) {
    tasks.push_back(method());
  }

  // Start all always blocks (they run as infinite loops with WaitEvent)
  for (auto& method : always_methods_) {
    tasks.push_back(method());
  }

  // Flush any NBA from initial execution
  FlushNba();

  // Process any delayed tasks and edge triggers
  scheduler.ProcessDelayedTasks();

  // Final NBA flush
  FlushNba();

  current_module = nullptr;
  current_scheduler = nullptr;
  return scheduler.CurrentTime();
}

}  // namespace lyra::sdk
