#pragma once

#include <coroutine>
#include <cstdint>
#include <map>
#include <vector>

#include "lyra/sdk/delay.hpp"
#include "lyra/sdk/module.hpp"
#include "lyra/sdk/task.hpp"
#include "lyra/sdk/wait_event.hpp"

namespace lyra::sdk {

// Waiter represents a suspended coroutine waiting for a value change
// Uses type-erased reader function for extensibility to any type/bit-width
struct Waiter {
  std::coroutine_handle<> handle;
  std::function<int64_t()> read_value;  // Type-erased value reader
  EdgeKind kind;
  int64_t previous_value;
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

  void RegisterWait(
      std::coroutine_handle<> handle, std::function<int64_t()> read_value,
      EdgeKind kind, int64_t current_value) {
    waiters_.push_back(
        Waiter{
            .handle = handle,
            .read_value = std::move(read_value),
            .kind = kind,
            .previous_value = current_value});
  }

  [[nodiscard]] auto CurrentTime() const -> uint64_t {
    return current_time_;
  }

  // Process all delayed tasks until queue is empty or simulation finishes
  void ProcessDelayedTasks() {
    while (!simulation_finished &&
           (!delay_queue_.empty() || !waiters_.empty())) {
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

        // After active region, check triggers
        if (!simulation_finished) {
          CheckTriggers();
        }
      } else {
        // No more delays but waiters exist - this means simulation stalled
        // (no events to trigger waiters). This is normal for always_comb
        // when inputs don't change anymore.
        break;
      }
    }
  }

 private:
  // Check if value change should trigger based on edge kind
  // Follows the same pattern as interpreter's TriggerManager::ShouldTrigger
  static auto ShouldTrigger(
      int64_t old_value, int64_t new_value, EdgeKind edge_kind) -> bool {
    if (edge_kind == EdgeKind::kAnyChange) {
      return old_value != new_value;
    }

    // For edge detection, only check bit 0
    uint64_t old_bit0 = static_cast<uint64_t>(old_value) & 1;
    uint64_t new_bit0 = static_cast<uint64_t>(new_value) & 1;

    switch (edge_kind) {
      case EdgeKind::kPosedge:
        return (old_bit0 == 0 && new_bit0 == 1);
      case EdgeKind::kNegedge:
        return (old_bit0 == 1 && new_bit0 == 0);
      case EdgeKind::kBothEdge:
        return (old_bit0 != new_bit0);
      default:
        return false;
    }
  }

  void CheckTriggers() {
    std::vector<std::coroutine_handle<>> to_resume;

    // First pass: find which handles should be resumed
    for (const auto& waiter : waiters_) {
      int64_t current_value = waiter.read_value();
      if (ShouldTrigger(waiter.previous_value, current_value, waiter.kind)) {
        // Only add if not already in to_resume (OR semantics)
        bool already_added = false;
        for (const auto& h : to_resume) {
          if (h.address() == waiter.handle.address()) {
            already_added = true;
            break;
          }
        }
        if (!already_added) {
          to_resume.push_back(waiter.handle);
        }
      }
    }

    // Second pass: remove ALL waiters for triggered handles and update others
    auto it = waiters_.begin();
    while (it != waiters_.end()) {
      bool should_remove = false;
      for (const auto& h : to_resume) {
        if (h.address() == it->handle.address()) {
          should_remove = true;
          break;
        }
      }
      if (should_remove) {
        it = waiters_.erase(it);
      } else {
        // Update previous value for next check
        it->previous_value = it->read_value();
        ++it;
      }
    }

    // Resume triggered coroutines
    for (auto handle : to_resume) {
      if (!handle.done() && !simulation_finished) {
        handle.resume();
      }
    }

    // Flush NBA after triggered processes run
    if (!to_resume.empty() && current_module != nullptr) {
      current_module->FlushNba();
    }

    // If any coroutines resumed, they might have modified variables
    // that trigger more waiters - check again
    if (!to_resume.empty() && !simulation_finished) {
      CheckTriggers();
    }
  }

  std::vector<Module*> modules_;
  std::vector<Task> tasks_;
  uint64_t current_time_ = 0;
  std::map<uint64_t, std::vector<std::coroutine_handle<>>> delay_queue_;
  std::vector<Waiter> waiters_;
};

// Implementation of Delay::await_suspend (needs Scheduler definition)
inline void Delay::await_suspend(std::coroutine_handle<> handle) const {
  if (current_scheduler != nullptr) {
    current_scheduler->ScheduleDelay(handle, amount_);
  }
}

// Implementation of ImplicitEvent<T>::await_suspend (needs Scheduler
// definition)
template <typename T>
void ImplicitEvent<T>::await_suspend(std::coroutine_handle<> handle) const {
  if (current_scheduler != nullptr) {
    // Capture type in lambda for type-safe reading
    T* var = var_;
    current_scheduler->RegisterWait(
        handle, [var]() { return static_cast<int64_t>(*var); }, kind_,
        static_cast<int64_t>(*var_));
  }
}

// Implementation of ImplicitEventOr::await_suspend (needs Scheduler definition)
inline void ImplicitEventOr::await_suspend(
    std::coroutine_handle<> handle) const {
  if (current_scheduler != nullptr) {
    for (const auto& trigger : triggers_) {
      current_scheduler->RegisterWait(
          handle, trigger.read_value, trigger.kind, trigger.value);
    }
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
