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

// Waiter represents a suspended coroutine waiting for a trigger condition
struct Waiter {
  std::coroutine_handle<> handle;
  std::function<bool()> check_triggered;  // Type-erased trigger check
};

class Scheduler {
 public:
  void RegisterModule(Module* module) {
    modules_.push_back(module);
  }

  void Run() {
    // Set thread-local scheduler for coroutines to access BEFORE creating tasks
    current_scheduler = this;

    // Collect all processes as tasks (they start immediately)
    for (auto* module : modules_) {
      for (auto& process : module->processes_) {
        tasks_.push_back(process());
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
      std::coroutine_handle<> handle, std::function<bool()> check_triggered) {
    waiters_.push_back(
        Waiter{
            .handle = handle, .check_triggered = std::move(check_triggered)});
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

  // Check triggers and wake waiting processes
  // Called after events that modify variables (mirrors interpreter's
  // WakeWaitingProcesses)
  void CheckTriggers() {
    std::vector<std::coroutine_handle<>> to_resume;

    // First pass: check each waiter's trigger condition
    for (auto& waiter : waiters_) {
      if (waiter.check_triggered()) {
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

    // Second pass: remove ALL waiters for triggered handles
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

 private:
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

// Implementation of Trigger<T>::await_suspend (needs Scheduler definition)
template <typename T>
void Trigger<T>::await_suspend(std::coroutine_handle<> handle) const {
  if (current_scheduler != nullptr) {
    current_scheduler->RegisterWait(handle, MakeChecker());
  }
}

// Implementation of AnyChangeAwaitable<Ts...>::await_suspend
template <typename... Ts>
void AnyChangeAwaitable<Ts...>::await_suspend(
    std::coroutine_handle<> handle) const {
  if (current_scheduler != nullptr) {
    std::apply(
        [handle](auto*... vars) {
          (current_scheduler->RegisterWait(
               handle, detail::MakeTriggerChecker(vars, EdgeKind::kAnyChange)),
           ...);
        },
        vars_);
  }
}

// Implementation of AnyOfAwaitable<Triggers...>::await_suspend
template <typename... Triggers>
void AnyOfAwaitable<Triggers...>::await_suspend(
    std::coroutine_handle<> handle) const {
  if (current_scheduler != nullptr) {
    std::apply(
        [handle](const auto&... triggers) {
          (current_scheduler->RegisterWait(handle, triggers.MakeChecker()),
           ...);
        },
        triggers_);
  }
}

// Implementation of CurrentTime() for diagnostic output
inline auto CurrentTime() -> uint64_t {
  return current_scheduler != nullptr ? current_scheduler->CurrentTime() : 0;
}

// Implementation of Module::Run (needs Scheduler definition)
inline auto Module::Run() -> uint64_t {
  Scheduler scheduler;
  // Set scheduler and module BEFORE creating tasks (they start immediately)
  current_scheduler = &scheduler;
  current_module = this;
  simulation_finished = false;
  simulation_stopped = false;
  std::vector<Task> tasks;

  // Start all processes
  for (auto& process : processes_) {
    tasks.push_back(process());
  }

  // Flush any NBA from initial execution
  FlushNba();

  // Check triggers after time 0 - wakes processes waiting on variables
  // modified during initial execution (like always_comb/always_latch)
  scheduler.CheckTriggers();

  // Process any delayed tasks and edge triggers
  scheduler.ProcessDelayedTasks();

  // Final NBA flush
  FlushNba();

  current_module = nullptr;
  current_scheduler = nullptr;
  return scheduler.CurrentTime();
}

}  // namespace lyra::sdk
