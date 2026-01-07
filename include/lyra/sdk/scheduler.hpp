#pragma once

#include <cmath>
#include <coroutine>
#include <cstdint>
#include <format>
#include <map>
#include <string>
#include <vector>

#include "lyra/sdk/bit.hpp"
#include "lyra/sdk/delay.hpp"
#include "lyra/sdk/module.hpp"
#include "lyra/sdk/task.hpp"
#include "lyra/sdk/time_utils.hpp"
#include "lyra/sdk/wait_event.hpp"

namespace lyra::sdk {

// Default timescale constants
constexpr int8_t kDefaultUnitPower = -9;        // 1ns
constexpr int8_t kDefaultPrecisionPower = -12;  // 1ps

/// State for $timeformat system task (IEEE 1800-2017 §21.3)
struct TimeFormatState {
  int8_t units = kUnitsUnset;
  int precision = 0;
  std::string suffix;
  int min_width = 20;

  static constexpr int8_t kUnitsUnset = 127;

  [[nodiscard]] auto FormatModuleTime(
      uint64_t time_in_module_unit, int8_t module_unit_power,
      int8_t global_precision) const -> std::string {
    int8_t effective_units = (units == kUnitsUnset) ? global_precision : units;
    int exponent = module_unit_power - effective_units;
    double scaled = static_cast<double>(time_in_module_unit);

    if (exponent > 0) {
      scaled *= std::pow(10.0, exponent);
    } else if (exponent < 0) {
      scaled /= std::pow(10.0, -exponent);
    }

    auto formatted = std::format("{:.{}f}{}", scaled, precision, suffix);
    if (static_cast<int>(formatted.size()) < min_width) {
      formatted =
          std::string(min_width - static_cast<int>(formatted.size()), ' ') +
          formatted;
    }
    return formatted;
  }
};

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

  // Flush NBA queues for all registered modules
  void FlushAllNba() {
    for (auto* module : modules_) {
      module->FlushNba();
    }
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
        FlushAllNba();

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
    if (!to_resume.empty()) {
      FlushAllNba();
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

// $time - returns 64-bit unsigned simulation time in module's timeunit
// divisor scales from global precision ticks to module's timeunit
inline auto Time(uint64_t divisor = 1) -> Bit<64> {
  return Bit<64>{CurrentTime() / divisor};
}

// $stime - returns low 32 bits of scaled time as unsigned
inline auto STime(uint64_t divisor = 1) -> Bit<32> {
  return Bit<32>{static_cast<uint32_t>((CurrentTime() / divisor) & 0xFFFFFFFF)};
}

// $realtime - returns scaled time as real (double)
inline auto RealTime(uint64_t divisor = 1) -> double {
  return static_cast<double>(CurrentTime()) / static_cast<double>(divisor);
}

// Global $timeformat state for %t formatting
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
inline TimeFormatState time_format_state;

// Global precision power for time formatting (set by generated module)
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
inline int8_t global_precision_power = kDefaultPrecisionPower;

// $timeformat - set format for %t display
// All arguments are optional (uses current values if not provided)
inline void TimeFormat(
    int8_t units = TimeFormatState::kUnitsUnset, int precision = 0,
    const std::string& suffix = "", int min_width = 20) {
  time_format_state.units = units;
  time_format_state.precision = precision;
  time_format_state.suffix = suffix;
  time_format_state.min_width = min_width;
}

// FormatTimeValue - format a time value according to $timeformat settings
// time_value: time value in module's timeunit (e.g., from $time)
// module_unit_power: the module's timeunit power (e.g., -9 for ns)
template <typename T>
inline auto FormatTimeValue(T time_value, int8_t module_unit_power)
    -> std::string {
  return time_format_state.FormatModuleTime(
      static_cast<uint64_t>(time_value), module_unit_power,
      global_precision_power);
}

// PowerToString is available from lyra/sdk/time_utils.hpp

// Implementation of Module::Run (needs Scheduler definition)
inline auto Module::Run() -> uint64_t {
  Scheduler scheduler;
  simulation_finished = false;
  simulation_stopped = false;

  // Collect all modules in hierarchy and register with scheduler
  std::vector<Module*> all_modules;
  CollectAllModules(all_modules);
  for (auto* mod : all_modules) {
    scheduler.RegisterModule(mod);
  }

  // Set scheduler BEFORE creating tasks (they start immediately)
  current_scheduler = &scheduler;

  // Start all processes from all modules
  std::vector<Task> tasks;
  for (auto* mod : all_modules) {
    for (auto& process : mod->processes_) {
      tasks.push_back(process());
    }
  }

  // Flush any NBA from initial execution
  scheduler.FlushAllNba();

  // Check triggers after time 0 - wakes processes waiting on variables
  // modified during initial execution (like always_comb/always_latch)
  scheduler.CheckTriggers();

  // Process any delayed tasks and edge triggers
  scheduler.ProcessDelayedTasks();

  // Final NBA flush
  scheduler.FlushAllNba();

  current_scheduler = nullptr;
  return scheduler.CurrentTime();
}

}  // namespace lyra::sdk
