#pragma once

#include <algorithm>
#include <coroutine>
#include <cstdint>
#include <functional>
#include <map>
#include <queue>
#include <string>
#include <vector>

#include "lyra/common/simulation_region.hpp"
#include "lyra/sdk/bit.hpp"
#include "lyra/sdk/delay.hpp"
#include "lyra/sdk/module.hpp"
#include "lyra/sdk/runtime_config.hpp"
#include "lyra/sdk/task.hpp"
#include "lyra/sdk/wait_event.hpp"

namespace lyra::sdk {

// Waiter represents a suspended coroutine waiting for a trigger condition
struct Waiter {
  std::coroutine_handle<> handle;
  std::function<bool()> check_triggered;  // Type-erased trigger check
};

/// State for active $monitor.
/// Only one monitor can be active at a time.
///
/// The check_and_print lambda is expected to:
/// - Capture `this` (the module) for member variable access
/// - Capture previous values as mutable copies for change detection
/// - Compare current vs previous values and print if changed
/// - Update previous values after printing
///
/// Lifetime: The module instance must outlive the scheduler (guaranteed by
/// Module::Run which creates scheduler on stack).
struct MonitorState {
  explicit MonitorState(
      std::function<void()> check_fn, std::optional<uint32_t> fd = std::nullopt)
      : check_and_print(std::move(check_fn)), file_descriptor(fd) {
  }

  MonitorState(const MonitorState&) = delete;
  MonitorState(MonitorState&&) = default;
  auto operator=(const MonitorState&) -> MonitorState& = delete;
  auto operator=(MonitorState&&) -> MonitorState& = default;
  ~MonitorState() = default;

  bool enabled = true;
  std::function<void()> check_and_print;  // Mutable lambda with captured state

  // File descriptor for $fmonitor output (for $fclose cancellation).
  // nullopt = stdout ($monitor), has value = file output ($fmonitor)
  std::optional<uint32_t> file_descriptor;
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

  // Schedule to Inactive region (for #0 delays)
  void ScheduleInactive(std::coroutine_handle<> handle) {
    inactive_queue_.push(handle);
  }

  // Schedule action for Postponed region ($strobe, $monitor)
  void SchedulePostponed(std::function<void()> action) {
    postponed_queue_.push_back(std::move(action));
  }

  // Register a new monitor (replaces any existing).
  // Uses perfect forwarding to avoid intermediate std::function construction
  // when passing a lambda directly.
  template <typename F>
  void SetMonitor(
      F&& check_and_print,
      std::optional<uint32_t> file_descriptor = std::nullopt) {
    active_monitor_.emplace(std::forward<F>(check_and_print), file_descriptor);
  }

  // Enable/disable monitor output ($monitoron/$monitoroff)
  void SetMonitorEnabled(bool enabled) {
    if (active_monitor_) {
      active_monitor_->enabled = enabled;
    }
  }

  // Cancel $fmonitor if its file descriptor matches (for $fclose)
  void CancelMonitorIfFileDescriptor(uint32_t descriptor) {
    if (active_monitor_ && active_monitor_->file_descriptor == descriptor) {
      active_monitor_.reset();
    }
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

  // Check if any module has pending NBA actions
  [[nodiscard]] auto HasAnyNba() const -> bool {
    return std::ranges::any_of(
        modules_, [](const Module* m) { return m->HasPendingNba(); });
  }

  // Check if Active group has pending activity
  // Per LRM 4.4: Active group includes Active, Inactive, and NBA regions
  [[nodiscard]] auto HasActiveGroupActivity() const -> bool {
    return !active_handles_.empty() || !inactive_queue_.empty() || HasAnyNba();
  }

  // Check if Reactive group has pending activity (stub - always false)
  // NOLINTNEXTLINE(readability-convert-member-functions-to-static)
  [[nodiscard]] auto HasActivityInReactiveGroup() const -> bool {
    // Stub: Reactive group not implemented yet (for program blocks)
    // Will access reactive queues when implemented
    return false;
  }

  // Check if any simulation activity remains (Active or Reactive groups)
  [[nodiscard]] auto HasPendingActivity() const -> bool {
    return HasActiveGroupActivity() || HasActivityInReactiveGroup();
  }

  // Process Inactive queue: move handles to Active
  void ProcessInactiveQueue() {
    while (!inactive_queue_.empty()) {
      active_handles_.push_back(inactive_queue_.front());
      inactive_queue_.pop();
    }
  }

  // Execute all actions in Postponed region
  void FlushPostponed() {
    for (const auto& action : postponed_queue_) {
      action();
    }
    postponed_queue_.clear();
  }

  // Process all delayed tasks until queue is empty or simulation finishes
  void ProcessDelayedTasks() {
    while (!simulation_finished &&
           (!delay_queue_.empty() || !waiters_.empty())) {
      // Process delays at next time point
      if (!delay_queue_.empty()) {
        auto it = delay_queue_.begin();
        current_time_ = it->first;

        // Move handles to active list for processing
        active_handles_ = std::move(it->second);
        delay_queue_.erase(it);

        // Execute time slot with proper region handling
        ExecuteTimeSlot();
      } else {
        // No more delays but waiters exist - this means simulation stalled
        // (no events to trigger waiters). This is normal for always_comb
        // when inputs don't change anymore.
        break;
      }
    }
  }

  // Execute a single region (mirrors interpreter's ExecuteRegion)
  void ExecuteRegion(common::Region region) {
    using common::Region;
    switch (region) {
      case Region::kActive:
        // Invariant: Process until empty, handles may add to inactive_queue_
        while (!active_handles_.empty() && !simulation_finished) {
          auto handle = active_handles_.back();
          active_handles_.pop_back();
          if (!handle.done()) {
            handle.resume();
          }
        }
        break;

      case Region::kInactive:
        ProcessInactiveQueue();
        break;

      case Region::kNBA:
        FlushAllNba();
        break;

      case Region::kPostponed:
        FlushPostponed();
        break;

      // Stub regions (not yet implemented)
      case Region::kPreponed:
      case Region::kObserved:
      case Region::kReactive:
      case Region::kReInactive:
      case Region::kReNBA:
        break;
    }
  }

  // Execute a complete time slot per IEEE 1800 Section 4.4
  void ExecuteTimeSlot() {
    using common::Region;

    // Preponed region: #1step sampling (stub)
    ExecuteRegion(Region::kPreponed);

    // Check triggers at start of time slot to update prev values and wake
    // processes. This handles the case where initial process execution
    // (outside ExecuteTimeSlot) modified variables that should trigger waiters.
    if (!simulation_finished) {
      CheckTriggersToActive();
    }

    // Main iteration loop (LRM 4.5)
    while (HasPendingActivity() && !simulation_finished) {
      // Active group iteration: Active -> Inactive -> NBA
      while (HasActiveGroupActivity() && !simulation_finished) {
        ExecuteRegion(Region::kActive);
        ExecuteRegion(Region::kInactive);
        ExecuteRegion(Region::kNBA);

        // Check triggers (wakes waiting processes to active)
        if (!simulation_finished) {
          CheckTriggersToActive();
        }
      }

      // Observed region: assertion evaluation (stub)
      ExecuteRegion(Region::kObserved);

      // Reactive group iteration (stub - for program blocks)
      while (HasActivityInReactiveGroup() && !simulation_finished) {
        ExecuteRegion(Region::kReactive);
        ExecuteRegion(Region::kReInactive);
        ExecuteRegion(Region::kReNBA);
      }
    }

    // Postponed region: $strobe, $monitor
    ExecuteRegion(Region::kPostponed);

    // $monitor: check for value changes and print if needed
    CheckMonitor();
  }

  // Check triggers and schedule to active (not immediate resume)
  void CheckTriggersToActive() {
    auto handles = CollectTriggeredHandles();
    for (auto handle : handles) {
      if (!handle.done()) {
        active_handles_.push_back(handle);
      }
    }
  }

  // Check triggers and wake waiting processes
  // Called after events that modify variables (mirrors interpreter's
  // WakeWaitingProcesses)
  void CheckTriggers() {
    auto triggered = CollectTriggeredHandles();

    // Resume triggered coroutines
    for (auto handle : triggered) {
      if (!handle.done() && !simulation_finished) {
        handle.resume();
      }
    }

    // Flush NBA after triggered processes run
    if (!triggered.empty()) {
      FlushAllNba();

      // If any coroutines resumed, they might have modified variables
      // that trigger more waiters - check again
      if (!simulation_finished) {
        CheckTriggers();
      }
    }
  }

 private:
  // Collect triggered handles and remove them from waiters_
  // Returns handles that should be woken (deduplicated by address)
  auto CollectTriggeredHandles() -> std::vector<std::coroutine_handle<>> {
    std::vector<std::coroutine_handle<>> triggered;

    // Collect triggered handles (deduplicate by address)
    for (auto& waiter : waiters_) {
      if (waiter.check_triggered()) {
        auto* addr = waiter.handle.address();
        bool already = std::ranges::any_of(
            triggered, [addr](auto h) { return h.address() == addr; });
        if (!already) {
          triggered.push_back(waiter.handle);
        }
      }
    }

    // Remove triggered waiters using C++20 erase_if
    std::erase_if(waiters_, [&](const Waiter& w) {
      auto* addr = w.handle.address();
      return std::ranges::any_of(
          triggered, [addr](auto h) { return h.address() == addr; });
    });

    return triggered;
  }

  std::vector<Module*> modules_;  // Non-owning, borrowed from Module::Run()
  std::vector<Task> tasks_;
  uint64_t current_time_ = 0;
  std::map<uint64_t, std::vector<std::coroutine_handle<>>> delay_queue_;
  std::vector<Waiter> waiters_;

  // Region queues (IEEE 1800 Section 4.4)
  std::vector<std::coroutine_handle<>> active_handles_;  // Active region
  std::queue<std::coroutine_handle<>> inactive_queue_;   // Inactive region
  std::vector<std::function<void()>> postponed_queue_;   // Postponed region

  // $monitor state
  std::optional<MonitorState> active_monitor_;

  // Check $monitor for value changes and print if needed
  void CheckMonitor() {
    if (active_monitor_ && active_monitor_->enabled &&
        active_monitor_->check_and_print) {
      active_monitor_->check_and_print();
    }
  }
};

// Implementation of Delay::await_suspend (needs Scheduler definition)
inline void Delay::await_suspend(std::coroutine_handle<> handle) const {
  if (current_scheduler != nullptr) {
    current_scheduler->ScheduleDelay(handle, amount_);
  }
}

// Implementation of ZeroDelay::await_suspend (schedules to Inactive region)
// NOLINTNEXTLINE(readability-convert-member-functions-to-static)
inline void ZeroDelay::await_suspend(std::coroutine_handle<> handle) const {
  if (current_scheduler != nullptr) {
    current_scheduler->ScheduleInactive(handle);
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

// Implementation of Module::Run (needs Scheduler definition)
inline auto Module::Run() -> SimulationResult {
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
  // Processes run their initial block until first delay/wait
  std::vector<Task> tasks;
  for (auto* mod : all_modules) {
    for (auto& process : mod->processes_) {
      tasks.push_back(process());
    }
  }

  // Execute time 0 slot with proper region handling
  // This handles NBA flush, triggers, and #0 delays from initial execution
  scheduler.ExecuteTimeSlot();

  // Process remaining time slots
  scheduler.ProcessDelayedTasks();

  current_scheduler = nullptr;
  return {
      .final_time = scheduler.CurrentTime(),
      .exit_code = simulation_stopped ? 1 : 0};
}

}  // namespace lyra::sdk
