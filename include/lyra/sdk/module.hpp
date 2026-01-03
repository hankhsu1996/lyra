#pragma once

#include <coroutine>
#include <cstdint>
#include <functional>
#include <string>
#include <vector>

#include "lyra/sdk/task.hpp"

namespace lyra::sdk {

class Scheduler;
class Module;

// Thread-local pointer to the current module for NBA access
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
inline thread_local Module* current_module = nullptr;

// Thread-local simulation termination flag (set by $finish)
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
inline thread_local bool simulation_finished = false;

// NOLINTBEGIN(readability-identifier-naming)
// Coroutine awaitable requires specific naming convention from C++ standard

// Awaitable for $finish - sets flag and suspends forever
class Finish {
 public:
  static auto await_ready() -> bool {
    return false;
  }

  static auto await_suspend(std::coroutine_handle<> /*handle*/) -> bool {
    simulation_finished = true;
    return true;  // Suspend forever (never resume)
  }

  static void await_resume() {
  }
};

// NOLINTEND(readability-identifier-naming)

class Module {
 public:
  explicit Module(std::string name) : name_(std::move(name)) {
  }
  virtual ~Module() = default;

  // Non-copyable and non-movable (lambdas capture 'this')
  Module(const Module&) = delete;
  auto operator=(const Module&) -> Module& = delete;
  Module(Module&&) = delete;
  auto operator=(Module&&) -> Module& = delete;

  [[nodiscard]] auto Name() const -> const std::string& {
    return name_;
  }

  // Runs all processes with a local scheduler
  // Returns the final simulation time
  // Defined in scheduler.hpp after Scheduler is defined
  auto Run() -> uint64_t;

  // Non-blocking assignment support
  // Uses std::type_identity_t to deduce T only from the pointer,
  // allowing implicit conversion for the value (e.g., int to Int)
  template <typename T>
  void ScheduleNba(T* target, std::type_identity_t<T> value) {
    nba_queue_.emplace_back([target, value]() { *target = value; });
  }

  void FlushNba() {
    for (auto& action : nba_queue_) {
      action();
    }
    nba_queue_.clear();
  }

 protected:
  template <typename T>
  void RegisterProcess(Task (T::*method)()) {
    processes_.push_back(
        [this, method]() { return (static_cast<T*>(this)->*method)(); });
  }

 private:
  friend class Scheduler;

  std::string name_;
  std::vector<std::function<Task()>> processes_;
  std::vector<std::function<void()>> nba_queue_;
};

}  // namespace lyra::sdk
