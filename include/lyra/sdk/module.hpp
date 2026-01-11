#pragma once

#include <coroutine>
#include <cstdint>
#include <functional>
#include <iostream>
#include <print>
#include <string>
#include <vector>

#include "lyra/sdk/task.hpp"

namespace lyra::sdk {

class Scheduler;
class Module;

// Thread-local simulation termination flag (set by $finish, $stop, $exit)
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
inline thread_local bool simulation_finished = false;

// Thread-local flag for $stop (non-zero exit code)
// NOLINTNEXTLINE(cppcoreguidelines-avoid-non-const-global-variables)
inline thread_local bool simulation_stopped = false;

// Forward declaration - implemented in scheduler.hpp after Scheduler is defined
auto CurrentTime() -> uint64_t;

// Result of running a simulation
struct SimulationResult {
  uint64_t final_time;
  int exit_code;  // 0 for $finish, 1 for $stop
};

// NOLINTBEGIN(readability-identifier-naming)
// Coroutine awaitable requires specific naming convention from C++ standard

// Awaitable for $finish/$exit - terminates simulation with exit code 0
class Finish {
  int level_;
  const char* name_;

 public:
  explicit Finish(int level = 1, const char* name = "$finish")
      : level_(level), name_(name) {
  }

  static auto await_ready() -> bool {
    return false;
  }

  [[nodiscard]] auto await_suspend(std::coroutine_handle<> /*handle*/) const
      -> bool {
    if (level_ >= 1) {
      std::println(std::cout, "{} called at time {}", name_, CurrentTime());
    }
    simulation_finished = true;
    return true;  // Suspend forever (never resume)
  }

  static void await_resume() {
  }
};

// Awaitable for $stop - terminates simulation with non-zero exit code
class Stop {
  int level_;

 public:
  explicit Stop(int level = 1) : level_(level) {
  }

  static auto await_ready() -> bool {
    return false;
  }

  [[nodiscard]] auto await_suspend(std::coroutine_handle<> /*handle*/) const
      -> bool {
    if (level_ >= 1) {
      std::println(std::cout, "$stop called at time {}", CurrentTime());
    }
    simulation_finished = true;
    simulation_stopped = true;
    return true;  // Suspend forever (never resume)
  }

  static void await_resume() {
  }
};

// Awaitable for $fatal - terminates simulation after printing severity message
class Fatal {
  int finish_number_;
  std::string_view file_;
  int line_;
  std::string_view scope_;
  std::string message_;

 public:
  Fatal(
      int finish_number, std::string_view file, int line,
      std::string_view scope, std::string message = "")
      : finish_number_(finish_number),
        file_(file),
        line_(line),
        scope_(scope),
        message_(std::move(message)) {
  }

  static auto await_ready() -> bool {
    return false;
  }

  [[nodiscard]] auto await_suspend(std::coroutine_handle<> /*handle*/) const
      -> bool {
    if (finish_number_ >= 1) {
      if (message_.empty()) {
        std::println(
            std::cout, "FATAL: {}:{}: {} @ {}", file_, line_, scope_,
            CurrentTime());
      } else {
        std::println(
            std::cout, "FATAL: {}:{}: {} @ {}: {}", file_, line_, scope_,
            CurrentTime(), message_);
      }
    }
    simulation_finished = true;
    simulation_stopped = true;  // $fatal has non-zero exit code
    return true;                // Suspend forever (never resume)
  }

  static void await_resume() {
  }
};

// Print severity message for $error, $warning, $info (non-terminating)
inline void SeverityMessage(
    std::string_view severity, std::string_view file, int line,
    std::string_view scope, const std::string& message = "") {
  if (message.empty()) {
    std::println(
        std::cout, "{}: {}:{}: {} @ {}", severity, file, line, scope,
        CurrentTime());
  } else {
    std::println(
        std::cout, "{}: {}:{}: {} @ {}: {}", severity, file, line, scope,
        CurrentTime(), message);
  }
}

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
  // Defined in scheduler.hpp after Scheduler is defined
  auto Run() -> SimulationResult;

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

  [[nodiscard]] auto HasPendingNba() const -> bool {
    return !nba_queue_.empty();
  }

  // Collect all modules in hierarchy (self + all descendants)
  void CollectAllModules(std::vector<Module*>& all_modules) {
    all_modules.push_back(this);
    for (auto* child : child_modules_) {
      child->CollectAllModules(all_modules);
    }
  }

 protected:
  template <typename T>
  void RegisterProcess(Task (T::*method)()) {
    processes_.push_back(
        [this, method]() { return (static_cast<T*>(this)->*method)(); });
  }

  void RegisterChild(Module* child) {
    child_modules_.push_back(child);
  }

 private:
  friend class Scheduler;

  std::string name_;
  std::vector<std::function<Task()>> processes_;
  std::vector<std::function<void()>> nba_queue_;
  std::vector<Module*> child_modules_;
};

}  // namespace lyra::sdk
