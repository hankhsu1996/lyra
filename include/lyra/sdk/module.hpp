#pragma once

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

inline void Finish() {
  simulation_finished = true;
}

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

  // Runs all initial blocks with a local scheduler
  // Returns the final simulation time
  // Defined in scheduler.hpp after Scheduler is defined
  auto RunInitials() -> uint64_t;

  // Non-blocking assignment support
  template <typename T>
  void ScheduleNba(T* target, T value) {
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
  void RegisterInitial(Task (T::*method)()) {
    initial_methods_.push_back(
        [this, method]() { return (static_cast<T*>(this)->*method)(); });
  }

  template <typename T>
  void RegisterAlways(Task (T::*method)()) {
    always_methods_.push_back(
        [this, method]() { return (static_cast<T*>(this)->*method)(); });
  }

 private:
  friend class Scheduler;

  std::string name_;
  std::vector<std::function<Task()>> initial_methods_;
  std::vector<std::function<Task()>> always_methods_;
  std::vector<std::function<void()>> nba_queue_;
};

}  // namespace lyra::sdk
