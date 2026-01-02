#pragma once

#include <functional>
#include <string>
#include <vector>

#include "lyra/sdk/task.hpp"

namespace lyra::sdk {

class Scheduler;

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

 protected:
  template <typename T>
  void RegisterInitial(Task (T::*method)()) {
    initial_methods_.push_back(
        [this, method]() { return (static_cast<T*>(this)->*method)(); });
  }

 private:
  friend class Scheduler;

  std::string name_;
  std::vector<std::function<Task()>> initial_methods_;
};

}  // namespace lyra::sdk
