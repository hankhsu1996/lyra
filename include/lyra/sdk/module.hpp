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

  [[nodiscard]] auto Name() const -> const std::string& {
    return name_;
  }

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
