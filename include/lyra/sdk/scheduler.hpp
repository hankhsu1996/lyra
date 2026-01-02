#pragma once

#include <vector>

#include "lyra/sdk/module.hpp"
#include "lyra/sdk/task.hpp"

namespace lyra::sdk {

class Scheduler {
 public:
  void RegisterModule(Module* module) {
    modules_.push_back(module);
  }

  void Run() {
    // Collect and run all initial blocks
    std::vector<Task> tasks;
    for (auto* module : modules_) {
      for (auto& method : module->initial_methods_) {
        tasks.push_back(method());
      }
    }
    // Tasks run to completion (no suspend points yet)
  }

 private:
  std::vector<Module*> modules_;
};

}  // namespace lyra::sdk
