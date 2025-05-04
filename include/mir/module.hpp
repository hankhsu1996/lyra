#pragma once

#include <memory>
#include <string>
#include <vector>

#include "common/variable.hpp"
#include "mir/process.hpp"

namespace lyra::mir {

class Module {
 public:
  std::string name;
  std::vector<common::Variable> variables;
  std::vector<std::shared_ptr<Process>> processes;
};

}  // namespace lyra::mir
