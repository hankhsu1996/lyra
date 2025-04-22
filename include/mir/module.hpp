#pragma once

#include <memory>
#include <string>
#include <vector>

#include "mir/process.hpp"
#include "mir/variable.hpp"

namespace volans::mir {

class Module {
 public:
  std::string name;
  std::vector<Variable> variables;
  std::vector<std::shared_ptr<Process>> processes;
};

}  // namespace volans::mir
