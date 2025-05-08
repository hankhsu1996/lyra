#pragma once

#include <memory>
#include <string>
#include <vector>

#include "lyra/common/variable.hpp"
#include "lyra/mir/process.hpp"

namespace lyra::mir {

class Module {
 public:
  std::string name;
  std::vector<common::Variable> variables;
  std::vector<std::shared_ptr<Process>> processes;
};

}  // namespace lyra::mir
