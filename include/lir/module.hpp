#pragma once

#include <memory>
#include <string>
#include <vector>

#include "lir/process.hpp"

namespace lyra::lir {

struct Module {
  std::string name;

  // Declared signal names in the module
  std::vector<std::string> signals;

  // Procedural blocks such as initial and always
  std::vector<std::shared_ptr<Process>> processes;
};

}  // namespace lyra::lir
