#pragma once

#include <string>
#include <vector>

#include "lyra/xir/arena.hpp"
#include "lyra/xir/process.hpp"
#include "lyra/xir/variable.hpp"

namespace lyra::xir {

struct CompilationUnit {
  std::string name;
  std::vector<Variable> variables;
  std::vector<ProcessEntry> processes;
  Arena arena;
};

}  // namespace lyra::xir
