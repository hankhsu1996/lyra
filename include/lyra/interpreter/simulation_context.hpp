#pragma once

#include <sstream>

#include "lyra/interpreter/tracer.hpp"
#include "lyra/interpreter/variable_table.hpp"

namespace lyra::interpreter {

using SimulationTime = uint64_t;

class SimulationContext {
 public:
  SimulationContext() : tracer(current_time) {
  }

  ModuleVariableTable variable_table;
  SimulationTime current_time = 0;
  Tracer tracer;
  std::ostringstream display_output;
};

}  // namespace lyra::interpreter
