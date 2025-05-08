#pragma once

#include "lyra/interpreter/temp_table.hpp"
#include "lyra/interpreter/tracer.hpp"
#include "lyra/interpreter/variable_table.hpp"

namespace lyra::interpreter {

using SimulationTime = uint64_t;

class SimulationContext {
 public:
  SimulationContext() : tracer(current_time) {
  }

  VariableTable variable_table;
  TempTable temp_table;
  SimulationTime current_time = 0;
  Tracer tracer;
};

}  // namespace lyra::interpreter
