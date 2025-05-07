#pragma once

#include "interpreter/temp_table.hpp"
#include "interpreter/tracer.hpp"
#include "interpreter/variable_table.hpp"

namespace lyra::interpreter {

using SimulationTime = uint64_t;

class ExecutionContext {
 public:
  ExecutionContext() : tracer(current_time) {
  }

  VariableTable variable_table;
  TempTable temp_table;
  SimulationTime current_time = 0;
  Tracer tracer;
};

}  // namespace lyra::interpreter
