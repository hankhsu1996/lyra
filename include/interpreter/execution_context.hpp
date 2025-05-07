#pragma once

#include "interpreter/temp_table.hpp"
#include "interpreter/tracer.hpp"
#include "interpreter/variable_table.hpp"

namespace lyra::interpreter {

class ExecutionContext {
 public:
  ExecutionContext() = default;

  VariableTable variable_table;
  TempTable temp_table;
  Tracer tracer;
};

}  // namespace lyra::interpreter
