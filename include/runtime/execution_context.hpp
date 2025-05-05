#pragma once

#include "runtime/temp_table.hpp"
#include "runtime/variable_table.hpp"

namespace lyra {

class ExecutionContext {
 public:
  VariableTable variable_table;
  TempTable temp_table;
};

}  // namespace lyra
