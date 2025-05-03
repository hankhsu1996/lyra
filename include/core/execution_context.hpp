#pragma once

#include "core/temp_table.hpp"
#include "core/variable_table.hpp"

namespace lyra {

class ExecutionContext {
 public:
  VariableTable variable_table;
  TempTable temp_table;
};

}  // namespace lyra
