#pragma once

#include "lyra/interpreter/temp_table.hpp"
#include "lyra/interpreter/variable_table.hpp"

namespace lyra::interpreter {

class ProcessContext {
 public:
  ProcessContext() = default;

  ProcessVariableTable variable_table;
  TempTable temp_table;
};

}  // namespace lyra::interpreter
