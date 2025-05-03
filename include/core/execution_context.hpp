#pragma once

#include "core/signal_table.hpp"
#include "core/temp_table.hpp"

namespace lyra {

class ExecutionContext {
 public:
  SignalTable signal_table;
  TempTable ssa_table;
};

}  // namespace lyra
