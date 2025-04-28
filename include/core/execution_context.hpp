#pragma once

#include "core/signal_table.hpp"
#include "core/ssa_table.hpp"

namespace lyra {

class ExecutionContext {
 public:
  SignalTable signalTable;
  SsaTable ssaTable;
};

}  // namespace lyra
