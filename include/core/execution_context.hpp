#pragma once

#include <unordered_map>

#include "core/signal_table.hpp"
#include "core/simulation_context.hpp"
#include "core/value.hpp"

namespace lyra {

class ExecutionContext {
 public:
  SignalTable signalTable;
  SimulationContext simContext;

  // SSA register table
  std::unordered_map<std::string, RuntimeValue> ssa_table;

  auto ReadSSA(const std::string& name) const -> RuntimeValue;
  void WriteSSA(const std::string& name, const RuntimeValue& value);
};

}  // namespace lyra
