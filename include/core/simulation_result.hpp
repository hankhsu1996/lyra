#pragma once

#include <cstdint>
#include <memory>
#include <string>

#include "core/execution_context.hpp"

namespace lyra {

// A wrapper that represents the result of running a simulation.
// Includes the final execution state and the total simulation time.
struct SimulationResult {
  std::unique_ptr<ExecutionContext> context;
  uint64_t final_time;

  [[nodiscard]] auto ReadSignal(const std::string& name) const -> RuntimeValue {
    return context->signal_table.Read(name);
  }
};

}  // namespace lyra
