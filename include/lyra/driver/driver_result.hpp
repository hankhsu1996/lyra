#pragma once

#include <memory>
#include <string>

#include "lyra/interpreter/simulation_context.hpp"

namespace lyra::driver {

// A wrapper that represents the result of running a simulation.
// Includes the final execution state and the total simulation time.
struct DriverResult {
  std::unique_ptr<interpreter::SimulationContext> context;

  [[nodiscard]] auto ReadVariable(const std::string& name) const
      -> RuntimeValue {
    return context->variable_table.Read(name);
  }

  [[nodiscard]] auto FinalTime() const -> uint64_t {
    return context->current_time;
  }
};

}  // namespace lyra::driver
