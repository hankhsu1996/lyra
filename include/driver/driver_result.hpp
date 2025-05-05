#pragma once

#include <cstdint>
#include <memory>
#include <string>

#include "runtime/execution_context.hpp"

namespace lyra::driver {

// A wrapper that represents the result of running a simulation.
// Includes the final execution state and the total simulation time.
struct DriverResult {
  std::unique_ptr<ExecutionContext> context;
  uint64_t final_time;

  [[nodiscard]] auto ReadVariable(const std::string& name) const
      -> RuntimeValue {
    return context->variable_table.Read(name);
  }
};

}  // namespace lyra::driver
