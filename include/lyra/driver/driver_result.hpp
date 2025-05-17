#pragma once

#include <memory>

#include <slang/ast/Compilation.h>

#include "lyra/common/symbol.hpp"
#include "lyra/interpreter/simulation_context.hpp"

namespace lyra::driver {

using SymbolRef = common::SymbolRef;

// A wrapper that represents the result of running a simulation.
// Includes the final execution state and the total simulation time.
struct DriverResult {
  std::unique_ptr<slang::ast::Compilation> compilation;
  std::unique_ptr<interpreter::SimulationContext> context;
  std::shared_ptr<lir::LirContext> lir_context;

  [[nodiscard]] auto ReadVariable(const std::string& name) const
      -> RuntimeValue {
    return context->variable_table.ReadFromName(name);
  }

  [[nodiscard]] auto FinalTime() const -> uint64_t {
    return context->current_time;
  }
};

}  // namespace lyra::driver
