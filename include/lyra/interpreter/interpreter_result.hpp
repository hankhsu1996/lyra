#pragma once

#include <memory>

#include <slang/ast/Compilation.h>

#include "lyra/common/symbol.hpp"
#include "lyra/interpreter/instance_context.hpp"
#include "lyra/interpreter/simulation_context.hpp"

namespace lyra::interpreter {

using SymbolRef = common::SymbolRef;

// A wrapper that represents the result of running a simulation.
// Includes the final execution state and the total simulation time.
struct InterpreterResult {
  std::unique_ptr<slang::ast::Compilation> compilation;
  std::unique_ptr<SimulationContext> context;
  std::shared_ptr<lir::LirContext> lir_context;
  std::shared_ptr<InstanceContext> top_instance;

  [[nodiscard]] auto ReadVariable(const std::string& name) const
      -> RuntimeValue {
    // First try to read from top instance's per-instance storage
    if (top_instance != nullptr) {
      for (const auto& [symbol, value] : top_instance->variables) {
        if (symbol->name == name) {
          return value;
        }
      }
    }
    // Fall back to global variable table (for backwards compatibility)
    return context->variable_table.ReadFromName(name);
  }

  [[nodiscard]] auto FinalTime() const -> uint64_t {
    return context->current_time;
  }

  [[nodiscard]] auto CapturedOutput() const -> std::string {
    return context->display_output.str();
  }

  [[nodiscard]] auto Stopped() const -> bool {
    return context->stopped;
  }
};

}  // namespace lyra::interpreter
