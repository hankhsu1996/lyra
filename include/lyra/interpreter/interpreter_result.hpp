#pragma once

#include <memory>

#include <slang/ast/Compilation.h>

#include "lyra/common/symbol.hpp"
#include "lyra/interpreter/hierarchy_context.hpp"
#include "lyra/interpreter/simulation_context.hpp"

namespace slang {
class SourceManager;
}

namespace lyra::interpreter {

// A wrapper that represents the result of running a simulation.
// Includes the final execution state and the total simulation time.
// Note: source_manager must be kept alive because symbol->name is a string_view
// pointing into memory owned by the SourceManager.
struct InterpreterResult {
  std::unique_ptr<slang::ast::Compilation> compilation;
  std::unique_ptr<SimulationContext> context;
  std::shared_ptr<lir::LirContext> lir_context;
  std::shared_ptr<HierarchyContext> top_instance;
  std::shared_ptr<slang::SourceManager> source_manager;

  [[nodiscard]] auto ReadVariable(const std::string& name) const
      -> RuntimeValue {
    return context->variable_store.ReadFromName(name, context->symbol_table);
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
