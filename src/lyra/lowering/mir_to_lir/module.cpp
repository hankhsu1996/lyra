#include "lyra/lowering/mir_to_lir/module.hpp"

#include <cassert>
#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/module.hpp"
#include "lyra/lowering/mir_to_lir/context.hpp"
#include "lyra/lowering/mir_to_lir/expression.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/lowering/mir_to_lir/process.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::mir_to_lir {

namespace {

auto MapPortDirection(mir::PortDirection dir) -> lir::PortDirection {
  switch (dir) {
    case mir::PortDirection::kInput:
      return lir::PortDirection::kInput;
    case mir::PortDirection::kOutput:
      return lir::PortDirection::kOutput;
    case mir::PortDirection::kInout:
      return lir::PortDirection::kInout;
  }
  std::abort();
}

// Extract symbol reference from a port connection signal expression.
// For simple identifier expressions, returns the symbol directly.
auto ExtractSignalSymbol(const mir::Expression& expr) -> common::SymbolRef {
  if (expr.kind == mir::Expression::Kind::kIdentifier) {
    return mir::As<mir::IdentifierExpression>(expr).symbol;
  }
  // For now, only support simple identifier connections
  throw DiagnosticException(
      Diagnostic::Error({}, "unsupported port connection expression type"));
}

}  // namespace

auto LowerModule(
    const mir::Module& module, std::optional<int8_t> global_precision)
    -> std::unique_ptr<lir::Module> {
  if (module.name.empty()) {
    throw DiagnosticException(Diagnostic::Error({}, "module has empty name"));
  }

  // Use provided global precision, or compute from this module alone
  int8_t actual_precision = global_precision.value_or(
      module.timescale ? module.timescale->precision_power
                       : common::TimeScale::kDefaultPrecisionPower);

  auto lir_context = std::make_shared<lir::LirContext>();
  LirBuilder builder(module.name, lir_context);
  LoweringContext lowering_context;

  // Set timescale context for delay scaling
  lowering_context.SetTimescale(module.timescale, actual_precision);

  builder.BeginModule();

  // Register ports (also as module variables for simulation)
  for (const auto& port : module.ports) {
    builder.AddPort(port.variable, MapPortDirection(port.direction));
  }

  // First pass: register all module variables
  for (const auto& mod_var : module.variables) {
    builder.AddModuleVariable(mod_var.variable);
  }

  // Create synthetic init process for variables with initializers
  bool has_initializers = false;
  for (const auto& mod_var : module.variables) {
    if (mod_var.initializer) {
      has_initializers = true;
      break;
    }
  }

  if (has_initializers) {
    builder.BeginProcess("__var_init");
    builder.StartBlock(builder.MakeLabel("entry"));

    for (const auto& mod_var : module.variables) {
      if (mod_var.initializer) {
        auto result = LowerExpression(*mod_var.initializer, builder);
        auto store_instr = lir::Instruction::StoreVariable(
            mod_var.variable.symbol, result, false);
        builder.AddInstruction(std::move(store_instr));
      }
    }

    builder.EndProcess();
  }

  // Lower user processes
  for (const auto& process : module.processes) {
    assert(process);
    LowerProcess(*process, builder, lowering_context);
  }

  // Carry over submodule instances
  for (const auto& submod : module.submodules) {
    lir::SubmoduleInstance lir_submod;
    lir_submod.instance_name = submod.instance_name;
    lir_submod.module_type = submod.module_type;

    for (const auto& binding : submod.output_bindings) {
      lir::OutputBinding lir_binding;
      lir_binding.port_name = binding.port_name;
      lir_binding.signal = ExtractSignalSymbol(*binding.signal);
      lir_submod.output_bindings.push_back(std::move(lir_binding));
    }

    builder.AddSubmodule(lir_submod);
  }

  auto lir_module = builder.EndModule();

  // Set timescale info on the LIR module for runtime use
  lir_module->timescale = module.timescale;
  lir_module->global_precision_power = actual_precision;

  return lir_module;
}

auto LowerModules(std::span<const std::unique_ptr<mir::Module>> modules)
    -> std::vector<std::unique_ptr<lir::Module>> {
  // Phase 1: Collect timescales from all modules
  std::vector<std::optional<common::TimeScale>> timescales;
  timescales.reserve(modules.size());
  for (const auto& module : modules) {
    timescales.push_back(module->timescale);
  }

  // Phase 2: Compute global precision as finest among all modules
  int8_t global_precision = common::ComputeGlobalPrecision(timescales);

  // Phase 3: Lower each module with shared global precision
  std::vector<std::unique_ptr<lir::Module>> result;
  result.reserve(modules.size());
  for (const auto& module : modules) {
    result.push_back(LowerModule(*module, global_precision));
  }
  return result;
}

}  // namespace lyra::lowering::mir_to_lir
