#include "lyra/lowering/mir_to_lir/module.hpp"

#include <cassert>
#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/module.hpp"
#include "lyra/lowering/mir_to_lir/context.hpp"
#include "lyra/lowering/mir_to_lir/expression/expression.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/lowering/mir_to_lir/process.hpp"
#include "lyra/lowering/mir_to_lir/statement/statement.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/package.hpp"

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
  throw common::InternalError("mir_to_lir", "unknown port direction");
}

// Extract symbol ID from a port connection signal expression.
// For simple identifier expressions, returns the symbol ID directly.
auto ExtractSignalSymbol(const mir::Expression& expr) -> common::SymbolId {
  if (expr.kind == mir::Expression::Kind::kIdentifier) {
    return mir::As<mir::IdentifierExpression>(expr).symbol;
  }
  // For now, only support simple identifier connections
  throw common::InternalError(
      "mir_to_lir", "unsupported port connection expression type");
}

}  // namespace

auto LowerModule(
    const mir::Module& module, const common::SymbolTable& symbol_table,
    std::optional<int8_t> global_precision) -> std::unique_ptr<lir::Module> {
  if (module.name.empty()) {
    throw common::InternalError("mir_to_lir", "module has empty name");
  }

  // Use provided global precision, or compute from this module alone
  int8_t actual_precision = global_precision.value_or(
      module.timescale ? module.timescale->precision_power
                       : common::TimeScale::kDefaultPrecisionPower);

  auto lir_context = std::make_shared<lir::LirContext>();
  LirBuilder builder(module.name, lir_context, symbol_table);
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
        const auto* pointee =
            builder.GetContext()->InternType(mod_var.variable.type);
        auto ptr = builder.AllocateTemp("ptr", common::Type::Pointer(pointee));
        builder.AddInstruction(
            lir::Instruction::ResolveVar(ptr, mod_var.variable.symbol));
        builder.AddInstruction(lir::Instruction::Store(ptr, result));
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
    lir_submod.instance_symbol = submod.instance_symbol;
    lir_submod.instance_name = submod.instance_name;
    lir_submod.module_type = submod.module_type;
    lir_submod.module_signature = submod.module_signature;

    for (const auto& binding : submod.output_bindings) {
      lir::OutputBinding lir_binding;
      lir_binding.port_name = binding.port_name;
      lir_binding.signal = ExtractSignalSymbol(*binding.signal);
      lir_submod.output_bindings.push_back(std::move(lir_binding));
    }

    builder.AddSubmodule(lir_submod);
  }

  // Lower user-defined functions
  // Note: We collect lowered functions first, then add to module after
  // EndModule
  std::vector<lir::Function> lowered_functions;
  for (const auto& mir_func : module.functions) {
    lir::Function lir_func;
    lir_func.name = mir_func.name;
    lir_func.return_type = mir_func.return_type;

    // Copy parameters
    for (const auto& param : mir_func.parameters) {
      lir_func.parameters.push_back(lir::FunctionParameter{param.variable});
    }

    // Copy local variables
    lir_func.local_variables = mir_func.local_variables;

    // Lower function body to basic blocks
    builder.BeginFunction(mir_func.name, lir_func.temps);

    auto entry_label = builder.MakeLabel("entry");
    builder.StartBlock(entry_label);

    if (mir_func.body) {
      LowerStatement(*mir_func.body, builder, lowering_context);
    }

    builder.EndFunction();

    lir_func.blocks = builder.TakeFunctionBlocks();
    if (!lir_func.blocks.empty()) {
      lir_func.entry_label = lir_func.blocks.front()->label;
    }

    lowered_functions.push_back(std::move(lir_func));
  }

  auto lir_module = builder.EndModule();

  // Add lowered functions to the module
  for (auto& func : lowered_functions) {
    lir_module->functions.push_back(std::move(func));
  }

  // Set timescale info on the LIR module for runtime use
  lir_module->timescale = module.timescale;
  lir_module->global_precision_power = actual_precision;
  lir_module->signature = module.signature;
  lir_module->instance_symbol = module.instance_symbol;

  return lir_module;
}

auto LowerPackages(
    std::span<const std::unique_ptr<mir::Package>> packages,
    const common::SymbolTable& symbol_table) -> PackageLoweringResult {
  PackageLoweringResult result;
  result.context = std::make_shared<lir::LirContext>();

  if (packages.empty()) {
    return result;
  }

  // Use a single builder for all package lowering
  LirBuilder builder("__packages", result.context, symbol_table);
  builder.BeginModule();

  LoweringContext lowering_context;

  // Check if any packages have variable initializers
  bool has_initializers = false;
  for (const auto& pkg : packages) {
    for (const auto& var : pkg->variables) {
      if (var.initializer) {
        has_initializers = true;
        break;
      }
    }
    if (has_initializers) {
      break;
    }
  }

  // Lower variable initializers into a single init process
  if (has_initializers) {
    builder.BeginProcess("__pkg_var_init");
    builder.StartBlock(builder.MakeLabel("entry"));

    for (const auto& pkg : packages) {
      for (const auto& var : pkg->variables) {
        if (var.initializer) {
          auto init_result = LowerExpression(*var.initializer, builder);
          const auto* pointee =
              builder.GetContext()->InternType(var.variable.type);
          auto ptr =
              builder.AllocateTemp("ptr", common::Type::Pointer(pointee));
          builder.AddInstruction(
              lir::Instruction::ResolveVar(ptr, var.variable.symbol));
          builder.AddInstruction(lir::Instruction::Store(ptr, init_result));
        }
      }
    }

    builder.EndProcess();
  }

  // Lower all package functions
  for (const auto& pkg : packages) {
    for (const auto& mir_func : pkg->functions) {
      auto lir_func = std::make_unique<lir::Function>();

      // Use qualified name: "PkgName::FuncName"
      lir_func->name = pkg->name + "::" + mir_func.name;
      lir_func->return_type = mir_func.return_type;

      // Copy parameters
      for (const auto& param : mir_func.parameters) {
        lir_func->parameters.push_back(lir::FunctionParameter{param.variable});
      }

      // Copy local variables
      lir_func->local_variables = mir_func.local_variables;

      // Lower function body to basic blocks
      builder.BeginFunction(lir_func->name, lir_func->temps);

      auto entry_label = builder.MakeLabel("entry");
      builder.StartBlock(entry_label);

      if (mir_func.body) {
        LowerStatement(*mir_func.body, builder, lowering_context);
      }

      builder.EndFunction();

      lir_func->blocks = builder.TakeFunctionBlocks();
      if (!lir_func->blocks.empty()) {
        lir_func->entry_label = lir_func->blocks.front()->label;
      }

      result.functions.push_back(std::move(lir_func));
    }
  }

  auto module = builder.EndModule();

  // Extract init process if created
  if (has_initializers && !module->processes.empty()) {
    result.init_process = module->processes[0];
  }

  return result;
}

}  // namespace lyra::lowering::mir_to_lir
