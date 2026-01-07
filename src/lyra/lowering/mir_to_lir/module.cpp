#include "lyra/lowering/mir_to_lir/module.hpp"

#include <cassert>
#include <memory>
#include <utility>

#include "lyra/common/diagnostic.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/module.hpp"
#include "lyra/lowering/mir_to_lir/context.hpp"
#include "lyra/lowering/mir_to_lir/expression.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/lowering/mir_to_lir/process.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::mir_to_lir {

auto LowerModule(const mir::Module& module) -> std::unique_ptr<lir::Module> {
  if (module.name.empty()) {
    throw DiagnosticException(Diagnostic::Error({}, "module has empty name"));
  }

  // Compute global precision (for single module, use module's precision or
  // default)
  int8_t global_precision = common::TimeScale::kDefaultPrecisionPower;
  if (module.timescale) {
    global_precision = module.timescale->precision_power;
  }

  auto lir_context = std::make_shared<lir::LirContext>();
  LirBuilder builder(module.name, lir_context);
  LoweringContext lowering_context;

  // Set timescale context for delay scaling
  lowering_context.SetTimescale(module.timescale, global_precision);

  builder.BeginModule();

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

  auto lir_module = builder.EndModule();

  // Set timescale info on the LIR module for runtime use
  lir_module->timescale = module.timescale;
  lir_module->global_precision_power = global_precision;

  return lir_module;
}

}  // namespace lyra::lowering::mir_to_lir
