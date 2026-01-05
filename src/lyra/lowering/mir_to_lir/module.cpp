#include "lyra/lowering/mir_to_lir/module.hpp"

#include "lyra/common/diagnostic.hpp"
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

  auto lir_context = std::make_shared<lir::LirContext>();
  LirBuilder builder(module.name, lir_context);
  LoweringContext lowering_context;

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

  return builder.EndModule();
}

}  // namespace lyra::lowering::mir_to_lir
