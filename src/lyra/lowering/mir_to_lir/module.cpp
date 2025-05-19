#include "lyra/lowering/mir_to_lir/module.hpp"

#include <stdexcept>

#include "lyra/lir/module.hpp"
#include "lyra/lowering/mir_to_lir/context.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/lowering/mir_to_lir/process.hpp"
#include "lyra/mir/module.hpp"

namespace lyra::lowering::mir_to_lir {

auto LowerModule(const mir::Module& module) -> std::unique_ptr<lir::Module> {
  if (module.name.empty()) {
    throw std::runtime_error("Module has empty name");
  }

  auto lir_context = std::make_shared<lir::LirContext>();
  LirBuilder builder(module.name, lir_context);
  LoweringContext lowering_context;

  builder.BeginModule();

  for (const auto& variable : module.variables) {
    builder.AddModuleVariable(variable);
  }

  for (const auto& process : module.processes) {
    assert(process);
    LowerProcess(*process, builder, lowering_context);
  }

  return builder.EndModule();
}

}  // namespace lyra::lowering::mir_to_lir
