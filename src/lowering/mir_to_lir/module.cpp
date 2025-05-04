#include "lowering/mir_to_lir/module.hpp"

#include <stdexcept>

#include "lir/module.hpp"
#include "lowering/mir_to_lir/lir_builder.hpp"
#include "lowering/mir_to_lir/process.hpp"
#include "mir/module.hpp"

namespace lyra::lowering {

auto LowerModule(const mir::Module& module) -> std::unique_ptr<lir::Module> {
  if (module.name.empty()) {
    throw std::runtime_error("Module has empty name");
  }

  LirBuilder builder(module.name);

  // Add variables
  for (const auto& variable : module.variables) {
    if (variable.name.empty()) {
      throw std::runtime_error("Variable has empty name");
    }
    builder.AddVariable(variable);
  }

  // Process each process
  for (const auto& process : module.processes) {
    if (!process) {
      throw std::runtime_error("Null process pointer");
    }

    LowerProcess(*process, builder);
  }

  return builder.Build();
}
}  // namespace lyra::lowering
