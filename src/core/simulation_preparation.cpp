#include "core/simulation_preparation.hpp"

namespace lyra {

auto SimulationPreparation::BuildVariableTriggerMap(const lir::Module& module)
    -> VariableTriggerMap {
  VariableTriggerMap trigger_map;

  for (const auto& process : module.processes) {
    for (const auto& trigger : process->trigger_list) {
      trigger_map[trigger.variable].emplace_back(trigger, process);
    }
  }

  return trigger_map;
}

void SimulationPreparation::InitializeVariables(
    const lir::Module& module, ExecutionContext& context) {
  for (const auto& variable : module.variables) {
    if (!context.variable_table.Exists(variable.name)) {
      switch (variable.type.kind) {
        case common::Type::Kind::kVoid:
          break;
        case common::Type::Kind::kTwoState: {
          auto two_state_data =
              std::get<common::TwoStateData>(variable.type.data);
          if (two_state_data.is_signed) {
            context.variable_table.CreateVariable(
                variable.name,
                RuntimeValue::TwoStateSigned(0, two_state_data.bit_width));
          } else {
            context.variable_table.CreateVariable(
                variable.name,
                RuntimeValue::TwoStateUnsigned(0, two_state_data.bit_width));
          }
          break;
        }
        case common::Type::Kind::kString:
          context.variable_table.CreateVariable(
              variable.name, RuntimeValue::String(""));
      }
    }
  }
}

}  // namespace lyra
