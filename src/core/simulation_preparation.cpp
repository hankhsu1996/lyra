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

void SimulationPreparation::InitializeSignals(
    const lir::Module& module, ExecutionContext& context) {
  for (const auto& signal_name : module.signals) {
    if (!context.signalTable.Exists(signal_name)) {
      context.signalTable.CreateSignal(signal_name, RuntimeValue::FromInt(0));
    }
  }
}

}  // namespace lyra
