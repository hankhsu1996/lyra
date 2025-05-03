#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/trigger.hpp"
#include "core/execution_context.hpp"
#include "lir/module.hpp"
#include "lir/process.hpp"

namespace lyra {

using VariableName = std::string;
using VariableTriggerList = std::vector<
    std::pair<common::Trigger<VariableName>, std::shared_ptr<lir::Process>>>;
using VariableTriggerMap =
    std::unordered_map<VariableName, VariableTriggerList>;

class SimulationPreparation {
 public:
  // Build a map from variable names to their triggers
  static auto BuildVariableTriggerMap(const lir::Module& module)
      -> VariableTriggerMap;

  // Initialize all declared variables in the module into the execution context
  static void InitializeVariables(
      const lir::Module& module, ExecutionContext& context);
};

}  // namespace lyra
