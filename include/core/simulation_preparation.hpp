#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/trigger.hpp"
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
  static auto BuildVariableTriggerMap(const lir::Module& module)
      -> VariableTriggerMap;
};

}  // namespace lyra
