#pragma once

#include <memory>
#include <vector>

#include "common/trigger.hpp"
#include "mir/statement.hpp"
#include "mir/variable.hpp"

namespace lyra::mir {

using Trigger = common::Trigger<Variable>;

enum class ProcessKind { kInitial, kAlwaysFF, kAlwaysComb };

class Process {
 public:
  ProcessKind process_kind;
  std::vector<std::unique_ptr<Statement>> body;
  std::vector<Trigger> trigger_list;
};

}  // namespace lyra::mir
