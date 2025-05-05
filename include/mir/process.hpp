#pragma once

#include <memory>
#include <vector>

#include "common/trigger.hpp"
#include "mir/statement.hpp"

namespace lyra::mir {

using Trigger = common::Trigger<std::string>;

enum class ProcessKind { kInitial, kAlways };

class Process {
 public:
  ProcessKind process_kind;
  std::unique_ptr<Statement> body;
  std::vector<Trigger> trigger_list;
};

}  // namespace lyra::mir
