#pragma once

#include <memory>
#include <vector>

#include "mir/statement.hpp"

namespace lyra::mir {

enum class ProcessKind { kInitial, kAlwaysFF, kAlwaysComb };

class Process {
 public:
  ProcessKind kind;
  std::vector<std::shared_ptr<Statement>> body;
};

}  // namespace lyra::mir
