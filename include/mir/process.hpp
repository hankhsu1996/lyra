#pragma once

#include <memory>
#include <vector>

#include "mir/expression.hpp"
#include "mir/statement.hpp"

namespace lyra::mir {

enum class EdgeKind { kPosedge, kNegedge, kAnyEdge };

struct Trigger {
  EdgeKind edge_kind;
  std::shared_ptr<Expression> expression;
};

enum class ProcessKind { kInitial, kAlwaysFF, kAlwaysComb };

class Process {
 public:
  ProcessKind process_kind;
  std::vector<std::shared_ptr<Statement>> body;
  std::vector<Trigger> trigger_list;
};

}  // namespace lyra::mir
