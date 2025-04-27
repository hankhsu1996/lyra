#pragma once

#include <memory>

namespace lyra::common {

enum class EdgeKind { kPosedge, kNegedge, kAnyEdge };

template <typename VariableType>
struct Trigger {
  EdgeKind edge_kind;
  std::shared_ptr<VariableType> variable;
};

}  // namespace lyra::common
