#pragma once

namespace lyra::common {

enum class EdgeKind { kPosedge, kNegedge, kAnyEdge };

template <typename VariableType>
struct Trigger {
  EdgeKind edge_kind;
  VariableType variable;
};

}  // namespace lyra::common
