#pragma once

#include <cstdint>

namespace lyra::common {

// Edge types for event-driven scheduling (@posedge, @negedge, @*).
enum class EdgeKind : uint8_t {
  kPosedge,
  kNegedge,
  kAnyChange,
};

}  // namespace lyra::common
