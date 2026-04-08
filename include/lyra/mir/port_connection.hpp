#pragma once

#include <cstdint>

namespace lyra::mir {

// Port connection direction kind.
struct PortConnection {
  enum class Kind : int32_t {
    kDriveParentToChild,  // input: parent expr -> child port
    kDriveChildToParent,  // output: child -> parent
  };
};

}  // namespace lyra::mir
