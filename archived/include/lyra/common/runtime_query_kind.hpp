#pragma once

#include <cstdint>

namespace lyra {

enum class RuntimeQueryKind : uint8_t {
  kTimeRawTicks,  // Returns simulation time in internal ticks (uint64)
};

}  // namespace lyra
