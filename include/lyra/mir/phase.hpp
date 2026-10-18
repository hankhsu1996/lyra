#pragma once

#include <cstdint>

namespace lyra::mir {

// What a unit of MIR is allowed to hold narrows as it is rewritten, as rustc's
// `MirPhase` names the dialects its MIR passes through. Each phase is named by
// what reads a unit in it.
enum class Phase : std::uint8_t {
  // As HIR-to-MIR states it, which the rewrites between it and the backends
  // read.
  kBuilt,
  // As every backend reads it: no frame holds a value too large for one.
  kForBackends,
};

}  // namespace lyra::mir
