#pragma once

#include <cstdint>

namespace lyra::mir {

// What a unit of MIR is allowed to hold narrows as it is converted, as rustc's
// `MirPhase` names the dialects its MIR passes through. Each phase after the
// first is named by the conversion that brought the unit there, as LLVM names
// a machine function's `Legalized` and `Selected` properties, and holds what
// that conversion leaves true.
enum class Phase : std::uint8_t {
  // As HIR-to-MIR states it.
  kBuilt,
  // Every construct is one the backends accept: the order an evaluation needs
  // is explicit, and no frame holds a value too large for one.
  kLegalized,
};

}  // namespace lyra::mir
