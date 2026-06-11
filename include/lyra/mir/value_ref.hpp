#pragma once

#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/structural_hops.hpp"
#include "lyra/mir/structural_var.hpp"

namespace lyra::mir {

struct StructuralVarRef {
  StructuralHops hops;
  StructuralVarId var{};
};

struct ProceduralVarRef {
  ProceduralHops hops;
  ProceduralVarId var{};
};

// A sensitivity leaf observes a structural var that resolves to a stored
// `Observable` the scheduler subscribes to: a plain signal, an upward
// ExternalRef member, or a downward borrowed-pointer slot. All three are
// StructuralVarRefs; the var's type tells the renderer how to reach the cell.
using SensitivityRef = StructuralVarRef;

}  // namespace lyra::mir
