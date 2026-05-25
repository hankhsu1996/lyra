#pragma once

#include <variant>

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

using Lvalue = std::variant<StructuralVarRef, ProceduralVarRef>;

}  // namespace lyra::mir
