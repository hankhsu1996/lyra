#pragma once

#include <variant>

#include "lyra/hir/loop_var.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/structural_hops.hpp"
#include "lyra/hir/structural_var.hpp"

namespace lyra::hir {

struct StructuralVarRef {
  StructuralHops hops;
  StructuralVarId var;

  auto operator==(const StructuralVarRef&) const -> bool = default;
};

struct ProceduralVarRef {
  ProceduralVarId var;

  auto operator==(const ProceduralVarRef&) const -> bool = default;
};

struct LoopVarRef {
  StructuralHops hops;
  LoopVarDeclId loop_var;

  auto operator==(const LoopVarRef&) const -> bool = default;
};

using ValueRef = std::variant<StructuralVarRef, ProceduralVarRef, LoopVarRef>;

}  // namespace lyra::hir
