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

// Lvalue names a storage location. Same shape is used in both
// assignment-target position (directly) and read position (wrapped in
// LvalueRead). Validity of LoopVarRef as a write target depends on context
// and is enforced at HIR-to-MIR (only the for-loop induction header may
// write a loop variable).
using Lvalue = std::variant<StructuralVarRef, ProceduralVarRef, LoopVarRef>;

}  // namespace lyra::hir
