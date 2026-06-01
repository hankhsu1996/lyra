#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct StructuralVarId {
  std::uint32_t value;

  auto operator<=>(const StructuralVarId&) const
      -> std::strong_ordering = default;
};

// HIR distinguishes "no user initializer" from "explicit zero initializer";
// MIR does not -- the LRM Table 6-7 default is materialised as a primitive
// expression at the HIR-to-MIR boundary, so every variable carries one.
struct StructuralVarDecl {
  std::string name;
  TypeId type;
  ExprId initializer;
};

}  // namespace lyra::mir
