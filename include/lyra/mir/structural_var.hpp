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
  // By-name lookup key; differs from the unique physical `name` only for a
  // generate companion, where same-name if/else arms (LRM 27.5) share one key.
  // Empty when it equals `name`.
  std::string source_name = {};
  TypeId type;
  ExprId initializer;
};

}  // namespace lyra::mir
