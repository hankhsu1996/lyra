#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct StructuralVarId {
  std::uint32_t value;

  auto operator<=>(const StructuralVarId&) const
      -> std::strong_ordering = default;
};

// A declaration carries name and type only. LRM 10.5 variable initialization
// (the user-supplied `= value` or the LRM Table 6-7 type default) lowers to
// an `AssignExpr` statement at the top of the enclosing scope's
// `constructor_scope.root_stmts`; every backend renders construction-time
// state from that single statement list.
struct StructuralVarDecl {
  std::string name;
  // By-name lookup key; differs from the unique physical `name` only for a
  // generate companion, where same-name if/else arms (LRM 27.5) share one key.
  // Empty when it equals `name`.
  std::string source_name = {};
  TypeId type;
};

}  // namespace lyra::mir
