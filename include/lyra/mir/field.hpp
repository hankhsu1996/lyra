#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct FieldId {
  std::uint32_t value;

  auto operator<=>(const FieldId&) const -> std::strong_ordering = default;
};

// A declaration carries name and type only. LRM 10.5 variable initialization
// (the user-supplied `= value` or the LRM Table 6-7 type default) lowers to
// an `AssignExpr` statement in the enclosing class's constructor body; every
// backend renders construction-time state from that statement list.
struct FieldDecl {
  std::string name;
  TypeId type;
};

// One field of a construction: which field (`target`, a stable `FieldId`)
// receives which value (`value`). The vocabulary every field-bearing
// construction states its initializers in. The value is a pure read of an
// already-materialized source, so the order of these entries is the
// source-semantic evaluation order, independent of a declaration's field order.
struct FieldInit {
  FieldId target;
  ExprId value;
};

}  // namespace lyra::mir
