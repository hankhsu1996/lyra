#pragma once

#include <cstdint>
#include <variant>

#include "lyra/hir/value_ref.hpp"

namespace lyra::hir {

struct IntegerLiteral {
  std::int64_t value;
};

// Per IEEE 1800 SystemVerilog grammar, `primary` includes both
// `primary_literal` (e.g. integer literals) and `hierarchical_identifier
// select` (i.e. value references). Grouping them under Primary keeps the
// HIR's expression tree aligned with the LRM grammar.
struct RefExpr {
  ValueRef target;
};

using Primary = std::variant<IntegerLiteral, RefExpr>;

}  // namespace lyra::hir
