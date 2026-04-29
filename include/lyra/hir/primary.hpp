#pragma once

#include <string>
#include <variant>

#include "lyra/hir/conversion.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/value_ref.hpp"

namespace lyra::hir {

enum class TimeScale : std::uint8_t { kFs, kPs, kNs, kUs, kMs, kS };

struct IntegerLiteral {
  IntegralConstant value;
  IntegerLiteralBase base = IntegerLiteralBase::kDecimal;
  bool declared_unsized = false;
};

struct StringLiteral {
  std::string value;
};

struct TimeLiteral {
  double value;
  TimeScale scale;
};

// Per IEEE 1800 SystemVerilog grammar, `primary` includes both
// `primary_literal` (e.g. integer literals) and `hierarchical_identifier
// select` (i.e. value references). Grouping them under Primary keeps the
// HIR's expression tree aligned with the LRM grammar.
struct RefExpr {
  ValueRef target;
};

using Primary =
    std::variant<IntegerLiteral, StringLiteral, TimeLiteral, RefExpr>;

}  // namespace lyra::hir
