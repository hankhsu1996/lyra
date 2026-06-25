#pragma once

#include <compare>
#include <cstdint>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/stmt.hpp"

namespace lyra::hir {

struct ContinuousAssignId {
  std::uint32_t value;

  auto operator<=>(const ContinuousAssignId&) const
      -> std::strong_ordering = default;
};

// LRM 10.3 `assign lhs = rhs;` -- source-aligned with slang's
// `ContinuousAssignSymbol` at scope level. Both `lhs` and `rhs` are ExprIds
// into the containing `StructuralScope.exprs` pool, matching how every other
// scope-level expression (parameter values, variable initialisers) is stored.
// The `lhs` form is restricted to a structural-var-rooted addressable
// expression. HIR -> MIR translates this into a synthesised process body
// `forever { lhs = rhs; SensitivityWaitStmt(sensitivity_list); }`, registered
// as a startup activation, giving continuous assignment the same runtime
// mental model as always_comb (LRM 9.2.2.2.1).
struct ContinuousAssign {
  diag::SourceSpan span;
  ExprId lhs;
  ExprId rhs;
  std::vector<SensitivityEntry> sensitivity_list;
};

}  // namespace lyra::hir
