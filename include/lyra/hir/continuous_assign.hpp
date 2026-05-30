#pragma once

#include <compare>
#include <cstdint>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/lvalue.hpp"
#include "lyra/hir/stmt.hpp"

namespace lyra::hir {

struct ContinuousAssignId {
  std::uint32_t value;

  auto operator<=>(const ContinuousAssignId&) const
      -> std::strong_ordering = default;
};

// LRM 10.3 `assign lhs = rhs;` -- source-aligned with slang's
// `ContinuousAssignSymbol` at scope level. The LHS and RHS live in the
// containing `StructuralScope.exprs` pool (selector ExprIds in `lhs` and the
// `rhs` ExprId both index into it), matching how every other scope-level
// expression (parameter values, variable initialisers) is stored. HIR -> MIR
// translates `lhs` and `rhs` into a synthesised `mir::Process` whose body is
// `forever { lhs = rhs; SensitivityWaitStmt(sensitivity_list); }`, giving
// continuous assignment the same runtime mental model as always_comb
// (LRM 9.2.2.2.1).
struct ContinuousAssign {
  diag::SourceSpan span;
  Lvalue lhs;
  ExprId rhs;
  std::vector<SensitivityEntry> sensitivity_list;
};

}  // namespace lyra::hir
