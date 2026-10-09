#pragma once

#include <compare>
#include <cstdint>
#include <vector>

#include "lyra/base/pool_id.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/timing.hpp"
#include "lyra/support/strength_level.hpp"

namespace lyra::hir {

struct ContinuousAssignId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ContinuousAssignId&) const
      -> std::strong_ordering = default;
};

// LRM 10.3 `assign lhs = rhs;` -- source-aligned with slang's
// `ContinuousAssignSymbol` at scope level. Both `lhs` and `rhs` are ExprIds
// into the containing `StructuralScope.exprs` pool, matching how every other
// scope-level expression (parameter values, variable initialisers) is stored.
// The `lhs` form is restricted to a structural-var-rooted addressable
// expression. It is evaluated once at time zero and again whenever something
// in `sensitivity_list` changes (LRM 10.3.2), the same model as an
// `always_comb` (LRM 9.2.2.2.1).
struct ContinuousAssign {
  diag::SourceSpan span;
  ExprId lhs;
  ExprId rhs;
  support::StrengthLevel strength;
  std::vector<SensitivityEntry> sensitivity_list;

  auto operator==(const ContinuousAssign&) const -> bool = default;
};

}  // namespace lyra::hir
