#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <variant>
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

// LRM 28.16 the delays a continuous assignment is written with, as many as the
// source gave (LRM 10.3.3). One is used for every change.
struct SingleDelay {
  ExprId amount;

  auto operator==(const SingleDelay&) const -> bool = default;
};

// A change to 1 takes the rise delay and to 0 the fall delay; to z or x the
// lesser of the two.
struct RiseFallDelay {
  ExprId rise;
  ExprId fall;

  auto operator==(const RiseFallDelay&) const -> bool = default;
};

// A change to 1, 0 or z takes the rise, fall or turn-off delay; to x the least
// of the three.
struct RiseFallTurnOffDelay {
  ExprId rise;
  ExprId fall;
  ExprId turn_off;

  auto operator==(const RiseFallTurnOffDelay&) const -> bool = default;
};

using AssignmentDelay =
    std::variant<SingleDelay, RiseFallDelay, RiseFallTurnOffDelay>;

// LRM 10.3 `assign lhs = rhs;` -- source-aligned with slang's
// `ContinuousAssignSymbol` at scope level. Both `lhs` and `rhs` are ExprIds
// into the containing `StructuralScope.exprs` pool, matching how every other
// scope-level expression (parameter values, variable initialisers) is stored.
// The `lhs` form is restricted to a structural-var-rooted addressable
// expression. It is evaluated once at time zero and again whenever something
// in `sensitivity_list` changes (LRM 10.3.2), the same model as an
// `always_comb` (LRM 9.2.2.2.1). Where it carries a delay, each value it
// computes reaches `lhs` that long after the change, unless a different value
// is computed first (LRM 10.3.3); an assignment carrying none assigns at once,
// which is not the same as a delay of zero.
struct ContinuousAssign {
  diag::SourceSpan span;
  ExprId lhs;
  ExprId rhs;
  support::StrengthLevel strength;
  std::optional<AssignmentDelay> delay;
  std::vector<SensitivityEntry> sensitivity_list;

  auto operator==(const ContinuousAssign&) const -> bool = default;
};

}  // namespace lyra::hir
