#pragma once

#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/constraint_id.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/procedural_var.hpp"

namespace lyra::hir {

// A constraint (LRM 18.5) as the source wrote it, inside a constraint block or
// the block a `randomize() with` call writes (LRM 18.7). Each alternative is
// one form of `constraint_expression`; its expressions live in the arena of
// the body that wrote it, beside its other expressions.

// Several constraints together: a block's body, or a `{ ... }` set.
struct ConstraintList {
  std::vector<ConstraintId> items;

  auto operator==(const ConstraintList&) const -> bool = default;
};

// An expression that holds (LRM 18.5), where it can for a `soft` one (LRM
// 18.5.13).
struct ExpressionConstraint {
  ExprId condition;
  bool is_soft = false;

  auto operator==(const ExpressionConstraint&) const -> bool = default;
};

// One item of a distribution (LRM 18.5.3): a value or a range `[low:high]`,
// and its weight, which `:=` gives each value and `:/` shares across the
// range. An item written with no weight weighs 1 per value.
struct DistItem {
  ExprId low;
  std::optional<ExprId> high;
  std::optional<ExprId> weight;
  bool weight_per_range = false;

  auto operator==(const DistItem&) const -> bool = default;
};

// `subject dist { items }` (LRM 18.5.3): the subject is one of the items'
// values, chosen with the weights given, where it can be for a `soft` one.
// `default :/ weight` gives every value no item names the weight shared
// across them, absent where the source writes none.
struct DistConstraint {
  ExprId subject;
  std::vector<DistItem> items;
  std::optional<ExprId> default_weight;
  bool is_soft = false;

  auto operator==(const DistConstraint&) const -> bool = default;
};

// `predicate -> body` (LRM 18.5.5).
struct ImplicationConstraint {
  ExprId predicate;
  ConstraintId body;

  auto operator==(const ImplicationConstraint&) const -> bool = default;
};

// `if (predicate) then_body else else_body` (LRM 18.5.6).
struct ConditionalConstraint {
  ExprId predicate;
  ConstraintId then_body;
  std::optional<ConstraintId> else_body;

  auto operator==(const ConditionalConstraint&) const -> bool = default;
};

// `unique { items }` (LRM 18.5.4): every value the items name, an array
// standing for each of its elements, differs from every other.
struct UniquenessConstraint {
  std::vector<ExprId> items;

  auto operator==(const UniquenessConstraint&) const -> bool = default;
};

// `foreach (array[loop_vars]) body` (LRM 18.5.7): `body` once for every index
// of the array, each loop variable bound as a `foreach` statement binds it
// (LRM 12.7.3).
struct ForeachConstraint {
  ExprId array;
  std::vector<std::optional<ProceduralVarId>> loop_vars;
  ConstraintId body;

  auto operator==(const ForeachConstraint&) const -> bool = default;
};

// `solve solve before after` (LRM 18.5.9).
struct SolveBeforeConstraint {
  std::vector<ExprId> solve;
  std::vector<ExprId> after;

  auto operator==(const SolveBeforeConstraint&) const -> bool = default;
};

// `disable soft target` (LRM 18.5.13.2).
struct DisableSoftConstraint {
  ExprId target;

  auto operator==(const DisableSoftConstraint&) const -> bool = default;
};

using ConstraintData = std::variant<
    ConstraintList, ExpressionConstraint, DistConstraint, ImplicationConstraint,
    ConditionalConstraint, UniquenessConstraint, ForeachConstraint,
    SolveBeforeConstraint, DisableSoftConstraint>;

struct Constraint {
  ConstraintData data;
  diag::SourceSpan span;

  auto operator==(const Constraint&) const -> bool = default;
};

}  // namespace lyra::hir
