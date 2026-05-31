#include "lyra/lowering/ast_to_hir/continuous_assign.hpp"

#include <expected>
#include <utility>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/lowering/ast_to_hir/expression/lower.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerContinuousAssign(
    const UnitLoweringFacts& unit_facts, ScopeLoweringState& scope_state,
    ScopeStack& stack, const slang::ast::ContinuousAssignSymbol& sym)
    -> diag::Result<hir::ContinuousAssign> {
  const auto& mapper = unit_facts.SourceMapper();
  const auto span = mapper.PointSpanOf(sym.location);

  if (sym.getDelay() != nullptr) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedContinuousAssignForm,
        "delay on continuous assignment is not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  const auto strength = sym.getDriveStrength();
  if (strength.first.has_value() || strength.second.has_value()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedContinuousAssignForm,
        "drive strength on continuous assignment is not yet supported",
        diag::UnsupportedCategory::kFeature);
  }

  const auto& assignment_expr = sym.getAssignment();
  if (assignment_expr.kind != slang::ast::ExpressionKind::Assignment) {
    throw InternalError(
        "LowerContinuousAssign: ContinuousAssignSymbol.getAssignment() did not "
        "return an AssignmentExpression");
  }
  const auto& assign = assignment_expr.as<slang::ast::AssignmentExpression>();

  // Null proc_state -> scope (continuous-assign) context: structural-var
  // targets only.
  auto validate_lhs = ValidateAssignableSlangExpr(
      unit_facts, scope_state.UnitState(), nullptr, assign.left());
  if (!validate_lhs) {
    return std::unexpected(std::move(validate_lhs.error()));
  }
  auto lhs_or = LowerStructuralExpr(
      unit_facts, scope_state.UnitState(), scope_state, stack, assign.left());
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const hir::ExprId lhs_id = scope_state.AddExpr(*std::move(lhs_or));

  auto rhs_or = LowerStructuralExpr(
      unit_facts, scope_state.UnitState(), scope_state, stack, assign.right());
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const hir::ExprId rhs_id = scope_state.AddExpr(*std::move(rhs_or));

  // Sensitivity is precomputed by slang's flow analysis on the assignment
  // expression during `BuildSensitivityReadStore` (see
  // `docs/decisions/read-set-inference.md`). Look up keyed by the
  // AssignmentExpression -- the same Expression-keyed bucket future
  // per-expression analyses (property / assert / wait_order events) use.
  std::vector<hir::SensitivityEntry> sensitivity;
  if (const auto* reads = unit_facts.SensitivityReads().Lookup(assign);
      reads != nullptr) {
    sensitivity =
        TranslateSensitivityReads(*reads, scope_state.UnitState(), stack);
  }

  return hir::ContinuousAssign{
      .span = span,
      .lhs = lhs_id,
      .rhs = rhs_id,
      .sensitivity_list = std::move(sensitivity),
  };
}

}  // namespace lyra::lowering::ast_to_hir
