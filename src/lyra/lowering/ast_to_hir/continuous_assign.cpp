#include "lyra/hir/continuous_assign.hpp"

#include <expected>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/symbols/MemberSymbols.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Assembles a continuous assignment (LRM 10.3.2) from its two already-built
// operand expressions and the read set its sensitivity derives from.
auto BuildContinuousAssign(
    UnitLowerer& unit_lowerer, WalkFrame frame, diag::SourceSpan span,
    hir::Expr lhs, hir::Expr rhs, const std::vector<SensitivityRead>& reads)
    -> diag::Result<hir::ContinuousAssign> {
  auto sensitivity = unit_lowerer.TranslateSensitivityReads(reads, frame);
  if (!sensitivity) return std::unexpected(std::move(sensitivity.error()));
  const hir::ExprId lhs_id = frame.Exprs().Add(std::move(lhs));
  const hir::ExprId rhs_id = frame.Exprs().Add(std::move(rhs));
  return hir::ContinuousAssign{
      .span = span,
      .lhs = lhs_id,
      .rhs = rhs_id,
      .sensitivity_list = *std::move(sensitivity),
  };
}

}  // namespace

auto StructuralScopeLowerer::LowerContinuousAssign(
    const slang::ast::ContinuousAssignSymbol& sym, WalkFrame frame)
    -> diag::Result<hir::ContinuousAssign> {
  const auto& mapper = owner_->SourceMapper();
  const auto span = mapper.PointSpanOf(sym.location);

  if (sym.getDelay() != nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedContinuousAssignForm,
        "delay on continuous assignment is not yet supported");
  }
  const auto strength = sym.getDriveStrength();
  if (strength.first.has_value() || strength.second.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedContinuousAssignForm,
        "drive strength on continuous assignment is not yet supported");
  }

  const auto& assignment_expr = sym.getAssignment();
  if (assignment_expr.kind != slang::ast::ExpressionKind::Assignment) {
    // slang bound the continuous assignment to a form other than a plain
    // assignment (a legitimate construct Lyra does not lower yet), not a
    // compiler-invariant violation.
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedContinuousAssignForm,
        "this continuous-assignment form is not yet supported");
  }
  const auto& assign = assignment_expr.as<slang::ast::AssignmentExpression>();

  auto lhs_or = LowerExpr(assign.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));

  // LRM 7.5.1 admits `new[]` on the right-hand side of a variable declaration
  // assignment and of a blocking procedural assignment, and a continuous
  // assignment is neither: it states a value its target follows for the whole
  // run, which sizing an array once cannot be.
  if (assign.right().kind == slang::ast::ExpressionKind::NewArray) {
    return diag::Fail(
        mapper.SpanOf(assign.right().sourceRange),
        diag::DiagCode::kUnsupportedContinuousAssignForm,
        "sizing a dynamic array with new[] is not legal in a continuous "
        "assignment (LRM 7.5.1); size it where the array is declared or in a "
        "procedural assignment");
  }

  auto rhs_or = LowerExpr(assign.right(), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));

  // LRM 10.3.2: continuous assignment sensitivity is the read set of the
  // RHS expression. slang treats the ContinuousAssignSymbol as the
  // procedural scope for analysis purposes.
  const auto& reads = owner_->Sensitivity().AnalyzeReads(assignment_expr, sym);

  return BuildContinuousAssign(
      *owner_, frame, span, *std::move(lhs_or), *std::move(rhs_or), reads);
}

}  // namespace lyra::lowering::ast_to_hir
