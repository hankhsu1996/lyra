#include "lyra/lowering/ast_to_hir/continuous_assign.hpp"

#include <expected>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/expressions/AssignmentExpressions.h>
#include <slang/ast/symbols/MemberSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/sensitivity.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

auto BuildContinuousAssign(
    ModuleLowerer& module, WalkFrame frame, diag::SourceSpan span,
    hir::Expr lhs, hir::Expr rhs, const std::vector<SensitivityRead>& reads)
    -> hir::ContinuousAssign {
  const hir::ExprId lhs_id = frame.Exprs().Add(std::move(lhs));
  const hir::ExprId rhs_id = frame.Exprs().Add(std::move(rhs));
  return hir::ContinuousAssign{
      .span = span,
      .lhs = lhs_id,
      .rhs = rhs_id,
      .sensitivity_list = module.TranslateSensitivityReads(reads, frame),
  };
}

auto StructuralScopeLowerer::LowerContinuousAssign(
    const slang::ast::ContinuousAssignSymbol& sym, WalkFrame frame)
    -> diag::Result<hir::ContinuousAssign> {
  const auto& mapper = module_->SourceMapper();
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
        "StructuralScopeLowerer::LowerContinuousAssign: ContinuousAssignSymbol."
        "getAssignment() did not return an AssignmentExpression");
  }
  const auto& assign = assignment_expr.as<slang::ast::AssignmentExpression>();

  // Structural (continuous-assign) context: structural-var targets only.
  auto validate_lhs = ValidateAssignableStructural(assign.left());
  if (!validate_lhs) {
    return std::unexpected(std::move(validate_lhs.error()));
  }
  auto lhs_or = LowerExpr(assign.left(), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));

  auto rhs_or = LowerExpr(assign.right(), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));

  // LRM 10.3.2: continuous assignment sensitivity is the read set of the
  // RHS expression. slang treats the ContinuousAssignSymbol as the
  // procedural scope for analysis purposes.
  const auto& reads = module_->Sensitivity().AnalyzeReads(assignment_expr, sym);

  return BuildContinuousAssign(
      *module_, frame, span, *std::move(lhs_or), *std::move(rhs_or), reads);
}

}  // namespace lyra::lowering::ast_to_hir
