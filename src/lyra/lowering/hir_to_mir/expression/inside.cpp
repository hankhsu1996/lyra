#include "lyra/lowering/hir_to_mir/expression/inside.hpp"

#include <expected>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

template <ExprLowerer Lowerer>
auto LowerHirInsideExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::InsideExpr& in,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_exprs = lowerer.HirExprs();
  if (in.items.empty()) {
    throw InternalError(
        "LowerHirInsideExpr: hir::InsideExpr has empty item list");
  }

  // The left operand is one operand of the operator, compared with every
  // member of the set (LRM 11.4.13), so it is evaluated once and each
  // comparison reads that. Evaluating it and comparing are the steps of one
  // block expression, so it is evaluated where the operator is written.
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  auto lhs_or = lowerer.LowerExpr(hir_exprs.Get(in.lhs), steps.Frame());
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id =
      EvaluatedOnce(steps.Frame(), body.exprs.Add(*std::move(lhs_or)));

  std::vector<mir::ExprId> tests;
  tests.reserve(in.items.size());
  for (const auto& item : in.items) {
    auto pred_or =
        BuildSetMemberTest(lowerer, steps.Frame(), lhs_id, item, result_type);
    if (!pred_or) return std::unexpected(std::move(pred_or.error()));
    tests.push_back(*pred_or);
  }
  return steps.Build(
      BuildMirLogicalOr(lowerer.Owner().Unit(), body, result_type, tests));
}

template auto LowerHirInsideExpr(
    ProcessLowerer&, WalkFrame, const hir::InsideExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirInsideExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::InsideExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
