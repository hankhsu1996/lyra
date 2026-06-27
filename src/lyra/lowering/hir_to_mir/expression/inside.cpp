#include "lyra/lowering/hir_to_mir/expression/inside.hpp"

#include <expected>
#include <optional>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

template <ExprLowerer Lowerer>
auto LowerHirInsideExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::InsideExpr& in,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  auto lhs_or = lowerer.LowerExpr(hir_exprs.Get(in.lhs), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = block.exprs.Add(*std::move(lhs_or));

  if (in.items.empty()) {
    throw InternalError(
        "LowerHirInsideExpr: hir::InsideExpr has empty item list");
  }
  std::optional<mir::ExprId> acc;
  for (const auto& item : in.items) {
    auto pred_or =
        BuildHirInsideItemPredicate(lowerer, frame, lhs_id, item, result_type);
    if (!pred_or) return std::unexpected(std::move(pred_or.error()));
    if (acc.has_value()) {
      acc = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::BinaryExpr{
                      .op = mir::BinaryOp::kLogicalOr,
                      .lhs = *acc,
                      .rhs = *pred_or},
              .type = result_type});
    } else {
      acc = *pred_or;
    }
  }
  return mir::Expr{block.exprs.Get(*acc)};
}

template auto LowerHirInsideExpr(
    ProcessLowerer&, WalkFrame, const hir::InsideExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirInsideExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::InsideExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
