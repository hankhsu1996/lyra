#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"

#include <expected>
#include <utility>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/inside_item.hpp"
#include "lyra/lowering/hir_to_mir/class_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

template <ExprLowerer Lowerer>
auto BuildHirInsideItemPredicate(
    Lowerer& lowerer, WalkFrame frame, mir::ExprId lhs_id,
    const hir::InsideItem& item, mir::TypeId result_type)
    -> diag::Result<mir::ExprId> {
  const auto& hir_exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  auto lower_id = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = lowerer.LowerExpr(hir_exprs.Get(id), frame);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    return block.exprs.Add(*std::move(lowered));
  };

  return std::visit(
      Overloaded{
          [&](const hir::ExprId& val_id) -> diag::Result<mir::ExprId> {
            auto v = lower_id(val_id);
            if (!v) return std::unexpected(std::move(v.error()));
            return block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kWildcardEquality,
                            .lhs = lhs_id,
                            .rhs = *v},
                    .type = result_type});
          },
          [&](const hir::InsideRangePair& r) -> diag::Result<mir::ExprId> {
            auto lo = lower_id(r.lo);
            if (!lo) return std::unexpected(std::move(lo.error()));
            auto hi = lower_id(r.hi);
            if (!hi) return std::unexpected(std::move(hi.error()));
            const mir::ExprId ge_id = block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kGreaterEqual,
                            .lhs = lhs_id,
                            .rhs = *lo},
                    .type = result_type});
            const mir::ExprId le_id = block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kLessEqual,
                            .lhs = lhs_id,
                            .rhs = *hi},
                    .type = result_type});
            return block.exprs.Add(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kLogicalAnd,
                            .lhs = ge_id,
                            .rhs = le_id},
                    .type = result_type});
          },
      },
      item);
}

template auto BuildHirInsideItemPredicate(
    ProcessLowerer&, WalkFrame, mir::ExprId, const hir::InsideItem&,
    mir::TypeId) -> diag::Result<mir::ExprId>;
template auto BuildHirInsideItemPredicate(
    const ClassLowerer&, WalkFrame, mir::ExprId, const hir::InsideItem&,
    mir::TypeId) -> diag::Result<mir::ExprId>;

}  // namespace lyra::lowering::hir_to_mir
