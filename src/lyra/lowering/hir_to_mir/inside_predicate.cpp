#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"

#include <expected>
#include <utility>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/inside_item.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildHirInsideItemPredicate(
    ProcessLowerer& proc, WalkFrame frame, mir::ExprId lhs_id,
    const hir::InsideItem& item, mir::TypeId result_type)
    -> diag::Result<mir::ExprId> {
  const auto& hir_body = proc.HirBody();
  auto& scope = *frame.current_procedural_scope;
  auto lower_id = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = LowerExpr(proc, frame, hir_body.exprs.at(id.value));
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    return scope.AddExpr(*std::move(lowered));
  };

  return std::visit(
      Overloaded{
          [&](const hir::ExprId& val_id) -> diag::Result<mir::ExprId> {
            auto v = lower_id(val_id);
            if (!v) return std::unexpected(std::move(v.error()));
            return scope.AddExpr(
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
            const mir::ExprId ge_id = scope.AddExpr(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kGreaterEqual,
                            .lhs = lhs_id,
                            .rhs = *lo},
                    .type = result_type});
            const mir::ExprId le_id = scope.AddExpr(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kLessEqual,
                            .lhs = lhs_id,
                            .rhs = *hi},
                    .type = result_type});
            return scope.AddExpr(
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

}  // namespace lyra::lowering::hir_to_mir
