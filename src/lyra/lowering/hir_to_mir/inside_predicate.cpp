#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"

#include <expected>
#include <utility>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/inside_item.hpp"
#include "lyra/hir/process.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildHirInsideItemPredicate(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::Process& hir_process, mir::ExprId lhs_id,
    const hir::InsideItem& item, mir::TypeId result_type)
    -> diag::Result<mir::ExprId> {
  auto lower_id = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_process,
        hir_process.exprs.at(id.value));
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    return proc_scope_state.AddExpr(*std::move(lowered));
  };

  return std::visit(
      Overloaded{
          [&](const hir::ExprId& val_id) -> diag::Result<mir::ExprId> {
            auto v = lower_id(val_id);
            if (!v) return std::unexpected(std::move(v.error()));
            return proc_scope_state.AddExpr(
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
            const mir::ExprId ge_id = proc_scope_state.AddExpr(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kGreaterEqual,
                            .lhs = lhs_id,
                            .rhs = *lo},
                    .type = result_type});
            const mir::ExprId le_id = proc_scope_state.AddExpr(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kLessEqual,
                            .lhs = lhs_id,
                            .rhs = *hi},
                    .type = result_type});
            return proc_scope_state.AddExpr(
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
