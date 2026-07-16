#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"

#include <expected>
#include <utility>

#include "lyra/base/overloaded.hpp"
#include "lyra/hir/inside_item.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
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
  auto& unit = lowerer.Owner().Unit();
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
            return block.exprs.Add(BuildMirBinaryExpr(
                unit, block, mir::BinaryOp::kWildcardEquality, lhs_id, *v,
                result_type));
          },
          [&](const hir::InsideRangePair& r) -> diag::Result<mir::ExprId> {
            auto lo = lower_id(r.lo);
            if (!lo) return std::unexpected(std::move(lo.error()));
            auto hi = lower_id(r.hi);
            if (!hi) return std::unexpected(std::move(hi.error()));
            const mir::ExprId ge_id = block.exprs.Add(BuildMirBinaryExpr(
                unit, block, mir::BinaryOp::kGreaterEqual, lhs_id, *lo,
                result_type));
            const mir::ExprId le_id = block.exprs.Add(BuildMirBinaryExpr(
                unit, block, mir::BinaryOp::kLessEqual, lhs_id, *hi,
                result_type));
            return block.exprs.Add(BuildMirBinaryExpr(
                unit, block, mir::BinaryOp::kLogicalAnd, ge_id, le_id,
                result_type));
          },
      },
      item);
}

template auto BuildHirInsideItemPredicate(
    ProcessLowerer&, WalkFrame, mir::ExprId, const hir::InsideItem&,
    mir::TypeId) -> diag::Result<mir::ExprId>;
template auto BuildHirInsideItemPredicate(
    const StructuralScopeLowerer&, WalkFrame, mir::ExprId,
    const hir::InsideItem&, mir::TypeId) -> diag::Result<mir::ExprId>;

}  // namespace lyra::lowering::hir_to_mir
