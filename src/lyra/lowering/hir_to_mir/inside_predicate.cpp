#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"

#include <expected>
#include <utility>

#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM 11.4.13: an operand of a membership test is compared with the
// asymmetric wildcard equality, except a value range, which is a bounds test.
// The distinction is a property of the operand's HIR shape, read here -- HIR
// carries the SV form, and this is the layer that turns it into primitives.
template <ExprLowerer Lowerer>
auto BuildHirInsideItemPredicate(
    Lowerer& lowerer, WalkFrame frame, mir::ExprId lhs_id, hir::ExprId item,
    mir::TypeId result_type) -> diag::Result<mir::ExprId> {
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

  if (const auto* range =
          std::get_if<hir::ValueRangeExpr>(&hir_exprs.Get(item).data)) {
    auto lo = lower_id(range->lo);
    if (!lo) return std::unexpected(std::move(lo.error()));
    auto hi = lower_id(range->hi);
    if (!hi) return std::unexpected(std::move(hi.error()));
    const mir::ExprId ge_id = block.exprs.Add(BuildMirBinaryExpr(
        unit, block, mir::BinaryOp::kGreaterEqual, lhs_id, *lo, result_type));
    const mir::ExprId le_id = block.exprs.Add(BuildMirBinaryExpr(
        unit, block, mir::BinaryOp::kLessEqual, lhs_id, *hi, result_type));
    return block.exprs.Add(BuildMirBinaryExpr(
        unit, block, mir::BinaryOp::kLogicalAnd, ge_id, le_id, result_type));
  }

  auto value = lower_id(item);
  if (!value) return std::unexpected(std::move(value.error()));
  return block.exprs.Add(BuildMirBinaryExpr(
      unit, block, mir::BinaryOp::kWildcardEquality, lhs_id, *value,
      result_type));
}

template auto BuildHirInsideItemPredicate(
    ProcessLowerer&, WalkFrame, mir::ExprId, hir::ExprId, mir::TypeId)
    -> diag::Result<mir::ExprId>;
template auto BuildHirInsideItemPredicate(
    const StructuralScopeLowerer&, WalkFrame, mir::ExprId, hir::ExprId,
    mir::TypeId) -> diag::Result<mir::ExprId>;

}  // namespace lyra::lowering::hir_to_mir
