#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"

#include <array>
#include <expected>
#include <utility>

#include "lyra/hir/binary_op.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/struct_methods.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM 11.4.13: an operand of a membership test is compared with the
// asymmetric wildcard equality, except a value range, which is a bounds test.
// The distinction is a property of the operand's HIR shape, read here -- HIR
// carries the SV form, and this is the layer that turns it into primitives.
// Each comparison answers with an unknown only where an operand can hold one,
// so it is built at that one-bit type and read as `result_type` afterwards.
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
  const auto type_of = [&](mir::ExprId id) { return block.exprs.Get(id).type; };

  if (const auto* range =
          std::get_if<hir::ValueRangeExpr>(&hir_exprs.Get(item).data)) {
    auto lo = lower_id(range->lo);
    if (!lo) return std::unexpected(std::move(lo.error()));
    auto hi = lower_id(range->hi);
    if (!hi) return std::unexpected(std::move(hi.error()));
    const mir::TypeId type = OneBitAnswerType(
        unit, std::array{type_of(lhs_id), type_of(*lo), type_of(*hi)});
    const mir::ExprId ge_id = block.exprs.Add(BuildMirBinaryExpr(
        unit, block, hir::BinaryOp::kGreaterEqual, lhs_id, *lo, type));
    const mir::ExprId le_id = block.exprs.Add(BuildMirBinaryExpr(
        unit, block, hir::BinaryOp::kLessEqual, lhs_id, *hi, type));
    return ConvertToType(
        unit, block,
        BuildMirLogicalAnd(unit, block, type, std::array{ge_id, le_id}),
        result_type);
  }

  auto value = lower_id(item);
  if (!value) return std::unexpected(std::move(value.error()));
  return ConvertToType(
      unit, block,
      block.exprs.Add(BuildMirBinaryExpr(
          unit, block, hir::BinaryOp::kWildcardEquality, lhs_id, *value,
          OneBitAnswerType(
              unit, std::array{type_of(lhs_id), type_of(*value)}))),
      result_type);
}

template auto BuildHirInsideItemPredicate(
    ProcessLowerer&, WalkFrame, mir::ExprId, hir::ExprId, mir::TypeId)
    -> diag::Result<mir::ExprId>;
template auto BuildHirInsideItemPredicate(
    const StructuralScopeLowerer&, WalkFrame, mir::ExprId, hir::ExprId,
    mir::TypeId) -> diag::Result<mir::ExprId>;

}  // namespace lyra::lowering::hir_to_mir
