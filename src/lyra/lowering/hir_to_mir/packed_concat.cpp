#include "lyra/lowering/hir_to_mir/packed_concat.hpp"

#include <cstddef>
#include <cstdint>
#include <span>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildPackedConcat(
    mir::CompilationUnit& unit, mir::Block& block,
    std::span<const mir::ExprId> operands) -> mir::ExprId {
  if (operands.empty()) {
    throw InternalError("BuildPackedConcat: a join has at least one operand");
  }
  const auto shape_of =
      [&](mir::ExprId operand) -> const mir::PackedArrayType& {
    return unit.types.Get(block.exprs.Get(operand).type).PackedShape();
  };
  // Bits join two at a time, because the entry that composes them takes two.
  // The order is left to right, which is the order the operands were written
  // in: joining is associative over both the bit plane and the state domain, so
  // the chain and the single N-operand join it stands for hold the same value.
  // Each step is as wide as what it has joined so far, and carries an X or a Z
  // as soon as one of those operands can.
  std::uint64_t width = shape_of(operands.front()).BitWidth();
  mir::IntegralStateKind state_kind = shape_of(operands.front()).state_kind;
  mir::ExprId joined = operands.front();
  for (std::size_t i = 1; i < operands.size(); ++i) {
    const mir::PackedArrayType& shape = shape_of(operands[i]);
    width += shape.BitWidth();
    if (shape.state_kind == mir::IntegralStateKind::kFourState) {
      state_kind = mir::IntegralStateKind::kFourState;
    }
    joined = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kConcat,
                            .receiver = joined},
                    .arguments = {operands[i]}},
            .type = mir::PackedVectorOf(unit.types, width, state_kind)});
  }
  return joined;
}

}  // namespace lyra::lowering::hir_to_mir
