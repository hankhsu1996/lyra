#include "lyra/lowering/hir_to_mir/condition.hpp"

#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto ReduceToCondition(
    mir::Block& block, mir::ExprId cond, mir::TypeId bit1_type) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{.data = mir::BoolCastExpr{.operand = cond}, .type = bit1_type});
}

}  // namespace lyra::lowering::hir_to_mir
