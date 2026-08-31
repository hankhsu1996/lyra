#include "lyra/lowering/hir_to_mir/condition.hpp"

#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto ReduceToCondition(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId cond)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::BoolCastExpr{.operand = cond},
          .type = unit.builtins.machine_bool});
}

}  // namespace lyra::lowering::hir_to_mir
