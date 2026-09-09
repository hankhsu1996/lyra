#include "lyra/lowering/hir_to_mir/expression/system/sampled_value.hpp"

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

template <ExprLowerer Lowerer>
auto LowerSampledValueSystemSubroutineCall(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  if (call.arguments.size() != 1 || !call.arguments.front().has_value()) {
    throw InternalError(
        "HIR->MIR sampled value: the operand whose sampled value this answers "
        "with is absent");
  }
  const hir::Expr& operand = lowerer.HirExprs().Get(*call.arguments.front());
  return lowerer.LowerExpr(operand, frame.WithReadsAsOf(ReadsAsOf::kPreponed));
}

template auto LowerSampledValueSystemSubroutineCall(
    ProcessLowerer&, const WalkFrame&, const hir::CallExpr&)
    -> diag::Result<mir::Expr>;
template auto LowerSampledValueSystemSubroutineCall(
    const StructuralScopeLowerer&, const WalkFrame&, const hir::CallExpr&)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
