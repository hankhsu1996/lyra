#include "lyra/lowering/hir_to_mir/expression/system/random.hpp"

#include <expected>
#include <utility>
#include <vector>

#include "lyra/hir/expr_id.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"  // IWYU pragma: keep
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"  // IWYU pragma: keep
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

template <ExprLowerer Lowerer>
auto LowerRandomSystemSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    const support::RandomSystemSubroutineInfo& info)
    -> diag::Result<mir::Expr> {
  const std::vector<hir::ExprId> operands = RequiredOperands(call);
  auto& unit = lowerer.Owner().Unit();
  auto& body = *frame.current_block;

  std::vector<mir::ExprId> arguments;
  arguments.push_back(
      body.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner())));
  for (const hir::ExprId operand : operands) {
    auto lowered = lowerer.LowerExpr(lowerer.HirExprs().Get(operand), frame);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    arguments.push_back(body.exprs.Add(*std::move(lowered)));
  }

  support::BuiltinFn target = support::BuiltinFn::kUrandom;
  if (info.kind == support::RandomKind::kUrandom) {
    if (operands.size() == 1) {
      target = support::BuiltinFn::kUrandomSeeded;
    }
  } else {
    target = support::BuiltinFn::kUrandomRange;
    if (operands.size() == 1) {
      arguments.push_back(
          body.exprs.Add(mir::MakeIntLiteral(unit.builtins.int_type, 0)));
    }
  }

  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = target},
              .arguments = std::move(arguments)},
      .type = unit.builtins.int_unsigned};
}

template auto LowerRandomSystemSubroutineCall(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&,
    const support::RandomSystemSubroutineInfo&) -> diag::Result<mir::Expr>;
template auto LowerRandomSystemSubroutineCall(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&,
    const support::RandomSystemSubroutineInfo&) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
