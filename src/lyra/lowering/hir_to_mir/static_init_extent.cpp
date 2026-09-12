#include "lyra/lowering/hir_to_mir/static_init_extent.hpp"

#include <utility>
#include <vector>

#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

void EmitStaticInitBracket(
    const UnitLowerer& unit_lowerer, mir::Block& block,
    support::BuiltinFn bracket, std::vector<mir::ExprId> operands) {
  std::vector<mir::ExprId> arguments;
  arguments.reserve(operands.size() + 1);
  arguments.push_back(
      block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer)));
  arguments.insert(arguments.end(), operands.begin(), operands.end());
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee = mir::Direct{.target = bracket},
                          .arguments = std::move(arguments)},
                  .type = unit_lowerer.Unit().builtins.void_type})});
}

auto CloseStaticInitExtent(
    const UnitLowerer& unit_lowerer, mir::Block&& extent, mir::Block&& body)
    -> mir::Block {
  mir::Block closed = std::move(extent);
  const mir::BlockId body_id = closed.child_scopes.Add(std::move(body));

  mir::Block cleanup;
  EmitStaticInitBracket(
      unit_lowerer, cleanup, support::BuiltinFn::kLeaveStaticInit, {});
  const mir::BlockId cleanup_id = closed.child_scopes.Add(std::move(cleanup));

  closed.AppendStmt(mir::FinallyStmt{.body = body_id, .cleanup = cleanup_id});
  return closed;
}

}  // namespace lyra::lowering::hir_to_mir
