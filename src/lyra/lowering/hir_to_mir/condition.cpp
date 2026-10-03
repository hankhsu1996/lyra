#include "lyra/lowering/hir_to_mir/condition.hpp"

#include <cstddef>
#include <span>

#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto BoolLiteral(
    const mir::CompilationUnit& unit, mir::Block& block, bool value)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::MachineBoolLiteral{.value = value},
          .type = unit.builtins.machine_bool});
}

// The search through `conditions` that stops at the first one equal to
// `settles`, answering `settles` there and the last condition's own answer
// when none did. Each step tests in the position a selection continues in, so
// a search of any length is one flat chain of selections rather than one
// nested a level per condition.
auto Search(
    const mir::CompilationUnit& unit, mir::Block& block,
    std::span<const mir::ExprId> conditions, bool settles) -> mir::ExprId {
  if (conditions.empty()) {
    return BoolLiteral(unit, block, !settles);
  }
  const mir::TypeId boolean = unit.builtins.machine_bool;
  mir::ExprId rest = ReduceToCondition(unit, block, conditions.back());
  for (std::size_t i = conditions.size() - 1; i-- > 0;) {
    mir::ExprId test = ReduceToCondition(unit, block, conditions[i]);
    if (!settles) {
      test = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::UnaryExpr{
                      .op = mir::UnaryOp::kLogicalNot, .operand = test},
              .type = boolean});
    }
    rest = block.exprs.Add(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = test,
                    .then_value = BoolLiteral(unit, block, settles),
                    .else_value = rest},
            .type = boolean});
  }
  return rest;
}

}  // namespace

auto ReduceToCondition(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId cond)
    -> mir::ExprId {
  if (block.exprs.Get(cond).type == unit.builtins.machine_bool) {
    return cond;
  }
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = cond},
          .type = unit.builtins.machine_bool});
}

auto ConditionAsBit(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kFromBool},
                  .arguments = {ReduceToCondition(unit, block, value)}},
          .type = unit.builtins.bit1});
}

auto AllHold(
    const mir::CompilationUnit& unit, mir::Block& block,
    std::span<const mir::ExprId> conditions) -> mir::ExprId {
  return Search(unit, block, conditions, false);
}

auto AnyHolds(
    const mir::CompilationUnit& unit, mir::Block& block,
    std::span<const mir::ExprId> conditions) -> mir::ExprId {
  return Search(unit, block, conditions, true);
}

}  // namespace lyra::lowering::hir_to_mir
