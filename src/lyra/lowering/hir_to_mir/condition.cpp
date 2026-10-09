#include "lyra/lowering/hir_to_mir/condition.hpp"

#include <ranges>
#include <span>
#include <vector>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/unary_op.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildMachineBool(
    const mir::CompilationUnit& unit, mir::Block& block, bool value)
    -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data = mir::MachineBoolLiteral{.value = value},
          .type = unit.builtins.machine_bool});
}

auto BuildLogicalNot(mir::Block& block, mir::ExprId operand) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::UnaryExpr{
                  .op = mir::UnaryOp::kLogicalNot, .operand = operand},
          .type = block.exprs.Get(operand).type});
}

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

auto BuildSelectionChain(
    mir::Block& block, std::span<const SelectedValue> arms,
    mir::ExprId otherwise, mir::TypeId type) -> mir::ExprId {
  mir::ExprId rest = otherwise;
  for (const SelectedValue& arm : arms | std::views::reverse) {
    rest = block.exprs.Add(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = arm.selected,
                    .then_value = arm.value,
                    .else_value = rest},
            .type = type});
  }
  return rest;
}

namespace {

// The search through `conditions` that stops at the first one equal to
// `settles`, answering `settles` there and the last condition's own answer
// when none did.
auto Search(
    const mir::CompilationUnit& unit, mir::Block& block,
    std::span<const mir::ExprId> conditions, bool settles) -> mir::ExprId {
  if (conditions.empty()) {
    return BuildMachineBool(unit, block, !settles);
  }
  std::vector<SelectedValue> arms;
  arms.reserve(conditions.size() - 1);
  for (const mir::ExprId condition : conditions.first(conditions.size() - 1)) {
    const mir::ExprId holds = ReduceToCondition(unit, block, condition);
    arms.push_back(
        {.selected = settles ? holds : BuildLogicalNot(block, holds),
         .value = BuildMachineBool(unit, block, settles)});
  }
  return BuildSelectionChain(
      block, arms, ReduceToCondition(unit, block, conditions.back()),
      unit.builtins.machine_bool);
}

}  // namespace

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
