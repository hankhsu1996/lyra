#include "lyra/lowering/hir_to_mir/condition.hpp"

#include <optional>
#include <ranges>
#include <span>
#include <vector>

#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
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

auto BuildConditionNot(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId condition)
    -> mir::ExprId {
  return block.exprs.Add(MakeUnary(
      unit, block, mir::UnaryOp::kLogicalNot, condition,
      unit.builtins.machine_bool));
}

auto ConditionAsBit(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value)
    -> mir::ExprId {
  const mir::ExprId condition = ReduceToCondition(unit, block, value);
  return block.exprs.Add(MakeBuiltinCall(
      unit, block, support::BuiltinFn::kFromBool, std::nullopt, {condition},
      unit.builtins.bit1));
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
// when none did. Each condition selects between the answer it settles and the
// search through the ones after it, so no condition is negated to be asked:
//
//   any:  c0 ? true : c1 ? true : ... : last
//   all:  c0 ? (c1 ? ... last ... : false) : false
auto Search(
    const mir::CompilationUnit& unit, mir::Block& block,
    std::span<const mir::ExprId> conditions, bool settles) -> mir::ExprId {
  if (conditions.empty()) {
    return BuildMachineBool(unit, block, !settles);
  }
  std::vector<mir::ExprId> held;
  held.reserve(conditions.size());
  for (const mir::ExprId condition : conditions) {
    held.push_back(ReduceToCondition(unit, block, condition));
  }
  mir::ExprId rest = held.back();
  for (const mir::ExprId holds :
       std::span(held).first(held.size() - 1) | std::views::reverse) {
    const mir::ExprId settled = BuildMachineBool(unit, block, settles);
    rest = block.exprs.Add(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = holds,
                    .then_value = settles ? settled : rest,
                    .else_value = settles ? rest : settled},
            .type = unit.builtins.machine_bool});
  }
  return rest;
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
