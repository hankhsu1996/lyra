#include "lyra/lowering/hir_to_mir/lower_constructor.hpp"

#include <cstdint>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/facts.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/unsupported.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerGenerate(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& owner_scope, const hir::Generate& gen,
    BodyLoweringState& body_state, ScopeStack& stack) -> mir::StmtId;

void LowerChildScopeIntoBody(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& child, mir::Body& out_body, ScopeStack& stack);

auto LowerStructuralExpr(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& owner_scope, hir::ExprId hir_id,
    BodyLoweringState& body_state, const ScopeStack& stack) -> mir::ExprId {
  const hir::Expr& src = owner_scope.GetExpr(hir_id);
  return body_state.AppendExpr(
      hir_id,
      mir::Expr{
          .data = LowerExprData(unit_facts, unit_state, stack, src.data)});
}

auto AllocateChildBody(std::vector<mir::Body>& child_bodies) -> mir::BodyId {
  const mir::BodyId id{static_cast<std::uint32_t>(child_bodies.size())};
  child_bodies.emplace_back();
  return id;
}

auto LowerIfGenerate(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& owner_scope, const hir::Generate& gen,
    const hir::IfGenerate& if_gen, BodyLoweringState& body_state,
    ScopeStack& stack) -> mir::StmtId {
  const mir::ExprId cond = LowerStructuralExpr(
      unit_facts, unit_state, owner_scope, if_gen.condition, body_state, stack);

  std::vector<mir::Body> child_bodies;
  const mir::BodyId then_id = AllocateChildBody(child_bodies);
  LowerChildScopeIntoBody(
      unit_facts, unit_state, gen.child_scopes[if_gen.then_scope.value],
      child_bodies[then_id.value], stack);

  std::optional<mir::BodyId> else_id;
  if (if_gen.else_scope.has_value()) {
    else_id = AllocateChildBody(child_bodies);
    LowerChildScopeIntoBody(
        unit_facts, unit_state, gen.child_scopes[if_gen.else_scope->value],
        child_bodies[else_id->value], stack);
  }

  return body_state.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data =
              mir::IfStmt{
                  .condition = cond,
                  .then_body = then_id,
                  .else_body = else_id},
          .child_bodies = std::move(child_bodies)});
}

auto LowerCaseGenerate(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& owner_scope, const hir::Generate& gen,
    const hir::CaseGenerate& case_gen, BodyLoweringState& body_state,
    ScopeStack& stack) -> mir::StmtId {
  const mir::ExprId cond = LowerStructuralExpr(
      unit_facts, unit_state, owner_scope, case_gen.condition, body_state,
      stack);

  std::vector<mir::Body> child_bodies;
  std::vector<mir::SwitchCase> cases;
  cases.reserve(case_gen.items.size());

  for (const auto& item : case_gen.items) {
    std::vector<mir::ExprId> labels;
    labels.reserve(item.labels.size());
    for (const auto lbl : item.labels) {
      labels.push_back(LowerStructuralExpr(
          unit_facts, unit_state, owner_scope, lbl, body_state, stack));
    }

    const mir::BodyId item_id = AllocateChildBody(child_bodies);
    LowerChildScopeIntoBody(
        unit_facts, unit_state, gen.child_scopes[item.scope.value],
        child_bodies[item_id.value], stack);

    cases.push_back(
        mir::SwitchCase{.labels = std::move(labels), .body = item_id});
  }

  std::optional<mir::BodyId> default_id;
  if (case_gen.default_scope.has_value()) {
    default_id = AllocateChildBody(child_bodies);
    LowerChildScopeIntoBody(
        unit_facts, unit_state, gen.child_scopes[case_gen.default_scope->value],
        child_bodies[default_id->value], stack);
  }

  return body_state.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data =
              mir::SwitchStmt{
                  .condition = cond,
                  .cases = std::move(cases),
                  .default_body = default_id},
          .child_bodies = std::move(child_bodies)});
}

auto LowerGenerate(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& owner_scope, const hir::Generate& gen,
    BodyLoweringState& body_state, ScopeStack& stack) -> mir::StmtId {
  return std::visit(
      [&](const auto& data) -> mir::StmtId {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, hir::IfGenerate>) {
          return LowerIfGenerate(
              unit_facts, unit_state, owner_scope, gen, data, body_state,
              stack);
        } else if constexpr (std::is_same_v<T, hir::CaseGenerate>) {
          return LowerCaseGenerate(
              unit_facts, unit_state, owner_scope, gen, data, body_state,
              stack);
        }
      },
      gen.data);
}

void LowerChildScopeIntoBody(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& child, mir::Body& out_body, ScopeStack& stack) {
  if (!child.VarDecls().empty()) {
    support::Unsupported(
        "MIR: declarations inside generate not supported in this cut");
  }
  if (!child.Processes().empty()) {
    support::Unsupported(
        "MIR: processes inside generate not supported in this cut");
  }

  const ScopeStackGuard guard(stack, child);
  BodyLoweringState body_state;

  for (const auto& nested : child.Generates()) {
    body_state.AppendRootStmt(LowerGenerate(
        unit_facts, unit_state, child, nested, body_state, stack));
  }
  out_body = body_state.Finish();
}

}  // namespace

void LowerConstructorFromScope(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& root_scope, mir::Body& out_body) {
  ScopeStack stack;
  const ScopeStackGuard guard(stack, root_scope);

  BodyLoweringState body_state;
  for (const auto& gen : root_scope.Generates()) {
    body_state.AppendRootStmt(LowerGenerate(
        unit_facts, unit_state, root_scope, gen, body_state, stack));
  }
  out_body = body_state.Finish();
}

}  // namespace lyra::lowering::hir_to_mir
