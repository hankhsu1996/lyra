#include "lyra/lowering/hir_to_mir/lower_constructor.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/facts.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerGenerateStmt(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& owner_scope, const hir::Generate& gen,
    BodyLoweringState& body_state, ScopeStack& stack)
    -> diag::Result<mir::Stmt>;

auto LowerChildScopeIntoBody(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& child, mir::Body& out_body, ScopeStack& stack)
    -> diag::Result<void>;

auto LowerStructuralExprFromScope(
    const hir::StructuralScope& owner_scope, hir::ExprId hir_id)
    -> diag::Result<mir::Expr> {
  const hir::Expr& src = owner_scope.GetExpr(hir_id);
  auto data = LowerStructuralExprData(src.data);
  if (!data) return std::unexpected(std::move(data.error()));
  return mir::Expr{.data = *std::move(data)};
}

auto AllocateChildBody(std::vector<mir::Body>& child_bodies) -> mir::BodyId {
  const mir::BodyId id{static_cast<std::uint32_t>(child_bodies.size())};
  child_bodies.emplace_back();
  return id;
}

auto LowerIfGenerateStmt(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& owner_scope, const hir::Generate& gen,
    const hir::IfGenerate& if_gen, BodyLoweringState& body_state,
    ScopeStack& stack) -> diag::Result<mir::Stmt> {
  auto cond_expr = LowerStructuralExprFromScope(owner_scope, if_gen.condition);
  if (!cond_expr) return std::unexpected(std::move(cond_expr.error()));
  const mir::ExprId cond_id =
      body_state.AppendExpr(if_gen.condition, *std::move(cond_expr));

  std::vector<mir::Body> child_bodies;
  const mir::BodyId then_id = AllocateChildBody(child_bodies);
  auto then_r = LowerChildScopeIntoBody(
      unit_facts, unit_state, gen.child_scopes[if_gen.then_scope.value],
      child_bodies[then_id.value], stack);
  if (!then_r) return std::unexpected(std::move(then_r.error()));

  std::optional<mir::BodyId> else_id;
  if (if_gen.else_scope.has_value()) {
    else_id = AllocateChildBody(child_bodies);
    auto else_r = LowerChildScopeIntoBody(
        unit_facts, unit_state, gen.child_scopes[if_gen.else_scope->value],
        child_bodies[else_id->value], stack);
    if (!else_r) return std::unexpected(std::move(else_r.error()));
  }

  return mir::Stmt{
      .label = std::nullopt,
      .data =
          mir::IfStmt{
              .condition = cond_id, .then_body = then_id, .else_body = else_id},
      .child_bodies = std::move(child_bodies)};
}

auto LowerCaseGenerateStmt(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& owner_scope, const hir::Generate& gen,
    const hir::CaseGenerate& case_gen, BodyLoweringState& body_state,
    ScopeStack& stack) -> diag::Result<mir::Stmt> {
  auto cond_expr =
      LowerStructuralExprFromScope(owner_scope, case_gen.condition);
  if (!cond_expr) return std::unexpected(std::move(cond_expr.error()));
  const mir::ExprId cond_id =
      body_state.AppendExpr(case_gen.condition, *std::move(cond_expr));

  std::vector<mir::Body> child_bodies;
  std::vector<mir::SwitchCase> cases;
  cases.reserve(case_gen.items.size());

  for (const auto& item : case_gen.items) {
    std::vector<mir::ExprId> labels;
    labels.reserve(item.labels.size());
    for (const hir::ExprId label_hir_id : item.labels) {
      auto label_expr = LowerStructuralExprFromScope(owner_scope, label_hir_id);
      if (!label_expr) return std::unexpected(std::move(label_expr.error()));
      labels.push_back(
          body_state.AppendExpr(label_hir_id, *std::move(label_expr)));
    }

    const mir::BodyId item_body = AllocateChildBody(child_bodies);
    auto item_r = LowerChildScopeIntoBody(
        unit_facts, unit_state, gen.child_scopes[item.scope.value],
        child_bodies[item_body.value], stack);
    if (!item_r) return std::unexpected(std::move(item_r.error()));

    cases.push_back(
        mir::SwitchCase{.labels = std::move(labels), .body = item_body});
  }

  std::optional<mir::BodyId> default_body;
  if (case_gen.default_scope.has_value()) {
    default_body = AllocateChildBody(child_bodies);
    auto default_r = LowerChildScopeIntoBody(
        unit_facts, unit_state, gen.child_scopes[case_gen.default_scope->value],
        child_bodies[default_body->value], stack);
    if (!default_r) return std::unexpected(std::move(default_r.error()));
  }

  return mir::Stmt{
      .label = std::nullopt,
      .data =
          mir::SwitchStmt{
              .condition = cond_id,
              .cases = std::move(cases),
              .default_body = default_body},
      .child_bodies = std::move(child_bodies)};
}

auto LowerGenerateStmt(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& owner_scope, const hir::Generate& gen,
    BodyLoweringState& body_state, ScopeStack& stack)
    -> diag::Result<mir::Stmt> {
  return std::visit(
      [&](const auto& data) -> diag::Result<mir::Stmt> {
        using T = std::decay_t<decltype(data)>;
        if constexpr (std::is_same_v<T, hir::IfGenerate>) {
          return LowerIfGenerateStmt(
              unit_facts, unit_state, owner_scope, gen, data, body_state,
              stack);
        } else if constexpr (std::is_same_v<T, hir::CaseGenerate>) {
          return LowerCaseGenerateStmt(
              unit_facts, unit_state, owner_scope, gen, data, body_state,
              stack);
        }
      },
      gen.data);
}

auto LowerChildScopeIntoBody(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& child, mir::Body& out_body, ScopeStack& stack)
    -> diag::Result<void> {
  if (!child.MemberVars().empty()) {
    return diag::Unsupported(
        diag::DiagCode::kUnsupportedDeclInGenerate,
        "declarations inside generate scopes are not supported yet",
        diag::UnsupportedCategory::kFeature);
  }
  if (!child.Processes().empty()) {
    return diag::Unsupported(
        diag::DiagCode::kUnsupportedProcessInGenerate,
        "processes inside generate scopes are not supported yet",
        diag::UnsupportedCategory::kFeature);
  }

  const ScopeStackGuard guard(stack, child);
  BodyLoweringState body_state;

  for (const auto& nested : child.Generates()) {
    auto stmt = LowerGenerateStmt(
        unit_facts, unit_state, child, nested, body_state, stack);
    if (!stmt) return std::unexpected(std::move(stmt.error()));
    const mir::StmtId sid = body_state.AppendStmt(*std::move(stmt));
    body_state.AppendRootStmt(sid);
  }
  out_body = body_state.Finish();
  return {};
}

}  // namespace

auto LowerConstructorIntoBody(
    const UnitLoweringFacts& unit_facts, const UnitLoweringState& unit_state,
    const hir::StructuralScope& root_scope, mir::Body& out_body)
    -> diag::Result<void> {
  ScopeStack stack;
  const ScopeStackGuard guard(stack, root_scope);

  BodyLoweringState body_state;
  for (const auto& gen : root_scope.Generates()) {
    auto stmt = LowerGenerateStmt(
        unit_facts, unit_state, root_scope, gen, body_state, stack);
    if (!stmt) return std::unexpected(std::move(stmt.error()));
    const mir::StmtId sid = body_state.AppendStmt(*std::move(stmt));
    body_state.AppendRootStmt(sid);
  }
  out_body = body_state.Finish();
  return {};
}

}  // namespace lyra::lowering::hir_to_mir
