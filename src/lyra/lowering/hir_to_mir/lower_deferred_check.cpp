#include "lyra/lowering/hir_to_mir/lower_deferred_check.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/lower_stmt.hpp"
#include "lyra/lowering/hir_to_mir/procedural_scope_helpers.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/runtime_diagnostic.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/runtime_submit.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/value_ref.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

struct UniqueIfCascade {
  std::vector<hir::ExprId> conditions;
  std::vector<hir::StmtId> bodies;
  std::optional<hir::StmtId> tail_else;
};

auto FlattenUniqueCascade(const hir::Process& proc, const hir::IfStmt& root)
    -> UniqueIfCascade {
  UniqueIfCascade out;
  out.conditions.push_back(root.condition);
  out.bodies.push_back(root.then_stmt);
  std::optional<hir::StmtId> cur_else = root.else_stmt;
  while (cur_else.has_value()) {
    const hir::Stmt& s = proc.stmts.at(cur_else->value);
    const auto* nested = std::get_if<hir::IfStmt>(&s.data);
    if (nested == nullptr || nested->check.has_value()) {
      out.tail_else = cur_else;
      break;
    }
    out.conditions.push_back(nested->condition);
    out.bodies.push_back(nested->then_stmt);
    cur_else = nested->else_stmt;
  }
  return out;
}

struct CheckVerdict {
  mir::BinaryOp violation_op;
  std::uint64_t expected;
  std::string prefix_text;
  bool include_count_value;
  std::string suffix_text;
};

auto VerdictFor(hir::UniquePriorityCheck check, std::size_t branch_count)
    -> CheckVerdict {
  switch (check) {
    case hir::UniquePriorityCheck::kUnique:
      return {
          .violation_op = mir::BinaryOp::kInequality,
          .expected = 1,
          .prefix_text = "unique check failed: matched ",
          .include_count_value = true,
          .suffix_text = std::format(" of {} conditions", branch_count)};
    case hir::UniquePriorityCheck::kUnique0:
      return {
          .violation_op = mir::BinaryOp::kGreaterThan,
          .expected = 1,
          .prefix_text = "unique0 check failed: matched ",
          .include_count_value = true,
          .suffix_text = std::format(" of {} conditions", branch_count)};
    case hir::UniquePriorityCheck::kPriority:
      return {
          .violation_op = mir::BinaryOp::kEquality,
          .expected = 0,
          .prefix_text = "priority check failed: no condition matched",
          .include_count_value = false,
          .suffix_text = ""};
  }
  throw InternalError("VerdictFor: unknown HIR UniquePriorityCheck");
}

auto AppendInt32Literal(
    mir::ProceduralScope& scope, mir::TypeId int32_type, std::uint64_t value)
    -> mir::ExprId {
  const mir::ExprId id{static_cast<std::uint32_t>(scope.exprs.size())};
  scope.exprs.push_back(
      mir::Expr{
          .data =
              mir::IntegerLiteral{
                  .value =
                      mir::IntegralConstant{
                          .value_words = {value},
                          .state_words = {},
                          .width = 32,
                          .signedness = mir::Signedness::kSigned,
                          .state_kind = mir::IntegralStateKind::kTwoState}},
          .type = int32_type});
  return id;
}

auto AppendExpr(mir::ProceduralScope& scope, mir::Expr expr) -> mir::ExprId {
  const mir::ExprId id{static_cast<std::uint32_t>(scope.exprs.size())};
  scope.exprs.push_back(std::move(expr));
  return id;
}

auto AppendStmt(mir::ProceduralScope& scope, mir::Stmt stmt) -> mir::StmtId {
  const mir::StmtId id{static_cast<std::uint32_t>(scope.stmts.size())};
  scope.stmts.push_back(std::move(stmt));
  return id;
}

auto SnapshotPredicate(
    ProceduralScopeLoweringState& wrapper_state, std::size_t index,
    mir::TypeId predicate_type, mir::ExprId predicate_expr_id)
    -> mir::ProceduralVarId {
  const std::string var_name = std::format("_lyra_unique_cond_{}", index);
  const mir::ProceduralVarId snap_var = wrapper_state.AddProceduralVar(
      mir::ProceduralVarDecl{.name = var_name, .type = predicate_type});

  const mir::StmtId decl_id = wrapper_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data =
              mir::ProceduralVarDeclStmt{
                  .target =
                      mir::ProceduralVarRef{
                          .hops = mir::ProceduralHops{.value = 0},
                          .var = snap_var},
                  .init = std::nullopt},
          .child_procedural_scopes = {}});
  wrapper_state.AddRootStmt(decl_id);

  const mir::ExprId assign_id = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::AssignExpr{
                  .target =
                      mir::Lvalue{
                          .root =
                              mir::ProceduralVarRef{
                                  .hops = mir::ProceduralHops{.value = 0},
                                  .var = snap_var},
                          .selectors = {}},
                  .value = predicate_expr_id},
          .type = predicate_type});
  const mir::StmtId assign_stmt_id = wrapper_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ExprStmt{.expr = assign_id},
          .child_procedural_scopes = {}});
  wrapper_state.AddRootStmt(assign_stmt_id);

  return snap_var;
}

auto BuildDiagnosticThenScope(
    mir::ProceduralVarId count_var, mir::TypeId int32_type,
    mir::TypeId void_type, const CheckVerdict& verdict)
    -> mir::ProceduralScope {
  mir::ProceduralScope scope;

  std::vector<mir::RuntimePrintItem> items;
  items.emplace_back(mir::RuntimePrintLiteral{.text = verdict.prefix_text});
  if (verdict.include_count_value) {
    const mir::ExprId count_read_id = AppendExpr(
        scope,
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = mir::ProceduralHops{.value = 1}, .var = count_var},
            .type = int32_type});
    items.emplace_back(
        mir::RuntimePrintValue(
            count_read_id, int32_type,
            mir::FormatSpec(
                mir::FormatKind::kDecimal, mir::FormatModifiers{})));
    items.emplace_back(mir::RuntimePrintLiteral{.text = verdict.suffix_text});
  }

  const mir::ExprId diag_call_id = AppendExpr(
      scope, mir::Expr{
                 .data =
                     mir::RuntimeCallExpr{
                         .call = mir::RuntimeDiagnosticCall(
                             mir::DiagnosticSeverity::kWarning, std::nullopt,
                             std::move(items))},
                 .type = void_type});
  const mir::StmtId diag_stmt_id = AppendStmt(
      scope, mir::Stmt{
                 .label = std::nullopt,
                 .data = mir::ExprStmt{.expr = diag_call_id},
                 .child_procedural_scopes = {}});
  scope.root_stmts.push_back(diag_stmt_id);
  return scope;
}

auto BuildUniqueCheckClosure(
    ProceduralScopeLoweringState& wrapper_state, mir::TypeId int32_type,
    mir::TypeId void_type, hir::UniquePriorityCheck check,
    const std::vector<mir::ProceduralVarId>& snapshot_vars)
    -> mir::ClosureExpr {
  mir::ClosureExpr closure;
  closure.body = std::make_unique<mir::ProceduralScope>();
  auto& body = *closure.body;

  std::vector<mir::ExprId> inner_reads;
  inner_reads.reserve(snapshot_vars.size());
  for (std::size_t i = 0; i < snapshot_vars.size(); ++i) {
    const mir::TypeId snap_type =
        wrapper_state.GetProceduralVar(snapshot_vars[i]).type;
    const mir::ExprId outer_read_id = wrapper_state.AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = mir::ProceduralHops{.value = 0},
                    .var = snapshot_vars[i]},
            .type = snap_type});

    const mir::ProceduralVarId binding{
        static_cast<std::uint32_t>(body.vars.size())};
    body.vars.emplace_back(
        mir::ProceduralVarDecl{
            .name = std::format("_lyra_unique_bind_{}", i), .type = snap_type});
    closure.captures.emplace_back(
        mir::ByValueCapture{.value = outer_read_id, .binding = binding});

    const mir::ExprId inner_read_id = AppendExpr(
        body,
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = mir::ProceduralHops{.value = 0}, .var = binding},
            .type = snap_type});
    inner_reads.push_back(inner_read_id);
  }

  const mir::ProceduralVarId count_var{
      static_cast<std::uint32_t>(body.vars.size())};
  body.vars.push_back(
      mir::ProceduralVarDecl{.name = "_lyra_unique_count", .type = int32_type});

  const mir::ExprId zero_init_id = AppendInt32Literal(body, int32_type, 0);
  const mir::StmtId count_decl_id = AppendStmt(
      body, mir::Stmt{
                .label = std::nullopt,
                .data =
                    mir::ProceduralVarDeclStmt{
                        .target =
                            mir::ProceduralVarRef{
                                .hops = mir::ProceduralHops{.value = 0},
                                .var = count_var},
                        .init = zero_init_id},
                .child_procedural_scopes = {}});
  body.root_stmts.push_back(count_decl_id);

  for (const mir::ExprId bit_read : inner_reads) {
    const mir::ExprId one_lit = AppendInt32Literal(body, int32_type, 1);
    const mir::ExprId zero_lit = AppendInt32Literal(body, int32_type, 0);
    const mir::ExprId cond_value = AppendExpr(
        body, mir::Expr{
                  .data =
                      mir::ConditionalExpr{
                          .condition = bit_read,
                          .then_value = one_lit,
                          .else_value = zero_lit},
                  .type = int32_type});
    const mir::ExprId count_read = AppendExpr(
        body,
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = mir::ProceduralHops{.value = 0}, .var = count_var},
            .type = int32_type});
    const mir::ExprId added = AppendExpr(
        body, mir::Expr{
                  .data =
                      mir::BinaryExpr{
                          .op = mir::BinaryOp::kAdd,
                          .lhs = count_read,
                          .rhs = cond_value},
                  .type = int32_type});
    const mir::ExprId assign = AppendExpr(
        body,
        mir::Expr{
            .data =
                mir::AssignExpr{
                    .target =
                        mir::Lvalue{
                            .root =
                                mir::ProceduralVarRef{
                                    .hops = mir::ProceduralHops{.value = 0},
                                    .var = count_var},
                            .selectors = {}},
                    .value = added},
            .type = int32_type});
    const mir::StmtId step_id = AppendStmt(
        body, mir::Stmt{
                  .label = std::nullopt,
                  .data = mir::ExprStmt{.expr = assign},
                  .child_procedural_scopes = {}});
    body.root_stmts.push_back(step_id);
  }

  const CheckVerdict verdict = VerdictFor(check, snapshot_vars.size());

  const mir::ExprId final_count_read = AppendExpr(
      body,
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = count_var},
          .type = int32_type});
  const mir::ExprId expected_lit =
      AppendInt32Literal(body, int32_type, verdict.expected);
  const mir::ExprId predicate_id = AppendExpr(
      body, mir::Expr{
                .data =
                    mir::BinaryExpr{
                        .op = verdict.violation_op,
                        .lhs = final_count_read,
                        .rhs = expected_lit},
                .type = int32_type});

  mir::ProceduralScope diag_scope =
      BuildDiagnosticThenScope(count_var, int32_type, void_type, verdict);

  std::vector<mir::ProceduralScope> if_children;
  const mir::ProceduralScopeId diag_scope_id =
      AddChildProceduralScope(if_children, std::move(diag_scope));

  const mir::StmtId if_stmt_id = AppendStmt(
      body, mir::Stmt{
                .label = std::nullopt,
                .data =
                    mir::IfStmt{
                        .condition = predicate_id,
                        .then_scope = diag_scope_id,
                        .else_scope = std::nullopt},
                .child_procedural_scopes = std::move(if_children)});
  body.root_stmts.push_back(if_stmt_id);

  return closure;
}

}  // namespace

auto BuildDeferredCheckCascade(
    const UnitLoweringState& unit_state,
    ProceduralScopeLoweringState wrapper_state,
    std::vector<DeferredCheckBranch> branches,
    std::optional<mir::ProceduralScope> tail_else,
    hir::UniquePriorityCheck check, std::optional<std::string> outer_label)
    -> mir::Stmt {
  const mir::TypeId void_type = unit_state.Builtins().void_type;
  const mir::TypeId int32_type = unit_state.Builtins().int32;

  std::vector<mir::ProceduralVarId> snapshot_vars;
  snapshot_vars.reserve(branches.size());
  for (std::size_t i = 0; i < branches.size(); ++i) {
    const mir::TypeId predicate_type =
        wrapper_state.GetExpr(branches[i].predicate).type;
    snapshot_vars.push_back(SnapshotPredicate(
        wrapper_state, i, predicate_type, branches[i].predicate));
  }

  mir::ClosureExpr closure = BuildUniqueCheckClosure(
      wrapper_state, int32_type, void_type, check, snapshot_vars);
  const mir::ExprId closure_expr_id = wrapper_state.AddExpr(
      mir::Expr{.data = std::move(closure), .type = void_type});

  const mir::DeferredCheckSiteId site_id =
      unit_state.AllocateDeferredCheckSiteId();
  const mir::ExprId submit_expr_id = wrapper_state.AddExpr(
      mir::Expr{
          .data =
              mir::RuntimeCallExpr{
                  .call =
                      mir::RuntimeSubmitObservedCall{
                          .site_id = site_id, .closure = closure_expr_id}},
          .type = void_type});
  const mir::StmtId submit_stmt_id = wrapper_state.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ExprStmt{.expr = submit_expr_id},
          .child_procedural_scopes = {}});
  wrapper_state.AddRootStmt(submit_stmt_id);

  // Cascade level i sits i scopes deeper than wrapper, so the read uses hops=i.
  std::optional<mir::ProceduralScope> tail = std::move(tail_else);
  for (std::size_t i = branches.size(); i-- > 1;) {
    ProceduralScopeLoweringState level_state;
    const mir::ExprId cond_read = level_state.AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops =
                        mir::ProceduralHops{
                            .value = static_cast<std::uint32_t>(i)},
                    .var = snapshot_vars[i]},
            .type = int32_type});

    std::vector<mir::ProceduralScope> if_child_scopes;
    const mir::ProceduralScopeId body_scope_id =
        AddChildProceduralScope(if_child_scopes, std::move(branches[i].body));
    std::optional<mir::ProceduralScopeId> else_scope_id;
    if (tail.has_value()) {
      else_scope_id =
          AddChildProceduralScope(if_child_scopes, std::move(*tail));
    }

    const mir::StmtId if_id = level_state.AddStmt(
        mir::Stmt{
            .label = std::nullopt,
            .data =
                mir::IfStmt{
                    .condition = cond_read,
                    .then_scope = body_scope_id,
                    .else_scope = else_scope_id},
            .child_procedural_scopes = std::move(if_child_scopes)});
    level_state.AddRootStmt(if_id);
    tail = level_state.Finish();
  }

  if (!branches.empty()) {
    const mir::ExprId cond_read0 = wrapper_state.AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = mir::ProceduralHops{.value = 0},
                    .var = snapshot_vars[0]},
            .type = int32_type});

    std::vector<mir::ProceduralScope> if0_child_scopes;
    const mir::ProceduralScopeId body0_id =
        AddChildProceduralScope(if0_child_scopes, std::move(branches[0].body));
    std::optional<mir::ProceduralScopeId> else0_id;
    if (tail.has_value()) {
      else0_id = AddChildProceduralScope(if0_child_scopes, std::move(*tail));
    }

    const mir::StmtId if0_id = wrapper_state.AddStmt(
        mir::Stmt{
            .label = std::nullopt,
            .data =
                mir::IfStmt{
                    .condition = cond_read0,
                    .then_scope = body0_id,
                    .else_scope = else0_id},
            .child_procedural_scopes = std::move(if0_child_scopes)});
    wrapper_state.AddRootStmt(if0_id);
  }

  std::vector<mir::ProceduralScope> outer_child_scopes;
  const mir::ProceduralScopeId wrapper_scope_id =
      AddChildProceduralScope(outer_child_scopes, wrapper_state.Finish());

  return mir::Stmt{
      .label = std::move(outer_label),
      .data = mir::BlockStmt{.scope = wrapper_scope_id},
      .child_procedural_scopes = std::move(outer_child_scopes)};
}

auto LowerUniqueIfStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::Process& hir_proc,
    const hir::Stmt& stmt, const hir::IfStmt& root) -> diag::Result<mir::Stmt> {
  const auto cascade = FlattenUniqueCascade(hir_proc, root);

  ProceduralScopeLoweringState wrapper_state;
  ProceduralDepthGuard wrapper_depth_guard{proc_state};

  // Lower each branch condition into wrapper_state; these are the predicates
  // that BuildDeferredCheckCascade will snapshot.
  std::vector<mir::ExprId> predicates;
  predicates.reserve(cascade.conditions.size());
  for (const hir::ExprId hir_cond : cascade.conditions) {
    auto cond_or = LowerExpr(
        unit_state, scope_state, proc_state, wrapper_state, hir_proc,
        hir_proc.exprs.at(hir_cond.value));
    if (!cond_or) return std::unexpected(std::move(cond_or.error()));
    predicates.push_back(wrapper_state.AddExpr(*std::move(cond_or)));
  }

  // Lower each body into its own child scope, with depth guards mirroring
  // the cascade structure (level i sits i scopes deeper than wrapper).
  auto with_extra_depth = [&](std::size_t extras, auto fn) {
    std::vector<std::unique_ptr<ProceduralDepthGuard>> guards;
    guards.reserve(extras);
    for (std::size_t i = 0; i < extras; ++i) {
      guards.push_back(std::make_unique<ProceduralDepthGuard>(proc_state));
    }
    return fn();
  };

  std::vector<DeferredCheckBranch> branches;
  branches.reserve(cascade.bodies.size());
  for (std::size_t i = 0; i < cascade.bodies.size(); ++i) {
    auto body_or = with_extra_depth(i, [&] {
      return LowerStmtIntoChildScope(
          unit_state, scope_state, proc_state, hir_proc, cascade.bodies[i]);
    });
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    branches.push_back(
        DeferredCheckBranch{
            .predicate = predicates[i], .body = std::move(*body_or)});
  }

  std::optional<mir::ProceduralScope> tail_scope;
  if (cascade.tail_else.has_value()) {
    auto tail_or = with_extra_depth(cascade.bodies.size(), [&] {
      return LowerStmtIntoChildScope(
          unit_state, scope_state, proc_state, hir_proc, *cascade.tail_else);
    });
    if (!tail_or) return std::unexpected(std::move(tail_or.error()));
    tail_scope = std::move(*tail_or);
  }

  return BuildDeferredCheckCascade(
      unit_state, std::move(wrapper_state), std::move(branches),
      std::move(tail_scope), *root.check, stmt.label);
}

}  // namespace lyra::lowering::hir_to_mir
