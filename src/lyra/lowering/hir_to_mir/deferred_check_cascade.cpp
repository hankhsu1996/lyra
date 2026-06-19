#include "lyra/lowering/hir_to_mir/deferred_check_cascade.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/runtime_submit.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/value_ref.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

struct UniqueIfCascade {
  std::vector<hir::ExprId> conditions;
  std::vector<hir::StmtId> bodies;
  std::optional<hir::StmtId> tail_else;
};

auto FlattenUniqueCascade(
    const hir::ProceduralBody& proc, const hir::IfStmt& root)
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

auto SnapshotPredicate(
    const ModuleLowerer& module, WalkFrame frame, mir::ProceduralScope& wrapper,
    std::size_t index, mir::TypeId predicate_type,
    mir::ExprId predicate_expr_id) -> mir::ProceduralVarId {
  const std::string var_name = std::format("_lyra_unique_cond_{}", index);
  const mir::ProceduralVarId snap_var = wrapper.AddProceduralVar(
      mir::ProceduralVarDecl{.name = var_name, .type = predicate_type});
  const mir::ExprId snap_default_init =
      wrapper.AddExpr(BuildDefaultValueExpr(module, frame, predicate_type));
  wrapper.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ProceduralVarDeclStmt{
              .target =
                  mir::ProceduralVarRef{
                      .hops = mir::ProceduralHops{.value = 0}, .var = snap_var},
              .init = snap_default_init}});

  const mir::ExprId snap_target_id = wrapper.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = snap_var},
          .type = predicate_type});
  const mir::ExprId assign_id = wrapper.AddExpr(
      mir::Expr{
          .data =
              mir::AssignExpr{
                  .target = snap_target_id, .value = predicate_expr_id},
          .type = predicate_type});
  wrapper.AppendStmt(
      mir::Stmt{
          .label = std::nullopt, .data = mir::ExprStmt{.expr = assign_id}});

  return snap_var;
}

auto BuildDiagnosticThenScope(
    mir::CompilationUnit& unit, mir::ProceduralVarId self_binding,
    mir::ProceduralVarId count_var, const CheckVerdict& verdict)
    -> mir::ProceduralScope {
  mir::ProceduralScope scope;
  const mir::TypeId int32_type = unit.builtins.int32;

  std::vector<mir::RuntimePrintItem> items;
  items.emplace_back(mir::RuntimePrintLiteral{.text = verdict.prefix_text});
  if (verdict.include_count_value) {
    const mir::ExprId count_read_id = scope.AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = mir::ProceduralHops{.value = 1}, .var = count_var},
            .type = int32_type});
    items.emplace_back(
        mir::RuntimePrintValue(
            count_read_id, int32_type,
            mir::FormatSpec(
                value::FormatKind::kDecimal, mir::FormatModifiers{})));
    items.emplace_back(mir::RuntimePrintLiteral{.text = verdict.suffix_text});
  }

  // The verdict text is fixed-format decimal, so no %t directive is possible
  // and the time-unit power is unread.
  const mir::ExprId items_array =
      scope.AddExpr(BuildPrintItemsArray(unit, scope, items, 0));

  // `self` lives in the enclosing closure body; this then-scope sits one level
  // deeper, so the read climbs one hop -- matching the count_var read above.
  const mir::ExprId self_read = scope.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 1}, .var = self_binding},
          .type = unit.builtins.self_pointer});
  const mir::ExprId services = scope.AddExpr(
      mir::MakeServicesCallExpr(self_read, unit.builtins.services));

  const support::SystemSubroutineDesc* warning_desc =
      support::FindSystemSubroutine("$warning");
  if (warning_desc == nullptr) {
    throw InternalError(
        "BuildDiagnosticThenScope: $warning system subroutine not registered");
  }

  std::vector<mir::ExprId> args{services, items_array};
  const mir::ExprId diag_call_id = scope.AddExpr(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::SystemSubroutineCallee{.id = warning_desc->id},
                  .arguments = std::move(args)},
          .type = unit.builtins.void_type});
  scope.AppendStmt(mir::ExprStmt{.expr = diag_call_id});
  return scope;
}

auto BuildUniqueCheckClosure(
    ModuleLowerer& module, const WalkFrame& wrapper_frame,
    mir::ProceduralScope& wrapper, hir::UniquePriorityCheck check,
    const std::vector<mir::ProceduralVarId>& snapshot_vars)
    -> mir::ClosureExpr {
  mir::ClosureExpr closure;
  closure.body = std::make_unique<mir::ProceduralScope>();
  auto& body = *closure.body;

  const mir::TypeId int32_type = module.Unit().builtins.int32;
  const mir::TypeId self_ptr_type = module.Unit().builtins.self_pointer;
  const mir::ProceduralVarId self_binding = body.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "self", .type = self_ptr_type});
  const mir::ExprId outer_self_read =
      wrapper.AddExpr(BuildSelfRefExpr(wrapper_frame, self_ptr_type));
  closure.captures.emplace_back(
      mir::ByValueCapture{.value = outer_self_read, .binding = self_binding});

  std::vector<mir::ExprId> inner_reads;
  inner_reads.reserve(snapshot_vars.size());
  for (std::size_t i = 0; i < snapshot_vars.size(); ++i) {
    const mir::TypeId snap_type =
        wrapper.GetProceduralVar(snapshot_vars[i]).type;
    const mir::ExprId outer_read_id = wrapper.AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = mir::ProceduralHops{.value = 0},
                    .var = snapshot_vars[i]},
            .type = snap_type});

    const mir::ProceduralVarId binding = body.AddProceduralVar(
        mir::ProceduralVarDecl{
            .name = std::format("_lyra_unique_bind_{}", i), .type = snap_type});
    closure.captures.emplace_back(
        mir::ByValueCapture{.value = outer_read_id, .binding = binding});

    const mir::ExprId inner_read_id = body.AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = mir::ProceduralHops{.value = 0}, .var = binding},
            .type = snap_type});
    inner_reads.push_back(inner_read_id);
  }

  const mir::ProceduralVarId count_var = body.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "_lyra_unique_count", .type = int32_type});

  const mir::ExprId zero_init_id =
      body.AddExpr(mir::MakeInt32Literal(int32_type, 0));
  body.AppendStmt(
      mir::ProceduralVarDeclStmt{
          .target =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = count_var},
          .init = zero_init_id});

  for (const mir::ExprId bit_read : inner_reads) {
    const mir::ExprId one_lit =
        body.AddExpr(mir::MakeInt32Literal(int32_type, 1));
    const mir::ExprId zero_lit =
        body.AddExpr(mir::MakeInt32Literal(int32_type, 0));
    const mir::ExprId cond_value = body.AddExpr(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = bit_read,
                    .then_value = one_lit,
                    .else_value = zero_lit},
            .type = int32_type});
    const mir::ExprId count_read = body.AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = mir::ProceduralHops{.value = 0}, .var = count_var},
            .type = int32_type});
    const mir::ExprId added = body.AddExpr(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = mir::BinaryOp::kAdd,
                    .lhs = count_read,
                    .rhs = cond_value},
            .type = int32_type});
    const mir::ExprId count_target = body.AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = mir::ProceduralHops{.value = 0}, .var = count_var},
            .type = int32_type});
    const mir::ExprId assign = body.AddExpr(
        mir::Expr{
            .data = mir::AssignExpr{.target = count_target, .value = added},
            .type = int32_type});
    body.AppendStmt(mir::ExprStmt{.expr = assign});
  }

  const CheckVerdict verdict = VerdictFor(check, snapshot_vars.size());

  const mir::ExprId final_count_read = body.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = count_var},
          .type = int32_type});
  const mir::ExprId expected_lit = body.AddExpr(
      mir::MakeInt32Literal(
          int32_type, static_cast<std::int64_t>(verdict.expected)));
  const mir::ExprId predicate_id = body.AddExpr(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = verdict.violation_op,
                  .lhs = final_count_read,
                  .rhs = expected_lit},
          .type = int32_type});

  mir::ProceduralScope diag_scope =
      BuildDiagnosticThenScope(module.Unit(), self_binding, count_var, verdict);

  const mir::ProceduralScopeId diag_scope_id =
      body.AddChildScope(std::move(diag_scope));

  body.AppendStmt(
      mir::IfStmt{
          .condition = predicate_id,
          .then_scope = diag_scope_id,
          .else_scope = std::nullopt});

  return closure;
}

}  // namespace

auto BuildDeferredCheckCascade(
    ModuleLowerer& module, WalkFrame frame, mir::ProceduralScope wrapper,
    std::vector<DeferredCheckBranch> branches,
    std::optional<mir::ProceduralScope> tail_else,
    hir::UniquePriorityCheck check, std::optional<std::string> outer_label)
    -> mir::Stmt {
  const mir::TypeId void_type = module.Unit().builtins.void_type;
  const mir::TypeId int32_type = module.Unit().builtins.int32;

  // SnapshotPredicate / BuildDefaultValueExpr append to wrapper, so
  // route through a wrapper-local frame; the cascade levels each derive their
  // own local frames below. The wrapper itself sits one procedural scope
  // deeper than the caller, so any read of an enclosing binding (e.g. the
  // process body's `self`) climbs through that extra level.
  const WalkFrame wrapper_frame = frame.WithProceduralScope(&wrapper).Deeper();

  std::vector<mir::ProceduralVarId> snapshot_vars;
  snapshot_vars.reserve(branches.size());
  for (std::size_t i = 0; i < branches.size(); ++i) {
    const mir::TypeId predicate_type =
        wrapper.GetExpr(branches[i].predicate).type;
    snapshot_vars.push_back(SnapshotPredicate(
        module, wrapper_frame, wrapper, i, predicate_type,
        branches[i].predicate));
  }

  mir::ClosureExpr closure = BuildUniqueCheckClosure(
      module, wrapper_frame, wrapper, check, snapshot_vars);
  const mir::ExprId closure_expr_id =
      wrapper.AddExpr(mir::Expr{.data = std::move(closure), .type = void_type});

  const mir::DeferredCheckSiteId site_id =
      module.Unit().AllocateDeferredCheckSiteId();
  const mir::ExprId submit_expr_id = wrapper.AddExpr(
      mir::Expr{
          .data =
              mir::RuntimeCallExpr{
                  .call =
                      mir::RuntimeSubmitObservedCall{
                          .site_id = site_id, .closure = closure_expr_id}},
          .type = void_type});
  wrapper.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ExprStmt{.expr = submit_expr_id}});

  // Cascade level i sits i scopes deeper than wrapper, so the read uses hops=i.
  std::optional<mir::ProceduralScope> tail = std::move(tail_else);
  for (std::size_t i = branches.size(); i-- > 1;) {
    mir::ProceduralScope level_scope;
    const mir::ExprId cond_read = level_scope.AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops =
                        mir::ProceduralHops{
                            .value = static_cast<std::uint32_t>(i)},
                    .var = snapshot_vars[i]},
            .type = int32_type});

    const mir::ProceduralScopeId body_scope_id =
        level_scope.AddChildScope(std::move(branches[i].body));
    std::optional<mir::ProceduralScopeId> else_scope_id;
    if (tail.has_value()) {
      else_scope_id = level_scope.AddChildScope(std::move(*tail));
    }

    level_scope.AppendStmt(
        mir::Stmt{
            .label = std::nullopt,
            .data = mir::IfStmt{
                .condition = cond_read,
                .then_scope = body_scope_id,
                .else_scope = else_scope_id}});
    tail = std::move(level_scope);
  }

  if (!branches.empty()) {
    const mir::ExprId cond_read0 = wrapper.AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = mir::ProceduralHops{.value = 0},
                    .var = snapshot_vars[0]},
            .type = int32_type});

    const mir::ProceduralScopeId body0_id =
        wrapper.AddChildScope(std::move(branches[0].body));
    std::optional<mir::ProceduralScopeId> else0_id;
    if (tail.has_value()) {
      else0_id = wrapper.AddChildScope(std::move(*tail));
    }

    wrapper.AppendStmt(
        mir::Stmt{
            .label = std::nullopt,
            .data = mir::IfStmt{
                .condition = cond_read0,
                .then_scope = body0_id,
                .else_scope = else0_id}});
  }

  const mir::ProceduralScopeId wrapper_scope_id =
      frame.current_procedural_scope->AddChildScope(std::move(wrapper));

  return mir::Stmt{
      .label = std::move(outer_label),
      .data = mir::BlockStmt{.scope = wrapper_scope_id}};
}

auto LowerUniqueIfStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::IfStmt& root) -> diag::Result<mir::Stmt> {
  const auto& hir_proc = process.HirBody();
  const auto cascade = FlattenUniqueCascade(hir_proc, root);

  mir::ProceduralScope wrapper;
  const WalkFrame wrapper_frame = frame.WithProceduralScope(&wrapper).Deeper();

  // Lower each branch condition into wrapper; these are the predicates
  // that BuildDeferredCheckCascade will snapshot.
  std::vector<mir::ExprId> predicates;
  predicates.reserve(cascade.conditions.size());
  for (const hir::ExprId hir_cond : cascade.conditions) {
    auto cond_or =
        process.LowerExpr(hir_proc.exprs.at(hir_cond.value), wrapper_frame);
    if (!cond_or) return std::unexpected(std::move(cond_or.error()));
    predicates.push_back(wrapper.AddExpr(*std::move(cond_or)));
  }

  // Lower each body into its own child scope. Cascade level i sits i scopes
  // deeper than wrapper, so each body lowering descends i extra frames before
  // opening its own child scope (LowerStmtIntoChildScope adds one more).
  auto deeper_by = [](WalkFrame f, std::size_t extras) {
    for (std::size_t i = 0; i < extras; ++i) {
      f = f.Deeper();
    }
    return f;
  };

  std::vector<DeferredCheckBranch> branches;
  branches.reserve(cascade.bodies.size());
  for (std::size_t i = 0; i < cascade.bodies.size(); ++i) {
    auto body_or = LowerStmtIntoChildScope(
        process, deeper_by(wrapper_frame, i), cascade.bodies[i]);
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    branches.push_back(
        DeferredCheckBranch{
            .predicate = predicates[i], .body = std::move(*body_or)});
  }

  // The trailing else sits at the same MIR depth as the last branch body
  // (their if-then-else share an enclosing level scope), so the lowering
  // walks one fewer level than `bodies.size()` extras.
  std::optional<mir::ProceduralScope> tail_scope;
  if (cascade.tail_else.has_value()) {
    const WalkFrame tail_enter_frame =
        cascade.bodies.empty()
            ? wrapper_frame
            : deeper_by(wrapper_frame, cascade.bodies.size() - 1);
    auto tail_or =
        LowerStmtIntoChildScope(process, tail_enter_frame, *cascade.tail_else);
    if (!tail_or) return std::unexpected(std::move(tail_or.error()));
    tail_scope = std::move(*tail_or);
  }

  return BuildDeferredCheckCascade(
      process.Module(), frame, std::move(wrapper), std::move(branches),
      std::move(tail_scope), *root.check, std::move(label));
}

}  // namespace lyra::lowering::hir_to_mir
