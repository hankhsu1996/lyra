#include "lyra/lowering/hir_to_mir/qualified_statement_check.hpp"

#include <cstddef>
#include <expected>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/pattern.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// What a report calls one arm of the statement and several of them.
struct ArmNoun {
  std::string_view one;
  std::string_view many;
};

auto NounFor(QualifiedArmKind arm_kind) -> ArmNoun {
  switch (arm_kind) {
    case QualifiedArmKind::kCondition:
      return {.one = "condition", .many = "conditions"};
    case QualifiedArmKind::kCaseItem:
      return {.one = "case item", .many = "case items"};
  }
  throw InternalError("NounFor: unknown qualified arm kind");
}

auto KeywordOf(hir::UniquePriorityCheck check) -> std::string_view {
  switch (check) {
    case hir::UniquePriorityCheck::kUnique:
      return "unique";
    case hir::UniquePriorityCheck::kUnique0:
      return "unique0";
    case hir::UniquePriorityCheck::kPriority:
      return "priority";
  }
  throw InternalError("KeywordOf: unknown HIR UniquePriorityCheck");
}

// Formats the report text already staged in `block` and hands it to the
// diagnostic broker at warning severity (LRM 20.10). `origin` is the qualified
// statement's own location, so the dispatcher attributes and rate-limits by
// where the statement is written rather than by where the report matured.
void AppendReportEmit(
    mir::CompilationUnit& unit, mir::Block& block,
    std::vector<mir::RuntimePrintItem> items, std::string origin) {
  // The text is fixed-format decimal, so no %t directive is possible and the
  // time-unit power is unread.
  const mir::ExprId items_array =
      block.exprs.Add(BuildPrintItemsArray(unit, block, items, 0));
  const mir::ExprId runtime_id =
      block.exprs.Add(mir::MakeCurrentRuntimeCallExpr(unit.builtins.effects));
  const mir::ExprId text_id = block.exprs.Add(
      BuildFormatCallExpr(unit, block, runtime_id, items_array));
  const mir::ExprId diagnostic_id =
      block.exprs.Add(BuildDiagnosticCallExpr(unit, runtime_id));
  const mir::ExprId origin_id =
      BuildStringValueExpr(unit, block, std::move(origin));
  const mir::ExprId emit_call_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kEmitWarning},
                  .arguments = {diagnostic_id, origin_id, text_id}},
          .type = unit.builtins.void_type});
  block.AppendStmt(mir::ExprStmt{.expr = emit_call_id});
}

// A pending violation report is scheduled where the check was decided and
// matures a region later (LRM 12.4.2.1), so every report is reached through a
// body the region runs rather than emitted in place.
void SubmitToObservedRegion(
    UnitLowerer& unit_lowerer, mir::Block& block, mir::Expr body) {
  const mir::ExprId body_id = block.exprs.Add(std::move(body));
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  const mir::ExprId submit_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kSubmitObserved},
                  .arguments = {runtime_id, body_id}},
          .type = unit_lowerer.Unit().builtins.void_type});
  block.AppendStmt(mir::ExprStmt{.expr = submit_id});
}

// A predicate snapshot bound for capture: the wrapper local holding the value
// frozen at check time, plus the synthesized origin a deferred body forwards it
// through.
struct SnapshotBinding {
  mir::LocalId local;
  BindingOriginId origin;
};

auto SnapshotPredicate(
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::Block& wrapper,
    std::size_t index, mir::TypeId predicate_type,
    mir::ExprId predicate_expr_id) -> SnapshotBinding {
  const BindingOriginId origin =
      BindingOriginId::Synthesized(unit_lowerer.NextSynthesizedSite(), 0);
  const mir::LocalId local = SnapshotExprToLocal(
      unit_lowerer, frame, wrapper, std::format("_lyra_unique_cond_{}", index),
      predicate_type, predicate_expr_id, origin);
  return {.local = local, .origin = origin};
}

// Counts the arms that held and reports where more than one did. A uniqueness
// violation is exactly that count exceeding one, for `unique` and `unique0`
// alike; the two differ in whether totality is also asserted, which this body
// does not decide (LRM 12.4.2, 12.5.3).
auto BuildUniquenessCheckBody(
    UnitLowerer& unit_lowerer, const WalkFrame& wrapper_frame,
    hir::UniquePriorityCheck check, QualifiedArmKind arm_kind,
    const std::vector<SnapshotBinding>& snapshot_vars, std::string origin)
    -> mir::Expr {
  ClosureBuilder closure(unit_lowerer.Unit(), wrapper_frame);
  mir::Block& body = closure.Body();

  const mir::TypeId int_type = unit_lowerer.Unit().builtins.int_type;

  std::vector<mir::ExprId> inner_reads;
  inner_reads.reserve(snapshot_vars.size());
  for (const SnapshotBinding& snap : snapshot_vars) {
    const BodyBindingRef ref = closure.Bindings().EnsureCarrier(snap.origin);
    inner_reads.push_back(
        body.exprs.Add(closure.Bindings().MakeReadExpr(ref, body)));
  }

  const mir::LocalId count_var = closure.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_unique_count", .type = int_type});

  const mir::ExprId zero_init_id =
      BuildIntLiteral(unit_lowerer.Unit(), body, 0);
  body.AppendStmt(
      mir::LocalDeclStmt{.target = count_var, .init = zero_init_id});

  for (const mir::ExprId bit_read : inner_reads) {
    const mir::ExprId one_lit = BuildIntLiteral(unit_lowerer.Unit(), body, 1);
    const mir::ExprId zero_lit = BuildIntLiteral(unit_lowerer.Unit(), body, 0);
    const mir::ExprId cond_value = body.exprs.Add(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition =
                        ReduceToCondition(unit_lowerer.Unit(), body, bit_read),
                    .then_value = one_lit,
                    .else_value = zero_lit},
            .type = int_type});
    const mir::ExprId count_read =
        body.exprs.Add(mir::MakeLocalRefExpr(count_var, int_type));
    const mir::ExprId added = body.exprs.Add(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = mir::BinaryOp::kAdd,
                    .lhs = count_read,
                    .rhs = cond_value},
            .type = int_type});
    const mir::ExprId count_target =
        body.exprs.Add(mir::MakeLocalRefExpr(count_var, int_type));
    const mir::ExprId assign = body.exprs.Add(
        mir::Expr{
            .data = mir::AssignExpr{.target = count_target, .value = added},
            .type = int_type});
    body.AppendStmt(mir::ExprStmt{.expr = assign});
  }

  const mir::ExprId final_count_read =
      body.exprs.Add(mir::MakeLocalRefExpr(count_var, int_type));
  const mir::ExprId one_lit = BuildIntLiteral(unit_lowerer.Unit(), body, 1);
  const mir::ExprId violated_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kGreaterThan,
                  .lhs = final_count_read,
                  .rhs = one_lit},
          .type = int_type});

  const ArmNoun noun = NounFor(arm_kind);
  mir::Block report;
  std::vector<mir::RuntimePrintItem> items;
  items.emplace_back(
      mir::RuntimePrintLiteral{
          .text = std::format("{} violation: ", KeywordOf(check))});
  const mir::ExprId count_in_report =
      report.exprs.Add(mir::MakeLocalRefExpr(count_var, int_type));
  items.emplace_back(
      mir::RuntimePrintValue(
          count_in_report, int_type,
          mir::FormatSpec(
              value::FormatKind::kDecimal, mir::FormatModifiers{})));
  items.emplace_back(
      mir::RuntimePrintLiteral{
          .text = std::format(
              " of {} {} matched", snapshot_vars.size(), noun.many)});
  AppendReportEmit(
      unit_lowerer.Unit(), report, std::move(items), std::move(origin));

  const mir::BlockId report_scope_id = body.child_scopes.Add(std::move(report));
  body.AppendStmt(
      mir::IfStmt{
          .condition =
              ReduceToCondition(unit_lowerer.Unit(), body, violated_id),
          .then_scope = report_scope_id,
          .else_scope = std::nullopt});

  return closure.BuildVoid();
}

}  // namespace

auto AssertionsOf(hir::UniquePriorityCheck check, bool has_catch_all)
    -> QualifiedAssertions {
  switch (check) {
    case hir::UniquePriorityCheck::kUnique:
      return {.uniqueness = true, .totality = !has_catch_all};
    case hir::UniquePriorityCheck::kUnique0:
      return {.uniqueness = true, .totality = false};
    case hir::UniquePriorityCheck::kPriority:
      return {.uniqueness = false, .totality = !has_catch_all};
  }
  throw InternalError("AssertionsOf: unknown HIR UniquePriorityCheck");
}

auto SeriesOf(const hir::ProceduralBody& proc, const hir::IfStmt& root)
    -> QualifiedIfSeries {
  QualifiedIfSeries out;
  out.arms.push_back(&root);
  std::optional<hir::StmtId> cur_else = root.else_stmt;
  while (cur_else.has_value()) {
    const hir::Stmt& s = proc.stmts.Get(*cur_else);
    const auto* nested = std::get_if<hir::IfStmt>(&s.data);
    if (nested == nullptr || nested->check.has_value()) {
      out.else_arm = cur_else;
      break;
    }
    out.arms.push_back(nested);
    cur_else = nested->else_stmt;
  }
  return out;
}

auto BuildTotalityReportScope(
    UnitLowerer& unit_lowerer, WalkFrame frame, hir::UniquePriorityCheck check,
    QualifiedArmKind arm_kind, diag::SourceSpan span) -> mir::Block {
  mir::Block scope;
  const WalkFrame scope_frame = frame.WithBlock(&scope);

  ClosureBuilder closure(unit_lowerer.Unit(), scope_frame);
  std::vector<mir::RuntimePrintItem> items;
  items.emplace_back(
      mir::RuntimePrintLiteral{
          .text = std::format(
              "{} violation: no {} matched", KeywordOf(check),
              NounFor(arm_kind).one)});
  AppendReportEmit(
      unit_lowerer.Unit(), closure.Body(), std::move(items),
      FormatRuntimeOriginString(span, unit_lowerer.SourceManager()));

  SubmitToObservedRegion(unit_lowerer, scope, closure.BuildVoid());
  return scope;
}

auto BuildUniquenessCheckCascade(
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::Block wrapper,
    std::vector<QualifiedArm> arms, std::optional<mir::Block> fall_through,
    hir::UniquePriorityCheck check, QualifiedArmKind arm_kind,
    std::optional<std::string> outer_label, diag::SourceSpan span)
    -> mir::Stmt {
  const mir::TypeId int_type = unit_lowerer.Unit().builtins.int_type;

  // The snapshots land in the wrapper, so they are taken through a
  // wrapper-local frame; the cascade levels each derive their own below. They
  // are body-locals of this callable, so every read names them directly with no
  // nesting bookkeeping.
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  std::vector<SnapshotBinding> snapshot_vars;
  snapshot_vars.reserve(arms.size());
  for (std::size_t i = 0; i < arms.size(); ++i) {
    const mir::TypeId predicate_type =
        wrapper.exprs.Get(arms[i].predicate).type;
    snapshot_vars.push_back(SnapshotPredicate(
        unit_lowerer, wrapper_frame, wrapper, i, predicate_type,
        arms[i].predicate));
  }

  SubmitToObservedRegion(
      unit_lowerer, wrapper,
      BuildUniquenessCheckBody(
          unit_lowerer, wrapper_frame, check, arm_kind, snapshot_vars,
          FormatRuntimeOriginString(span, unit_lowerer.SourceManager())));

  // The dispatch is folded from the innermost arm outward, each level becoming
  // the one below it's else, so what the wrapper ends up carrying is a single
  // arm reached however the snapshots came out.
  std::optional<mir::Block> tail = std::move(fall_through);
  for (std::size_t i = arms.size(); i-- > 0;) {
    mir::Block level_block;
    const mir::ExprId cond_read = level_block.exprs.Add(
        mir::MakeLocalRefExpr(snapshot_vars[i].local, int_type));

    const mir::BlockId body_scope_id =
        level_block.child_scopes.Add(std::move(arms[i].body));
    std::optional<mir::BlockId> else_scope_id;
    if (tail.has_value()) {
      else_scope_id = level_block.child_scopes.Add(std::move(*tail));
    }

    level_block.AppendStmt(
        mir::IfStmt{
            .condition =
                ReduceToCondition(unit_lowerer.Unit(), level_block, cond_read),
            .then_scope = body_scope_id,
            .else_scope = else_scope_id});
    tail = std::move(level_block);
  }

  if (tail.has_value()) {
    const mir::BlockId tail_id = wrapper.child_scopes.Add(std::move(*tail));
    wrapper.AppendStmt(mir::BlockStmt{.scope = tail_id});
  }

  const mir::BlockId wrapper_scope_id =
      frame.current_block->child_scopes.Add(std::move(wrapper));

  return mir::Stmt{
      .label = std::move(outer_label),
      .data = mir::BlockStmt{.scope = wrapper_scope_id}};
}

auto LowerIfFallThrough(
    ProcessLowerer& process, WalkFrame frame, const QualifiedIfSeries& series,
    hir::UniquePriorityCheck check, diag::SourceSpan span)
    -> diag::Result<std::optional<mir::Block>> {
  const bool has_catch_all = series.else_arm.has_value();
  if (has_catch_all) {
    auto else_or = LowerStmtIntoChildScope(process, frame, *series.else_arm);
    if (!else_or) return std::unexpected(std::move(else_or.error()));
    return std::optional<mir::Block>(std::move(*else_or));
  }
  if (!AssertionsOf(check, has_catch_all).totality) {
    return std::optional<mir::Block>{};
  }
  return std::optional<mir::Block>(BuildTotalityReportScope(
      process.Owner(), frame, check, QualifiedArmKind::kCondition, span));
}

auto LowerUniquenessIfSeries(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const QualifiedIfSeries& series, hir::UniquePriorityCheck check,
    diag::SourceSpan span) -> diag::Result<mir::Stmt> {
  const mir::TypeId bit1_type = process.Owner().Unit().builtins.bit1;

  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  // LRM 12.4.2 evaluates every arm's predicate before any arm's statement
  // runs, so each arm's clause chain is emitted here with an empty then-arm
  // and its outcome left in a flag. Reducing an arm to a flag is what makes
  // arm shape invisible to the check: one clause or several, pattern or not,
  // every arm answers the same question the same way.
  std::vector<QualifiedArm> arms;
  arms.reserve(series.arms.size());
  for (const hir::IfStmt* arm : series.arms) {
    const mir::LocalId held = wrapper_frame.bindings->DeclareAnonymous(
        mir::LocalDecl{.name = "_lyra_arm_held", .type = bit1_type});
    const mir::ExprId not_held =
        BuildBit1Literal(process.Owner().Unit(), wrapper, false);
    wrapper.AppendStmt(mir::LocalDeclStmt{.target = held, .init = not_held});

    auto chain_or = BuildClauseChainIf(
        process, wrapper_frame,
        std::span<const hir::ConditionClause>{arm->conditions}, held,
        [](WalkFrame) -> diag::Result<void> { return {}; });
    if (!chain_or) return std::unexpected(std::move(chain_or.error()));
    wrapper.AppendStmt(*std::move(chain_or));

    auto body_or =
        LowerStmtIntoChildScope(process, wrapper_frame, arm->then_stmt);
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    arms.push_back(
        QualifiedArm{
            .predicate =
                wrapper.exprs.Add(mir::MakeLocalRefExpr(held, bit1_type)),
            .body = std::move(*body_or)});
  }

  auto fall_through_or =
      LowerIfFallThrough(process, wrapper_frame, series, check, span);
  if (!fall_through_or) {
    return std::unexpected(std::move(fall_through_or.error()));
  }

  return BuildUniquenessCheckCascade(
      process.Owner(), frame, std::move(wrapper), std::move(arms),
      std::move(*fall_through_or), check, QualifiedArmKind::kCondition,
      std::move(label), span);
}

}  // namespace lyra::lowering::hir_to_mir
