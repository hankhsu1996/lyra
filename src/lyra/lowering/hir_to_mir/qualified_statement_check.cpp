#include "lyra/lowering/hir_to_mir/qualified_statement_check.hpp"

#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
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
  const mir::ExprId text_id =
      block.exprs.Add(BuildFormatCallExpr(unit, block, items_array));
  const mir::ExprId diagnostic_id =
      block.exprs.Add(BuildDiagnosticCallExpr(unit, block));
  const mir::ExprId origin_id =
      BuildStringValueExpr(unit, block, std::move(origin));
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(BuildReportCallExpr(
              unit, support::BuiltinFn::kEmitWarning, diagnostic_id, origin_id,
              text_id))});
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
                          .target = support::BuiltinFn::kSubmitViolationReport,
                          .receiver = runtime_id},
                  .arguments = {body_id}},
          .type = unit_lowerer.Unit().builtins.void_type});
  block.AppendStmt(mir::ExprStmt{.expr = submit_id});
}

// Whether one arm held, frozen at check time as a bit: the local holding it,
// and the synthesized origin a deferred body forwards it through.
struct HeldArm {
  mir::LocalId local;
  BindingOriginId origin;
};

// The body the Observed region runs: counts the arms that held and reports
// where more than one did. A uniqueness violation is exactly that count
// exceeding one, for `unique` and `unique0` alike; the two differ in whether
// totality is also asserted, which this body does not decide (LRM 12.4.2,
// 12.5.3).
auto BuildUniquenessReportBody(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    hir::UniquePriorityCheck check, QualifiedArmKind arm_kind,
    std::span<const HeldArm> arms, std::string origin) -> mir::Expr {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::TypeId int_type = unit.builtins.int_type;
  ClosureBuilder closure(unit, frame);
  mir::Block& body = closure.Body();

  const mir::LocalId count = closure.Bindings().DeclareAnonymous(int_type);
  const auto read_count = [&](mir::Block& block) {
    return block.exprs.Add(mir::MakeLocalRefExpr(count, int_type));
  };
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = count, .init = BuildIntLiteral(unit, body, 0)});
  for (const HeldArm& arm : arms) {
    const mir::ExprId held = body.exprs.Add(closure.Bindings().MakeReadExpr(
        closure.Bindings().EnsureCarrier(arm.origin), body));
    const mir::ExprId one_if_held = body.exprs.Add(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition = ReduceToCondition(unit, body, held),
                    .then_value = BuildIntLiteral(unit, body, 1),
                    .else_value = BuildIntLiteral(unit, body, 0)},
            .type = int_type});
    const mir::ExprId counted = body.exprs.Add(MakeBinary(
        unit, body, mir::BinaryOp::kAdd, read_count(body), one_if_held,
        int_type));
    body.AppendStmt(
        mir::ExprStmt{
            .expr = body.exprs.Add(
                mir::MakeAssignExpr(
                    unit.builtins, read_count(body), counted))});
  }

  mir::Block report;
  std::vector<mir::RuntimePrintItem> items;
  items.emplace_back(
      mir::RuntimePrintLiteral{
          .text = std::format("{} violation: ", KeywordOf(check))});
  items.emplace_back(
      mir::RuntimePrintValue(
          read_count(report), int_type,
          mir::FormatSpec(
              value::FormatKind::kDecimal, mir::FormatModifiers{})));
  items.emplace_back(
      mir::RuntimePrintLiteral{
          .text = std::format(
              " of {} {} matched", arms.size(), NounFor(arm_kind).many)});
  AppendReportEmit(unit, report, std::move(items), std::move(origin));

  const mir::ExprId violated = body.exprs.Add(MakeBinary(
      unit, body, mir::BinaryOp::kGreaterThan, read_count(body),
      BuildIntLiteral(unit, body, 1), unit.builtins.bit1));
  body.AppendStmt(
      mir::IfStmt{
          .condition = ReduceToCondition(unit, body, violated),
          .then_scope = body.child_scopes.Add(std::move(report)),
          .else_scope = std::nullopt});
  return closure.BuildVoid();
}

// The arm a statement asserting totality runs when none of its own held. The
// body it submits reads nothing the statement computed.
auto BuildTotalityReportScope(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    hir::UniquePriorityCheck check, QualifiedArmKind arm_kind,
    diag::SourceSpan span) -> mir::Block {
  mir::Block scope;
  ClosureBuilder closure(unit_lowerer.Unit(), frame.WithBlock(&scope));
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

}  // namespace

auto AssertionsOf(
    std::optional<hir::UniquePriorityCheck> check, bool has_catch_all)
    -> QualifiedAssertions {
  if (!check.has_value()) {
    return {.uniqueness = false, .totality = false};
  }
  switch (*check) {
    case hir::UniquePriorityCheck::kUnique:
      return {.uniqueness = true, .totality = !has_catch_all};
    case hir::UniquePriorityCheck::kUnique0:
      return {.uniqueness = true, .totality = false};
    case hir::UniquePriorityCheck::kPriority:
      return {.uniqueness = false, .totality = !has_catch_all};
  }
  throw InternalError("AssertionsOf: unknown HIR UniquePriorityCheck");
}

auto BuildFallThrough(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    std::optional<mir::Block> catch_all,
    std::optional<hir::UniquePriorityCheck> check, QualifiedArmKind arm_kind,
    diag::SourceSpan span) -> std::optional<mir::Block> {
  if (AssertionsOf(check, catch_all.has_value()).totality) {
    return BuildTotalityReportScope(
        unit_lowerer, frame, *check, arm_kind, span);
  }
  return catch_all;
}

auto BuildUniquenessCheck(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    std::span<const mir::ExprId> held, hir::UniquePriorityCheck check,
    QualifiedArmKind arm_kind, diag::SourceSpan span)
    -> std::vector<mir::LocalId> {
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;

  std::vector<HeldArm> arms;
  std::vector<mir::LocalId> locals;
  arms.reserve(held.size());
  locals.reserve(held.size());
  for (const mir::ExprId arm_held : held) {
    const BindingOriginId origin =
        BindingOriginId::Synthesized(unit_lowerer.NextSynthesizedSite(), 0);
    const mir::LocalId local = SnapshotExprToLocal(
        unit_lowerer, frame, block, unit.builtins.bit1,
        ConditionAsBit(unit, block, arm_held), origin);
    arms.push_back({.local = local, .origin = origin});
    locals.push_back(local);
  }
  SubmitToObservedRegion(
      unit_lowerer, block,
      BuildUniquenessReportBody(
          unit_lowerer, frame, check, arm_kind, arms,
          FormatRuntimeOriginString(span, unit_lowerer.SourceManager())));
  return locals;
}

}  // namespace lyra::lowering::hir_to_mir
