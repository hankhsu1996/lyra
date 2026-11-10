#include "lyra/lowering/hir_to_mir/qualified_statement_check.hpp"

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/assertion_control.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/callable_bounds.hpp"

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

// Formats the report text and hands it to the diagnostic broker at warning
// severity (LRM 20.10), in the block `frame` builds into, placed where that
// block builds from -- the qualified statement -- so the report names where
// the statement is written, and its scope, rather than where the report
// matured.
void AppendReportEmit(
    mir::CompilationUnit& unit, const WalkFrame& frame,
    std::vector<RuntimePrintItem> items) {
  mir::Builder block = frame.Builder();
  // The text is fixed-format decimal, so no %t directive is possible and the
  // time-unit power is unread.
  const mir::ExprId items_array =
      block.Add(BuildPrintItemsArray(unit, block, items, 0));
  const mir::ExprId text_id =
      block.Add(BuildFormatCallExpr(unit, block, items_array));
  const mir::ExprId diagnostic_id =
      block.Add(BuildDiagnosticCallExpr(unit, block));
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(BuildReportCallExpr(
              unit, frame, support::kWarningReport, diagnostic_id, text_id))});
}

// A pending violation report is scheduled where the check was decided and
// matures a region later (LRM 12.4.2.1), so every report is reached through a
// body the region runs rather than emitted in place. It is made where the
// statement's assertion control keeps it, which queues it only while checking
// is on and keeps it until it matures on the condition that the process
// raising it reaches no flush point, and the control no Kill, first (LRM
// 20.11).
void KeepViolationReport(
    ProcessLowerer& process, const WalkFrame& frame,
    hir::ControlledAssertionId control, mir::Expr body) {
  mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Builder block = frame.Builder();
  mir::Expr keeper =
      ControlAddress(unit, block, frame, process.ControlCellOf(control));
  const mir::ExprId kept = block.Add(
      BuildEmplacedClosure(unit, frame, std::move(keeper), std::move(body)));
  block.AppendStmt(mir::ExprStmt{.expr = kept});
}

// The scope a statement runs where it found a violation: it keeps `text` as a
// violation report, and the body it submits reads nothing the
// statement computed.
auto BuildReportScope(
    ProcessLowerer& process, const WalkFrame& frame,
    hir::ControlledAssertionId control, std::string text) -> mir::Block {
  mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block scope;
  const WalkFrame scope_frame = frame.WithBlock(&scope);
  ClosureBuilder closure(unit, scope_frame, support::ClosureKind::kFn);
  std::vector<RuntimePrintItem> items;
  items.emplace_back(RuntimePrintLiteral{.text = std::move(text)});
  AppendReportEmit(unit, closure.Frame(), std::move(items));
  KeepViolationReport(process, scope_frame, control, closure.BuildVoid());
  return scope;
}

}  // namespace

auto AssertionsOf(std::optional<hir::QualifiedCheck> check, bool has_catch_all)
    -> QualifiedAssertions {
  if (!check.has_value()) {
    return {.uniqueness = false, .totality = false};
  }
  switch (check->keyword) {
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
    ProcessLowerer& process, const WalkFrame& frame,
    std::optional<mir::Block> catch_all,
    std::optional<hir::QualifiedCheck> check, QualifiedArmKind arm_kind)
    -> std::optional<mir::Block> {
  if (AssertionsOf(check, catch_all.has_value()).totality) {
    return BuildReportScope(
        process, frame, check->control,
        base::DiagMessage(
            "'{}' violation; no {} matched", KeywordOf(check->keyword),
            NounFor(arm_kind).one)
            .TakeText());
  }
  return catch_all;
}

void BuildUniquenessCheck(
    ProcessLowerer& process, const WalkFrame& frame,
    std::span<const mir::LocalId> held, const hir::QualifiedCheck& check,
    QualifiedArmKind arm_kind) {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Builder block = frame.Builder();
  const mir::TypeId int_type = unit.builtins.int_type;

  const mir::LocalId count =
      DeclareLocal(frame, mir::BuildIntLiteral(unit, block, 0));
  for (const mir::LocalId arm_held : held) {
    const mir::ExprId one_if_held = block.Add(
        mir::Expr{
            .data =
                mir::ConditionalExpr{
                    .condition =
                        block.ReadLocal(arm_held, unit.builtins.machine_bool),
                    .then_value = mir::BuildIntLiteral(unit, block, 1),
                    .else_value = mir::BuildIntLiteral(unit, block, 0)},
            .type = int_type});
    AssignLocal(
        unit, block, count,
        block.Add(
            mir::MakeBinary(
                unit, block.Body(), mir::BinaryOp::kAdd,
                block.ReadLocal(count, int_type), one_if_held, int_type)));
  }
  const mir::ExprId violated = block.Add(
      mir::MakeBinary(
          unit, block.Body(), mir::BinaryOp::kGreaterThan,
          block.ReadLocal(count, int_type),
          mir::BuildIntLiteral(unit, block, 1), unit.builtins.bit1));
  block.AppendStmt(
      mir::IfStmt{
          .condition = ReduceToCondition(unit, block, violated),
          .then_scope = block.AddScope(BuildReportScope(
              process, frame, check.control,
              base::DiagMessage(
                  "'{}' violation; more than one of the {} {} matched",
                  KeywordOf(check.keyword), held.size(), NounFor(arm_kind).many)
                  .TakeText())),
          .else_scope = std::nullopt});
}

}  // namespace lyra::lowering::hir_to_mir
