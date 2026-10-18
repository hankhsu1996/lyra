#include "lyra/lowering/hir_to_mir/statement/assertions.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The noun a default failure report names. LRM 16.3 gives an assumption the
// same checking an assertion gets, so all the directive decides is which of
// the two the reader is told about.
auto FailureText(hir::AssertionDirective directive) -> base::DiagMessage {
  switch (directive) {
    case hir::AssertionDirective::kAssert:
      return base::DiagMessage("assertion failed");
    case hir::AssertionDirective::kAssume:
      return base::DiagMessage("assumption failed");
  }
  throw InternalError("FailureText: unknown assertion directive");
}

// One arm of an action block, lowered into a scope of its own. LRM 16.3 lets
// either arm be omitted, and an omitted one is an empty scope.
auto LowerArmIntoScope(
    ProcessLowerer& process, WalkFrame frame, std::optional<hir::StmtId> arm)
    -> diag::Result<mir::Block> {
  if (!arm.has_value()) {
    return mir::Block{};
  }
  return LowerStmtIntoChildScope(process, frame, *arm);
}

auto BuildDefaultReportScope(
    ProcessLowerer& process, hir::AssertionDirective directive,
    diag::SourceSpan span) -> mir::Block {
  mir::Block report;
  AppendDefaultReport(process, report, directive, span);
  return report;
}

// The runtime commit a deferred assertion's report rides: an observed (`#0`)
// one matures in Observed and runs in Reactive, a final one in Postponed (LRM
// 16.4). The report is the body its action runs as, so the call carries
// only the runtime handle and that coroutine.
auto DeferredSubmitBuiltin(hir::AssertionTiming timing) -> support::BuiltinFn {
  switch (timing) {
    case hir::AssertionTiming::kObserved:
      return support::BuiltinFn::kSubmitDeferredObserved;
    case hir::AssertionTiming::kFinal:
      return support::BuiltinFn::kSubmitDeferredFinal;
    case hir::AssertionTiming::kSimple:
      break;
  }
  throw InternalError(
      "DeferredSubmitBuiltin: a simple assertion runs inline and submits no "
      "deferred report");
}

// Queues what `fill` writes as a body of its own, which is what a deferred
// action runs as, on the process's deferred report queue, to mature and run a
// region later (LRM 16.4); the queuing is what the outcome's arm runs inline.
// `fill` is handed the body and the frame of the arm, where whatever the
// action is passed by value is evaluated.
template <typename Fill>
auto BuildDeferredSubmit(
    ProcessLowerer& process, WalkFrame frame, hir::AssertionTiming timing,
    Fill fill) -> diag::Result<mir::Block> {
  mir::Block scope;
  const WalkFrame scope_frame = frame.WithBlock(&scope);
  ClosureBuilder body(
      process.Owner().Unit(), scope_frame, support::ClosureKind::kFnOnce);
  if (auto filled = fill(body, scope_frame); !filled) {
    return std::unexpected(std::move(filled.error()));
  }
  const mir::ExprId body_id = scope.exprs.Add(body.BuildCallOnce());
  const mir::ExprId runtime_id =
      scope.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  scope.AppendStmt(
      mir::ExprStmt{
          .expr = scope.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee =
                              mir::Direct{
                                  .target = DeferredSubmitBuiltin(timing),
                                  .receiver = runtime_id},
                          .arguments = {body_id}},
                  .type = process.Owner().Unit().builtins.void_type})});
  return scope;
}

// LRM 16.4: a deferred assertion's pass or fail statement is one subroutine
// call, made in the region the report runs in -- where it may wait, since the
// subroutine can be a task -- with the arguments it is passed by value
// evaluated where the assertion is, on every outcome that selects it, whether
// or not the report is later flushed (16.4.1). One passed by reference is the
// variable as it is when the call runs.
auto LowerDeferredCallInto(
    ProcessLowerer& process, ClosureBuilder& body, const WalkFrame& statement,
    hir::StmtId action) -> diag::Result<void> {
  const hir::Stmt& call_stmt = process.HirBody().stmts.Get(action);
  const auto* call_expr = std::get_if<hir::ExprStmt>(&call_stmt.data);
  const auto* call =
      call_expr == nullptr
          ? nullptr
          : std::get_if<hir::CallExpr>(
                &process.HirBody().exprs.Get(call_expr->expr).data);
  if (call == nullptr) {
    throw InternalError(
        "LowerDeferredCallInto: a deferred assertion's action is one "
        "subroutine "
        "call, which the front end requires (LRM 16.4)");
  }
  std::vector<hir::ExprId> operands;
  for (const std::optional<hir::ExprId>& argument : call->arguments) {
    if (argument.has_value()) operands.push_back(*argument);
  }
  const ArgumentsEvaluatedEarlier arguments{
      .operands = operands, .statement = &statement};
  auto lowered = process.LowerStmt(
      call_stmt,
      body.Frame().WithBodyCanWait(true).WithArgumentsEvaluatedEarlier(
          &arguments));
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  body.Body().AppendStmt(*std::move(lowered));
  return {};
}

// The submit of a deferred arm the source wrote a statement for.
auto BuildDeferredCallSubmit(
    ProcessLowerer& process, WalkFrame frame, hir::AssertionTiming timing,
    hir::StmtId arm) -> diag::Result<mir::Block> {
  return BuildDeferredSubmit(
      process, frame, timing,
      [&](ClosureBuilder& body, const WalkFrame& statement) {
        return LowerDeferredCallInto(process, body, statement, arm);
      });
}

// A false result of a deferred assert or assume runs the fail statement the
// source wrote, nothing where that statement is null, and the tool's own report
// where the source wrote no else clause (LRM 16.4.1).
auto BuildDeferredFailArm(
    ProcessLowerer& process, WalkFrame frame, const hir::AssertStmt& a,
    diag::SourceSpan span) -> diag::Result<mir::Block> {
  if (!a.fail_stmt.has_value()) {
    return BuildDeferredSubmit(
        process, frame, a.timing,
        [&](ClosureBuilder& body, const WalkFrame&) -> diag::Result<void> {
          AppendDefaultReport(process, body.Body(), a.directive, span);
          return {};
        });
  }
  if (!HasRealArm(process.HirBody(), a.fail_stmt)) {
    return mir::Block{};
  }
  return BuildDeferredCallSubmit(process, frame, a.timing, *a.fail_stmt);
}

// LRM 16.3: one evaluation of an immediate cover, and whether it succeeded,
// recorded against the statement's own place, which is what separates one
// coverage goal from another.
void AppendRecordCoverage(
    ProcessLowerer& process, mir::Block& block, diag::SourceSpan span,
    mir::ExprId outcome) {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  const mir::ExprId site_id = BuildStringValueExpr(
      unit, block,
      diag::RuntimeOriginOf(span, process.Owner().SourceManager()));
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee =
                              mir::Direct{
                                  .target = support::BuiltinFn::kRecordCoverage,
                                  .receiver = runtime_id},
                          .arguments = {site_id, outcome}},
                  .type = unit.builtins.void_type})});
}

}  // namespace

void AppendDefaultReport(
    ProcessLowerer& process, mir::Block& block,
    hir::AssertionDirective directive, diag::SourceSpan span) {
  AppendToolReportStmt(
      process.Owner(), block, support::BuiltinFn::kEmitError,
      FailureText(directive), span);
}

auto HasRealArm(const hir::ProceduralBody& body, std::optional<hir::StmtId> arm)
    -> bool {
  if (!arm.has_value()) return false;
  return !std::holds_alternative<hir::EmptyStmt>(body.stmts.Get(*arm).data);
}

auto LowerAssertStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::AssertStmt& a, diag::SourceSpan span)
    -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block& block = *frame.current_block;

  auto cond_or =
      process.LowerExpr(process.HirBody().exprs.Get(a.condition), frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId condition = block.exprs.Add(*std::move(cond_or));

  // LRM 16.4: a deferred assertion evaluates its expression inline and holds
  // the action the outcome selects for a later region.
  if (a.timing != hir::AssertionTiming::kSimple) {
    // A true result with no pass statement runs nothing.
    auto pass_or =
        HasRealArm(process.HirBody(), a.pass_stmt)
            ? BuildDeferredCallSubmit(process, frame, a.timing, *a.pass_stmt)
            : diag::Result<mir::Block>{mir::Block{}};
    if (!pass_or) return std::unexpected(std::move(pass_or.error()));
    auto fail_or = BuildDeferredFailArm(process, frame, a, span);
    if (!fail_or) return std::unexpected(std::move(fail_or.error()));
    return mir::Stmt{
        .label = std::move(label),
        .data = mir::IfStmt{
            .condition = ReduceToCondition(unit, block, condition),
            .then_scope = block.child_scopes.Add(*std::move(pass_or)),
            .else_scope = block.child_scopes.Add(*std::move(fail_or))}};
  }

  auto pass_or = LowerArmIntoScope(process, frame, a.pass_stmt);
  if (!pass_or) return std::unexpected(std::move(pass_or.error()));

  mir::Block fail_scope;
  if (a.fail_stmt.has_value()) {
    auto fail_or = LowerStmtIntoChildScope(process, frame, *a.fail_stmt);
    if (!fail_or) return std::unexpected(std::move(fail_or.error()));
    fail_scope = *std::move(fail_or);
  } else {
    fail_scope = BuildDefaultReportScope(process, a.directive, span);
  }

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::IfStmt{
          .condition = ReduceToCondition(unit, block, condition),
          .then_scope = block.child_scopes.Add(*std::move(pass_or)),
          .else_scope = block.child_scopes.Add(std::move(fail_scope))}};
}

auto LowerCoverStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CoverStmt& c, diag::SourceSpan span) -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const mir::TypeId bool_type = unit.builtins.machine_bool;

  auto cond_or =
      process.LowerExpr(process.HirBody().exprs.Get(c.condition), frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId condition = block.exprs.Add(*std::move(cond_or));

  // LRM 16.3 evaluates the expression once, and both the coverage result and
  // the statement the goal carries read that one outcome.
  const mir::LocalId succeeded = frame.bindings->DeclareAnonymous(bool_type);
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = succeeded,
          .init = ReduceToCondition(unit, block, condition)});
  // A condition consumes a machine boolean, and the outcome already is one.
  const mir::ExprId taken_id = block.ReadLocal(succeeded, bool_type);

  // LRM 16.4: a deferred cover holds back what it records together with its
  // statement, so a flush that withdraws the one withdraws the other and a
  // value met only in passing is not credited.
  if (c.timing != hir::AssertionTiming::kSimple) {
    auto covered_or = BuildDeferredSubmit(
        process, frame, c.timing,
        [&](ClosureBuilder& body,
            const WalkFrame& statement) -> diag::Result<void> {
          AppendRecordCoverage(
              process, body.Body(), span,
              BuildMachineBool(unit, body.Body(), true));
          if (!HasRealArm(process.HirBody(), c.pass_stmt)) return {};
          return LowerDeferredCallInto(process, body, statement, *c.pass_stmt);
        });
    if (!covered_or) return std::unexpected(std::move(covered_or.error()));
    auto missed_or = BuildDeferredSubmit(
        process, frame, c.timing,
        [&](ClosureBuilder& body, const WalkFrame&) -> diag::Result<void> {
          AppendRecordCoverage(
              process, body.Body(), span,
              BuildMachineBool(unit, body.Body(), false));
          return {};
        });
    if (!missed_or) return std::unexpected(std::move(missed_or.error()));
    return mir::Stmt{
        .label = std::move(label),
        .data = mir::IfStmt{
            .condition = taken_id,
            .then_scope = block.child_scopes.Add(*std::move(covered_or)),
            .else_scope = block.child_scopes.Add(*std::move(missed_or))}};
  }

  AppendRecordCoverage(
      process, block, span, block.ReadLocal(succeeded, bool_type));
  auto pass_or = LowerArmIntoScope(process, frame, c.pass_stmt);
  if (!pass_or) return std::unexpected(std::move(pass_or.error()));
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::IfStmt{
          .condition = taken_id,
          .then_scope = block.child_scopes.Add(*std::move(pass_or)),
          .else_scope = std::nullopt}};
}

}  // namespace lyra::lowering::hir_to_mir
