#include "lyra/lowering/hir_to_mir/statement/assertions.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/diag_message.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lowering/hir_to_mir/assertion_control.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/support/assertion_control.hpp"
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
    ProcessLowerer& process, mir::Builder block,
    hir::AssertionDirective directive) -> mir::Block {
  mir::Block report;
  AppendDefaultReport(process, block.Into(report), directive);
  return report;
}

// The commit a deferred assertion's report rides: an observed (`#0`) one
// matures in Observed and runs in Reactive, a final one in Postponed (LRM
// 16.4). The report is the body its action runs as, kept by the assertion's
// own control, which a Kill withdraws it through (LRM 20.11), so the call
// carries only that coroutine.
auto DeferredSubmitBuiltin(hir::AssertionTiming timing) -> support::BuiltinFn {
  switch (timing) {
    case hir::AssertionTiming::kObserved:
      return support::BuiltinFn::kAssertionControlSubmitObserved;
    case hir::AssertionTiming::kFinal:
      return support::BuiltinFn::kAssertionControlSubmitFinal;
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
    hir::ControlledAssertionId control, Fill fill) -> diag::Result<mir::Block> {
  mir::Block scope;
  mir::Builder scope_builder(scope, frame.span);
  const WalkFrame scope_frame = frame.WithBlock(&scope);
  ClosureBuilder body(
      process.Owner().Unit(), scope_frame, support::ClosureKind::kFnOnce);
  if (auto filled = fill(body, scope_frame); !filled) {
    return std::unexpected(std::move(filled.error()));
  }
  const mir::ExprId body_id = scope_builder.Add(body.BuildCallOnce());
  const mir::ExprId control_place = process.ControlPlace(scope_frame, control);
  scope_builder.AppendStmt(
      mir::ExprStmt{
          .expr = scope_builder.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee =
                              mir::Direct{
                                  .target = DeferredSubmitBuiltin(timing),
                                  .receiver = control_place},
                          .arguments = {body_id}},
                  .type = process.Owner().Unit().builtins.void_type})});
  return scope;
}

// `then_block` as what runs where `allowed` lets the execution do `what`, and
// nothing otherwise, in a scope of its own (LRM 20.11).
auto GuardedBy(
    const mir::CompilationUnit& unit, mir::Builder outer, mir::LocalId allowed,
    std::uint64_t what, mir::Block then_block) -> mir::Block {
  mir::Block guarded;
  mir::Builder guarded_builder = outer.Into(guarded);
  const mir::ExprId condition = Allows(unit, guarded_builder, allowed, what);
  guarded_builder.AppendStmt(
      mir::IfStmt{
          .condition = condition,
          .then_scope = guarded_builder.AddScope(std::move(then_block)),
          .else_scope = std::nullopt});
  return guarded;
}

// A statement that checks only where its control lets it (LRM 20.11): what the
// execution may do is read as it begins, into `allowed`, and `checked` is the
// whole of the statement, run only where checking is on.
auto Checked(
    ProcessLowerer& process, const WalkFrame& frame,
    std::optional<std::string> label, mir::LocalId allowed, mir::Block checked)
    -> mir::Stmt {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Builder block = frame.Builder();
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::IfStmt{
          .condition = Allows(unit, block, allowed, support::kAssertionChecks),
          .then_scope = block.AddScope(std::move(checked)),
          .else_scope = std::nullopt}};
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
        "subroutine call, which the front end requires (LRM 16.4)");
  }
  std::vector<hir::ExprId> operands;
  for (const std::optional<hir::ExprId>& argument : call->arguments) {
    if (argument.has_value()) operands.push_back(*argument);
  }
  const ArgumentsEvaluatedEarlier arguments{
      .operands = operands, .statement = &statement};
  auto lowered = process.LowerStmt(
      call_stmt,
      body.Frame().WithRunsAsFunctionCall(false).WithArgumentsEvaluatedEarlier(
          &arguments));
  if (!lowered) return std::unexpected(std::move(lowered.error()));
  body.Body().AppendStmt(*std::move(lowered));
  return {};
}

// The submit of a deferred arm the source wrote a statement for.
auto BuildDeferredCallSubmit(
    ProcessLowerer& process, WalkFrame frame, hir::AssertionTiming timing,
    hir::ControlledAssertionId control, hir::StmtId arm)
    -> diag::Result<mir::Block> {
  return BuildDeferredSubmit(
      process, frame, timing, control,
      [&](ClosureBuilder& body, const WalkFrame& statement) {
        return LowerDeferredCallInto(process, body, statement, arm);
      });
}

// A false result of a deferred assert or assume runs the fail statement the
// source wrote, nothing where that statement is null, and the tool's own report
// where the source wrote no else clause (LRM 16.4.1).
auto BuildDeferredFailArm(
    ProcessLowerer& process, WalkFrame frame, const hir::AssertStmt& a)
    -> diag::Result<mir::Block> {
  if (!a.fail_stmt.has_value()) {
    return BuildDeferredSubmit(
        process, frame, a.timing, a.control,
        [&](ClosureBuilder& body, const WalkFrame&) -> diag::Result<void> {
          AppendDefaultReport(process, body.Body(), a.directive);
          return {};
        });
  }
  if (!HasRealArm(process.HirBody(), a.fail_stmt)) {
    return mir::Block{};
  }
  return BuildDeferredCallSubmit(
      process, frame, a.timing, a.control, *a.fail_stmt);
}

// LRM 16.3: one evaluation of an immediate cover, and whether it succeeded,
// recorded against the statement's own place, which is what separates one
// coverage goal from another.
void AppendRecordCoverage(
    ProcessLowerer& process, mir::Builder block, mir::ExprId outcome) {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  const mir::ExprId runtime_id =
      block.Add(BuildCurrentRuntimeCallExpr(process.Owner().Unit()));
  const mir::ExprId site_id =
      process.Owner().BuildSourceLocationAt(block, block.Span());
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.Add(
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
    ProcessLowerer& process, mir::Builder block,
    hir::AssertionDirective directive) {
  AppendToolReportStmt(
      process.Owner().Unit(), block, support::BuiltinFn::kEmitError,
      FailureText(directive));
}

auto HasRealArm(const hir::ProceduralBody& body, std::optional<hir::StmtId> arm)
    -> bool {
  if (!arm.has_value()) return false;
  return !std::holds_alternative<hir::EmptyStmt>(body.stmts.Get(*arm).data);
}

auto LowerAssertStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::AssertStmt& a) -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  const mir::LocalId allowed = ReadAllowed(process, frame, a.control);
  mir::Block checked;
  const WalkFrame checked_frame = frame.WithBlock(&checked);
  mir::Builder block = checked_frame.Builder();

  auto cond_or = process.LowerExpr(
      process.HirBody().exprs.Get(a.condition), checked_frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId condition = block.Add(*std::move(cond_or));

  diag::Result<mir::Block> pass_or = mir::Block{};
  diag::Result<mir::Block> fail_or = mir::Block{};
  // LRM 16.4: a deferred assertion evaluates its expression inline and holds
  // the action the outcome selects for a later region. A true result with no
  // pass statement runs nothing.
  if (a.timing != hir::AssertionTiming::kSimple) {
    if (HasRealArm(process.HirBody(), a.pass_stmt)) {
      pass_or = BuildDeferredCallSubmit(
          process, checked_frame, a.timing, a.control, *a.pass_stmt);
      if (!pass_or) return std::unexpected(std::move(pass_or.error()));
    }
    fail_or = BuildDeferredFailArm(process, checked_frame, a);
  } else {
    pass_or = LowerArmIntoScope(process, checked_frame, a.pass_stmt);
    if (!pass_or) return std::unexpected(std::move(pass_or.error()));
    if (a.fail_stmt.has_value()) {
      fail_or = LowerStmtIntoChildScope(process, checked_frame, *a.fail_stmt);
    } else {
      fail_or = BuildDefaultReportScope(process, block, a.directive);
    }
  }
  if (!fail_or) return std::unexpected(std::move(fail_or.error()));

  // An immediate assertion has no vacuous success, so what its pass statement
  // asks of the control is that a nonvacuous pass runs (LRM 20.11); its fail
  // statement, the tool's own report included, is what failing runs.
  block.AppendStmt(
      mir::IfStmt{
          .condition = ReduceToCondition(unit, block, condition),
          .then_scope = block.AddScope(GuardedBy(
              unit, block, allowed, support::kAssertionPassesNonvacuously,
              *std::move(pass_or))),
          .else_scope = block.AddScope(GuardedBy(
              unit, block, allowed, support::kAssertionFails,
              *std::move(fail_or)))});
  return Checked(process, frame, std::move(label), allowed, std::move(checked));
}

auto LowerCoverStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CoverStmt& c) -> diag::Result<mir::Stmt> {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  const mir::TypeId bool_type = unit.builtins.machine_bool;
  const mir::LocalId allowed = ReadAllowed(process, frame, c.control);
  mir::Block checked;
  const WalkFrame checked_frame = frame.WithBlock(&checked);
  mir::Builder block = checked_frame.Builder();

  auto cond_or = process.LowerExpr(
      process.HirBody().exprs.Get(c.condition), checked_frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId condition = block.Add(*std::move(cond_or));

  // LRM 16.3 evaluates the expression once, and both the coverage result and
  // the statement the goal carries read that one outcome.
  const mir::LocalId succeeded =
      checked_frame.bindings->DeclareAnonymous(bool_type);
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = succeeded,
          .init = ReduceToCondition(unit, block, condition)});
  // A condition consumes a machine boolean, and the outcome already is one.
  const mir::ExprId taken_id = block.ReadLocal(succeeded, bool_type);

  // LRM 16.4: a deferred cover holds back what it records together with its
  // statement, so a flush that withdraws the one withdraws the other and a
  // value met only in passing is not credited. What it records is the
  // coverage whether or not its control lets the statement run (LRM 20.11).
  if (c.timing != hir::AssertionTiming::kSimple) {
    const auto covered = [&](bool runs_statement) {
      return BuildDeferredSubmit(
          process, checked_frame, c.timing, c.control,
          [&, runs_statement](ClosureBuilder& body, const WalkFrame& statement)
              -> diag::Result<void> {
            AppendRecordCoverage(
                process, body.Body(),
                BuildMachineBool(unit, body.Body(), true));
            if (!runs_statement || !HasRealArm(process.HirBody(), c.pass_stmt))
              return {};
            return LowerDeferredCallInto(
                process, body, statement, *c.pass_stmt);
          });
    };
    auto with_statement = covered(true);
    if (!with_statement) {
      return std::unexpected(std::move(with_statement.error()));
    }
    auto without_statement = covered(false);
    if (!without_statement) {
      return std::unexpected(std::move(without_statement.error()));
    }
    auto missed_or = BuildDeferredSubmit(
        process, checked_frame, c.timing, c.control,
        [&](ClosureBuilder& body, const WalkFrame&) -> diag::Result<void> {
          AppendRecordCoverage(
              process, body.Body(), BuildMachineBool(unit, body.Body(), false));
          return {};
        });
    if (!missed_or) return std::unexpected(std::move(missed_or.error()));
    mir::Block hit;
    mir::Builder hit_builder = block.Into(hit);
    hit_builder.AppendStmt(
        mir::IfStmt{
            .condition = Allows(
                unit, hit_builder, allowed,
                support::kAssertionPassesNonvacuously),
            .then_scope = hit_builder.AddScope(*std::move(with_statement)),
            .else_scope = hit_builder.AddScope(*std::move(without_statement))});
    block.AppendStmt(
        mir::IfStmt{
            .condition = taken_id,
            .then_scope = block.AddScope(std::move(hit)),
            .else_scope = block.AddScope(*std::move(missed_or))});
    return Checked(
        process, frame, std::move(label), allowed, std::move(checked));
  }

  AppendRecordCoverage(process, block, block.ReadLocal(succeeded, bool_type));
  auto pass_or = LowerArmIntoScope(process, checked_frame, c.pass_stmt);
  if (!pass_or) return std::unexpected(std::move(pass_or.error()));
  block.AppendStmt(
      mir::IfStmt{
          .condition = taken_id,
          .then_scope = block.AddScope(GuardedBy(
              unit, block, allowed, support::kAssertionPassesNonvacuously,
              *std::move(pass_or))),
          .else_scope = std::nullopt});
  return Checked(process, frame, std::move(label), allowed, std::move(checked));
}

}  // namespace lyra::lowering::hir_to_mir
