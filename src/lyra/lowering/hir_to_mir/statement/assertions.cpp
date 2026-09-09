#include "lyra/lowering/hir_to_mir/statement/assertions.hpp"

#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/statement/blocks.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The noun a default failure report names. LRM 16.3 gives an assumption the
// same checking an assertion gets, so all the directive decides is which of
// the two the reader is told about.
auto FailureText(hir::AssertionDirective directive) -> std::string_view {
  switch (directive) {
    case hir::AssertionDirective::kAssert:
      return "assertion failed";
    case hir::AssertionDirective::kAssume:
      return "assumption failed";
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

// LRM 16.3: with no fail statement, what a false expression selects is the
// tool's own report, which the standard fixes at error severity. A pass
// statement does not take its place -- the pass statement is what a true
// expression selects -- so this arm is reached on the same terms either way.
// The report emit goes into `block`, which is a fresh scope for a simple
// assertion and the deferred closure body for a deferred one.
void AppendDefaultReport(
    ProcessLowerer& process, mir::Block& block,
    hir::AssertionDirective directive, diag::SourceSpan span) {
  const mir::CompilationUnit& unit = process.Owner().Unit();
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  const mir::ExprId diagnostic_id =
      block.exprs.Add(BuildDiagnosticCallExpr(unit, runtime_id));
  const mir::ExprId origin_id = BuildStringValueExpr(
      unit, block,
      FormatRuntimeOriginString(span, process.Owner().SourceManager()));
  const mir::ExprId text_id =
      BuildStringValueExpr(unit, block, std::string(FailureText(directive)));
  const mir::ExprId emit_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kEmitError,
                          .receiver = diagnostic_id},
                  .arguments = {origin_id, text_id}},
          .type = unit.builtins.void_type});
  block.AppendStmt(mir::ExprStmt{.expr = emit_id});
}

auto BuildDefaultReportScope(
    ProcessLowerer& process, hir::AssertionDirective directive,
    diag::SourceSpan span) -> mir::Block {
  mir::Block report;
  AppendDefaultReport(process, report, directive, span);
  return report;
}

// Whether an action arm carries a statement to run. An omitted arm is absent,
// and slang gives an omitted deferred-assertion arm an empty statement rather
// than none, so both stand for "no action here" (LRM 16.3, 16.4).
auto HasRealArm(const hir::ProceduralBody& body, std::optional<hir::StmtId> arm)
    -> bool {
  if (!arm.has_value()) return false;
  return !std::holds_alternative<hir::EmptyStmt>(body.stmts.Get(*arm).data);
}

// The runtime commit a deferred assertion's report rides: an observed (`#0`)
// one matures in Observed and runs in Reactive, a final one in Postponed (LRM
// 16.4). The report is a receiver-less void closure the region invokes, so the
// call carries only the runtime handle and that closure.
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

// LRM 16.4: a failed deferred assert / assume with no fail statement submits
// its default report to the process's deferred report queue, to mature and run
// a region later. The report emit is frozen into a closure the region invokes;
// the submit itself is what the false branch of the assertion runs inline.
auto BuildDeferredReportSubmit(
    ProcessLowerer& process, WalkFrame frame, hir::AssertionTiming timing,
    hir::AssertionDirective directive, diag::SourceSpan span) -> mir::Block {
  mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block scope;
  const WalkFrame scope_frame = frame.WithBlock(&scope);

  ClosureBuilder closure(unit, scope_frame);
  AppendDefaultReport(process, closure.Body(), directive, span);
  const mir::ExprId closure_id = scope.exprs.Add(closure.BuildVoid());

  const mir::ExprId runtime_id =
      scope.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  const mir::ExprId submit_id = scope.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = DeferredSubmitBuiltin(timing),
                          .receiver = runtime_id},
                  .arguments = {closure_id}},
          .type = unit.builtins.void_type});
  scope.AppendStmt(mir::ExprStmt{.expr = submit_id});
  return scope;
}

}  // namespace

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

  // LRM 16.4: a deferred assertion holds its action for a later region. The
  // action block is not yet carried; the default report is, so a deferred
  // assertion with no action submits its report and one with an action is
  // refused rather than run inline.
  if (a.timing != hir::AssertionTiming::kSimple) {
    if (HasRealArm(process.HirBody(), a.pass_stmt) ||
        HasRealArm(process.HirBody(), a.fail_stmt)) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedStatementForm,
          "a deferred immediate assertion with an action block is not yet "
          "supported, though its default report is; pass --assertions skip to "
          "elide it");
    }
    // The expression is evaluated inline; only a false one submits the report,
    // so the pass arm runs nothing and the fail arm submits (LRM 16.4).
    mir::Block report_scope =
        BuildDeferredReportSubmit(process, frame, a.timing, a.directive, span);
    return mir::Stmt{
        .label = std::move(label),
        .data = mir::IfStmt{
            .condition = ReduceToCondition(unit, block, condition),
            .then_scope = block.child_scopes.Add(mir::Block{}),
            .else_scope = block.child_scopes.Add(std::move(report_scope))}};
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
  const mir::LocalId succeeded = frame.bindings->DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_cover_succeeded", .type = bool_type});
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = succeeded,
          .init = ReduceToCondition(unit, block, condition)});

  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
  const mir::ExprId site_id = BuildStringValueExpr(
      unit, block,
      FormatRuntimeOriginString(span, process.Owner().SourceManager()));
  const mir::ExprId outcome_id =
      block.exprs.Add(mir::MakeLocalRefExpr(succeeded, bool_type));
  const mir::ExprId record_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kRecordCoverage,
                          .receiver = runtime_id},
                  .arguments = {site_id, outcome_id}},
          .type = unit.builtins.void_type});
  block.AppendStmt(mir::ExprStmt{.expr = record_id});

  auto pass_or = LowerArmIntoScope(process, frame, c.pass_stmt);
  if (!pass_or) return std::unexpected(std::move(pass_or.error()));

  // A condition consumes a machine boolean, and the outcome already is one.
  const mir::ExprId taken_id =
      block.exprs.Add(mir::MakeLocalRefExpr(succeeded, bool_type));
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::IfStmt{
          .condition = taken_id,
          .then_scope = block.child_scopes.Add(*std::move(pass_or)),
          .else_scope = std::nullopt}};
}

}  // namespace lyra::lowering::hir_to_mir
