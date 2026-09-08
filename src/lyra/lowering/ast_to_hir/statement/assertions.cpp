#include "lyra/lowering/ast_to_hir/statement/assertions.hpp"

#include <expected>
#include <optional>
#include <utility>

#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Statement.h>
#include <slang/ast/statements/MiscStatements.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// One arm of an action block (LRM 16.3). An absent arm is what the source
// wrote, not a failure to lower one.
auto LowerActionArm(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::Statement* action)
    -> diag::Result<std::optional<hir::Stmt>> {
  if (action == nullptr) {
    return std::optional<hir::Stmt>{};
  }
  auto stmt_or = proc.LowerStmt(*action, frame);
  if (!stmt_or) return std::unexpected(std::move(stmt_or.error()));
  return std::optional<hir::Stmt>(*std::move(stmt_or));
}

auto AddActionArm(hir::ProceduralBody& body, std::optional<hir::Stmt> arm)
    -> std::optional<hir::StmtId> {
  if (!arm.has_value()) {
    return std::nullopt;
  }
  return body.stmts.Add(*std::move(arm));
}

auto AssertStmtOf(
    hir::AssertionDirective directive, hir::AssertionTiming timing,
    hir::ExprId condition, std::optional<hir::StmtId> pass_stmt,
    std::optional<hir::StmtId> fail_stmt, diag::SourceSpan span) -> hir::Stmt {
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::AssertStmt{
              .directive = directive,
              .timing = timing,
              .condition = condition,
              .pass_stmt = pass_stmt,
              .fail_stmt = fail_stmt},
      .span = span};
}

// LRM 16.4: `#0` defers the action to the Observed region and matures it in
// Reactive, `final` defers it to Postponed; an assertion with no qualifier runs
// its action inline.
auto TimingOf(const slang::ast::ImmediateAssertionStatement& as)
    -> hir::AssertionTiming {
  if (!as.isDeferred) return hir::AssertionTiming::kSimple;
  return as.isFinal ? hir::AssertionTiming::kFinal
                    : hir::AssertionTiming::kObserved;
}

}  // namespace

auto LowerImmediateAssertionStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ImmediateAssertionStatement& as, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  const hir::AssertionTiming timing = TimingOf(as);

  auto cond_or = proc.LowerExpr(as.cond, frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId condition = frame.Exprs().Add(*std::move(cond_or));

  auto pass_or = LowerActionArm(proc, frame, as.ifTrue);
  if (!pass_or) return std::unexpected(std::move(pass_or.error()));

  auto fail_or = LowerActionArm(proc, frame, as.ifFalse);
  if (!fail_or) return std::unexpected(std::move(fail_or.error()));

  hir::ProceduralBody& body = *frame.current_procedural_body;
  const std::optional<hir::StmtId> pass_stmt =
      AddActionArm(body, *std::move(pass_or));
  const std::optional<hir::StmtId> fail_stmt =
      AddActionArm(body, *std::move(fail_or));

  switch (as.assertionKind) {
    case slang::ast::AssertionKind::Assert:
      return AssertStmtOf(
          hir::AssertionDirective::kAssert, timing, condition, pass_stmt,
          fail_stmt, span);
    case slang::ast::AssertionKind::Assume:
      return AssertStmtOf(
          hir::AssertionDirective::kAssume, timing, condition, pass_stmt,
          fail_stmt, span);
    case slang::ast::AssertionKind::CoverProperty:
      if (timing != hir::AssertionTiming::kSimple) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedStatementForm,
            "a deferred cover statement holds its recorded hit back to a later "
            "region of the time step, which is not yet supported; pass "
            "--assertions skip to elide it");
      }
      return hir::Stmt{
          .label = std::nullopt,
          .data =
              hir::CoverStmt{.condition = condition, .pass_stmt = pass_stmt},
          .span = span};
    case slang::ast::AssertionKind::CoverSequence:
    case slang::ast::AssertionKind::Restrict:
    case slang::ast::AssertionKind::Expect:
      break;
  }
  throw InternalError(
      "LowerImmediateAssertionStmt: an immediate assertion statement carried "
      "a directive that has no immediate form (LRM 16.2)");
}

}  // namespace lyra::lowering::ast_to_hir
