#include <expected>
#include <optional>
#include <utility>
#include <variant>

#include <slang/ast/Expression.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/SystemSubroutine.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/statements/ConditionalStatements.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/parsing/KnownSystemName.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/statement/blocks.hpp"
#include "lyra/lowering/ast_to_hir/statement/branches.hpp"
#include "lyra/lowering/ast_to_hir/statement/loops.hpp"
#include "lyra/lowering/ast_to_hir/statement/timing.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Trivial statement handlers kept inline here -- each is a short literal
// constructor, not large enough to warrant its own subsystem file.

auto LowerEmptyStmt(diag::SourceSpan span) -> diag::Result<hir::Stmt> {
  return hir::Stmt{
      .label = std::nullopt, .data = hir::EmptyStmt{}, .span = span};
}

auto LowerBreakStmt(const WalkFrame& frame, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  // A break whose innermost SystemVerilog loop is a `foreach` must leave every
  // nested dimension, so it carries that foreach's loop label; consuming the
  // label marks the outer loop a landing target. An ordinary innermost break
  // (no foreach label in scope) stays plain.
  std::optional<hir::LoopLabelId> target = frame.innermost_break_label;
  if (target.has_value() && frame.innermost_break_used != nullptr) {
    *frame.innermost_break_used = true;
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::BreakStmt{.target = target},
      .span = span};
}

auto LowerContinueStmt(diag::SourceSpan span) -> diag::Result<hir::Stmt> {
  return hir::Stmt{
      .label = std::nullopt, .data = hir::ContinueStmt{}, .span = span};
}

auto LowerVariableDeclStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::VariableDeclStatement& vd, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  const auto& mapper = proc.Owner().SourceMapper();
  const auto& sym = vd.symbol;
  auto type_id_or =
      proc.Owner().InternType(sym.getType(), mapper.PointSpanOf(sym.location));
  if (!type_id_or) return std::unexpected(std::move(type_id_or.error()));
  // The identity is minted before the initializer lowers, because the
  // initializer is bound in the scope the declaration has already entered
  // (`int x = x;` names this very declaration), so a reference inside it has to
  // resolve to this identity.
  auto& body = *frame.current_procedural_body;
  const auto local_id = proc.DeclareProceduralVar(frame, body, sym);
  std::optional<hir::ExprId> init_id;
  if (const auto* init_expr = sym.getInitializer()) {
    auto init_or = proc.LowerExpr(*init_expr, frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init_id = frame.Exprs().Add(*std::move(init_or));
  }
  proc.DefineProceduralVar(body, local_id, sym, *type_id_or, init_id);
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::VarDeclStmt{.var = local_id},
      .span = span};
}

// LRM 20.12 assertion control tasks. Each is a void system task, so a call is
// only ever a statement -- matching here covers every position the LRM gives
// them.
auto IsAssertionControlTask(const slang::ast::Expression& expr) -> bool {
  if (expr.kind != slang::ast::ExpressionKind::Call) {
    return false;
  }
  const auto* info = std::get_if<slang::ast::CallExpression::SystemCallInfo>(
      &expr.as<slang::ast::CallExpression>().subroutine);
  if (info == nullptr || info->subroutine == nullptr) {
    return false;
  }
  using slang::parsing::KnownSystemName;
  switch (info->subroutine->knownNameId) {
    case KnownSystemName::AssertControl:
    case KnownSystemName::AssertOn:
    case KnownSystemName::AssertOff:
    case KnownSystemName::AssertKill:
    case KnownSystemName::AssertPassOn:
    case KnownSystemName::AssertPassOff:
    case KnownSystemName::AssertFailOn:
    case KnownSystemName::AssertFailOff:
    case KnownSystemName::AssertNonvacuousOn:
    case KnownSystemName::AssertVacuousOff:
      return true;
    default:
      return false;
  }
}

auto LowerExpressionStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ExpressionStatement& es, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  // Expressions the enclosing lowering has already consumed as a higher-
  // level semantic fact have no additional body-level effect, so the
  // wrapping statement lowers to empty. Pointer identity keeps this check
  // free of any per-kind knowledge of the consumed construct.
  if (proc.ConsumedBodyExprs().contains(&es.expr)) return LowerEmptyStmt(span);
  // Everything an assertion control task turns on, off, or kills is an
  // assertion of the design, so once those are elided the call has nothing
  // left to act on: it is a no-op by construction, not an approximation of
  // one. Without the policy it is a rejected construct, not silently ignored.
  if (IsAssertionControlTask(es.expr)) {
    if (proc.Owner().DisableAssertions()) {
      return LowerEmptyStmt(span);
    }
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedStatementForm,
        "assertion control tasks are not supported; pass "
        "--disable-assertions to skip them");
  }
  auto expr = proc.LowerExpr(es.expr, frame);
  if (!expr) return std::unexpected(std::move(expr.error()));
  const hir::ExprId id = frame.Exprs().Add(*std::move(expr));
  return hir::Stmt{
      .label = std::nullopt, .data = hir::ExprStmt{.expr = id}, .span = span};
}

// LRM 13.4.1 `return [expr];`. A non-void function carries the returned
// expression; void functions and tasks use the bare form, leaving `value`
// absent.
auto LowerReturnStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ReturnStatement& rs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  std::optional<hir::ExprId> value;
  if (rs.expr != nullptr) {
    auto expr_or = proc.LowerExpr(*rs.expr, frame);
    if (!expr_or) return std::unexpected(std::move(expr_or.error()));
    value = frame.Exprs().Add(*std::move(expr_or));
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::ReturnStmt{.value = value},
      .span = span};
}

}  // namespace

auto LowerStatement(
    ProcessLowerer& proc, WalkFrame frame, const slang::ast::Statement& stmt)
    -> diag::Result<hir::Stmt> {
  const auto& mapper = proc.Owner().SourceMapper();
  const auto span = mapper.SpanOf(stmt.sourceRange);
  switch (stmt.kind) {
    case slang::ast::StatementKind::Empty:
      return LowerEmptyStmt(span);

    case slang::ast::StatementKind::EventTrigger:
      return LowerEventTriggerStmt(
          proc, frame, stmt.as<slang::ast::EventTriggerStatement>(), span);

    case slang::ast::StatementKind::Wait:
      return LowerWaitStmt(
          proc, frame, stmt.as<slang::ast::WaitStatement>(), span);

    case slang::ast::StatementKind::WaitFork:
      return hir::Stmt{
          .label = std::nullopt, .data = hir::WaitForkStmt{}, .span = span};

    case slang::ast::StatementKind::DisableFork:
      return hir::Stmt{
          .label = std::nullopt, .data = hir::DisableForkStmt{}, .span = span};

    case slang::ast::StatementKind::Disable: {
      const auto& dis = stmt.as<slang::ast::DisableStatement>();
      // A disable names a block or a task (LRM 9.6.2) and slang rejects every
      // other target, so what arrives here always denotes a procedural scope.
      if (dis.target.kind != slang::ast::ExpressionKind::ArbitrarySymbol) {
        throw InternalError(
            "LowerStatement: a disable target is not a symbol reference");
      }
      const slang::ast::Symbol& target =
          *dis.target.as<slang::ast::ArbitrarySymbolExpression>().symbol;
      // The target resolves to the identity its own structural scope minted.
      // A class method body is inside none, and a target in another instance
      // or generate scope belongs to a different one; reaching either is a
      // hierarchical reference this statement does not yet route.
      if (frame.current_structural_scope == nullptr ||
          proc.Owner().OwningScopeFrame(target) != frame.Current()) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedStatementForm,
            "disable of a block or task outside the enclosing structural scope "
            "is not yet supported");
      }
      return hir::Stmt{
          .label = std::nullopt,
          .data =
              hir::DisableStmt{
                  .target = proc.Owner().LookupProceduralScope(target)},
          .span = span};
    }

    case slang::ast::StatementKind::Timed:
      return LowerTimedStmt(
          proc, frame, stmt.as<slang::ast::TimedStatement>(), span);

    case slang::ast::StatementKind::List:
      return LowerStatementListStmt(
          proc, frame, stmt.as<slang::ast::StatementList>(), span);

    case slang::ast::StatementKind::Block:
      return LowerBlockStmt(
          proc, frame, stmt.as<slang::ast::BlockStatement>(), span);

    case slang::ast::StatementKind::VariableDeclaration:
      return LowerVariableDeclStmt(
          proc, frame, stmt.as<slang::ast::VariableDeclStatement>(), span);

    case slang::ast::StatementKind::ExpressionStatement:
      return LowerExpressionStmt(
          proc, frame, stmt.as<slang::ast::ExpressionStatement>(), span);

    case slang::ast::StatementKind::ForLoop:
      return LowerForLoopStmt(
          proc, frame, stmt.as<slang::ast::ForLoopStatement>(), span);

    case slang::ast::StatementKind::WhileLoop:
      return LowerWhileLoopStmt(
          proc, frame, stmt.as<slang::ast::WhileLoopStatement>(), span);

    case slang::ast::StatementKind::RepeatLoop:
      return LowerRepeatLoopStmt(
          proc, frame, stmt.as<slang::ast::RepeatLoopStatement>(), span);

    case slang::ast::StatementKind::DoWhileLoop:
      return LowerDoWhileLoopStmt(
          proc, frame, stmt.as<slang::ast::DoWhileLoopStatement>(), span);

    case slang::ast::StatementKind::ForeverLoop:
      return LowerForeverLoopStmt(
          proc, frame, stmt.as<slang::ast::ForeverLoopStatement>(), span);

    case slang::ast::StatementKind::ForeachLoop:
      return proc.LowerForeachStmt(
          stmt.as<slang::ast::ForeachLoopStatement>(), frame);

    case slang::ast::StatementKind::Break:
      return LowerBreakStmt(frame, span);

    case slang::ast::StatementKind::Continue:
      return LowerContinueStmt(span);

    case slang::ast::StatementKind::Case:
      return LowerCaseStmt(
          proc, frame, stmt.as<slang::ast::CaseStatement>(), span);

    case slang::ast::StatementKind::PatternCase:
      return LowerPatternCaseStmt(
          proc, frame, stmt.as<slang::ast::PatternCaseStatement>(), span);

    case slang::ast::StatementKind::Conditional:
      return LowerConditionalStmt(
          proc, frame, stmt.as<slang::ast::ConditionalStatement>(), span);

    case slang::ast::StatementKind::Return:
      return LowerReturnStmt(
          proc, frame, stmt.as<slang::ast::ReturnStatement>(), span);

    case slang::ast::StatementKind::ImmediateAssertion:
    case slang::ast::StatementKind::ConcurrentAssertion:
      // An assertion embedded in surrounding behavior contributes no statement
      // when disabled; the rest of the process runs. Without the policy it is a
      // rejected construct, not silently ignored.
      if (proc.Owner().DisableAssertions()) {
        return LowerEmptyStmt(span);
      }
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedStatementForm,
          "assertion statements are not supported; pass --disable-assertions "
          "to skip them");

    default:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedStatementForm,
          "this statement form is not supported yet");
  }
}

// Class method wrapper. The pass-class entry delegates to the free-function
// dispatcher above.
auto ProcessLowerer::LowerStmt(
    const slang::ast::Statement& stmt, WalkFrame frame)
    -> diag::Result<hir::Stmt> {
  return LowerStatement(*this, frame, stmt);
}

}  // namespace lyra::lowering::ast_to_hir
