#include <expected>
#include <optional>
#include <utility>

#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/statements/ConditionalStatements.h>
#include <slang/ast/statements/LoopStatements.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"
#include "lyra/lowering/ast_to_hir/module_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/statement/blocks.hpp"
#include "lyra/lowering/ast_to_hir/statement/branches.hpp"
#include "lyra/lowering/ast_to_hir/statement/loops.hpp"
#include "lyra/lowering/ast_to_hir/statement/timing.hpp"

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
  const auto& mapper = proc.Module().SourceMapper();
  const auto& sym = vd.symbol;
  auto type_id_or =
      proc.Module().InternType(sym.getType(), mapper.PointSpanOf(sym.location));
  if (!type_id_or) return std::unexpected(std::move(type_id_or.error()));
  const auto local_id =
      proc.AddProceduralVar(*frame.current_procedural_body, sym, *type_id_or);
  std::optional<hir::ExprId> init_id;
  if (const auto* init_expr = sym.getInitializer()) {
    auto init_or = proc.LowerExpr(*init_expr, frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    init_id = frame.current_procedural_body->AddExpr(*std::move(init_or));
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::VarDeclStmt{.var = local_id, .init = init_id},
      .span = span};
}

auto LowerExpressionStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ExpressionStatement& es, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto expr = proc.LowerExpr(es.expr, frame);
  if (!expr) return std::unexpected(std::move(expr.error()));
  const hir::ExprId id =
      frame.current_procedural_body->AddExpr(*std::move(expr));
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
    value = frame.current_procedural_body->AddExpr(*std::move(expr_or));
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
  const auto& mapper = proc.Module().SourceMapper();
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

    case slang::ast::StatementKind::Conditional:
      return LowerConditionalStmt(
          proc, frame, stmt.as<slang::ast::ConditionalStatement>(), span);

    case slang::ast::StatementKind::Return:
      return LowerReturnStmt(
          proc, frame, stmt.as<slang::ast::ReturnStatement>(), span);

    default:
      return diag::Unsupported(
          span, diag::DiagCode::kUnsupportedStatementForm,
          "this statement form is not supported yet",
          diag::UnsupportedCategory::kFeature);
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
