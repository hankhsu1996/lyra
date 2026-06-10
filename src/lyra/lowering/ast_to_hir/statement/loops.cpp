#include "lyra/lowering/ast_to_hir/statement/loops.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/Statement.h>
#include <slang/ast/statements/LoopStatements.h>

#include "lyra/lowering/ast_to_hir/statement/dispatch.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerForLoopStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ForLoopStatement& fs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  // slang elaborates `for (int i = 0; ...)` as an independent preceding
  // VariableDeclStatement plus a ForLoopStatement whose `initializers` is
  // empty and `loopVars` only points at the already-declared symbol. The
  // preceding VarDeclStatement carries the initializer, so loopVars is
  // informational only and is ignored here.
  std::vector<hir::ForInit> hir_init;
  hir_init.reserve(fs.initializers.size());
  for (const auto* init_expr : fs.initializers) {
    auto init_or = proc.LowerExpr(*init_expr, frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    hir_init.emplace_back(
        hir::ForInitExpr{.expr = proc.AddExpr(*std::move(init_or))});
  }
  std::optional<hir::ExprId> cond_id;
  if (fs.stopExpr != nullptr) {
    auto cond_or = proc.LowerExpr(*fs.stopExpr, frame);
    if (!cond_or) return std::unexpected(std::move(cond_or.error()));
    cond_id = proc.AddExpr(*std::move(cond_or));
  }
  std::vector<hir::ExprId> step_ids;
  step_ids.reserve(fs.steps.size());
  for (const auto* step_expr : fs.steps) {
    auto step_or = proc.LowerExpr(*step_expr, frame);
    if (!step_or) return std::unexpected(std::move(step_or.error()));
    step_ids.push_back(proc.AddExpr(*std::move(step_or)));
  }
  auto body_stmt = LowerStatement(proc, frame, fs.body);
  if (!body_stmt) return std::unexpected(std::move(body_stmt.error()));
  const hir::StmtId body_id = proc.AddStmt(*std::move(body_stmt));
  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::ForStmt{
              .init = std::move(hir_init),
              .condition = cond_id,
              .step = std::move(step_ids),
              .body = body_id},
      .span = span};
}

auto LowerWhileLoopStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::WhileLoopStatement& ws, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto cond_or = proc.LowerExpr(ws.cond, frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id = proc.AddExpr(*std::move(cond_or));
  auto body_or = LowerStatement(proc, frame, ws.body);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  const hir::StmtId body_id = proc.AddStmt(*std::move(body_or));
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::WhileStmt{.condition = cond_id, .body = body_id},
      .span = span};
}

auto LowerRepeatLoopStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::RepeatLoopStatement& rs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto count_or = proc.LowerExpr(rs.count, frame);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const hir::ExprId count_id = proc.AddExpr(*std::move(count_or));
  auto body_or = LowerStatement(proc, frame, rs.body);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  const hir::StmtId body_id = proc.AddStmt(*std::move(body_or));
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::RepeatStmt{.count = count_id, .body = body_id},
      .span = span};
}

auto LowerDoWhileLoopStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::DoWhileLoopStatement& ds, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto body_or = LowerStatement(proc, frame, ds.body);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  const hir::StmtId body_id = proc.AddStmt(*std::move(body_or));
  auto cond_or = proc.LowerExpr(ds.cond, frame);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const hir::ExprId cond_id = proc.AddExpr(*std::move(cond_or));
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::DoWhileStmt{.condition = cond_id, .body = body_id},
      .span = span};
}

auto LowerForeverLoopStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::ForeverLoopStatement& fs, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  auto body_or = LowerStatement(proc, frame, fs.body);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  const hir::StmtId body_id = proc.AddStmt(*std::move(body_or));
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::ForeverStmt{.body = body_id},
      .span = span};
}

}  // namespace lyra::lowering::ast_to_hir
