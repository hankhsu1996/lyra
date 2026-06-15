#include "lyra/lowering/ast_to_hir/statement/blocks.hpp"

#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/kind.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// LRM 9.3.2 parallel block. Each parallel statement becomes one branch. FJ1
// covers branches that touch only module-scope state; forms that need
// procedural state or a hierarchy name, and forks inside a function, are
// rejected here rather than miscompiled.
auto LowerForkStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::BlockStatement& block, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  // A function body is not a coroutine and cannot suspend, so it cannot spawn a
  // branch (LRM 13.4). A task body is a coroutine and is supported.
  if (proc.ContainingSymbol().kind == slang::ast::SymbolKind::Subroutine &&
      proc.ContainingSymbol()
              .as<slang::ast::SubroutineSymbol>()
              .subroutineKind == slang::ast::SubroutineKind::Function) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedForkJoinForm,
        "a fork-join block inside a function is not yet supported",
        diag::UnsupportedCategory::kFeature);
  }
  if (block.blockSymbol != nullptr && !block.blockSymbol->name.empty()) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedForkJoinForm,
        "a named fork-join block is not yet supported",
        diag::UnsupportedCategory::kFeature);
  }

  hir::JoinMode mode = hir::JoinMode::kAll;
  switch (block.blockKind) {
    case slang::ast::StatementBlockKind::JoinAll:
      mode = hir::JoinMode::kAll;
      break;
    case slang::ast::StatementBlockKind::JoinAny:
      mode = hir::JoinMode::kAny;
      break;
    case slang::ast::StatementBlockKind::JoinNone:
      mode = hir::JoinMode::kNone;
      break;
    case slang::ast::StatementBlockKind::Sequential:
      throw InternalError("LowerForkStmt: called on a sequential block");
  }

  std::vector<const slang::ast::Statement*> body_stmts;
  if (block.body.kind == slang::ast::StatementKind::List) {
    const auto& list = block.body.as<slang::ast::StatementList>();
    body_stmts.assign(list.list.begin(), list.list.end());
  } else {
    body_stmts.push_back(&block.body);
  }

  // LRM 9.3.2: a fork's block_item_declarations are not parallel statements --
  // they are locals of the fork scope, initialized in the parent at block entry
  // before any branch spawns. The grammar places them before the statements, so
  // they form a prefix; each remaining statement is a branch. Locals lower in
  // the enclosing (parent) context; only the branches enter the fork-branch
  // scope.
  std::vector<hir::StmtId> locals;
  std::vector<const slang::ast::Statement*> branch_stmts;
  for (const auto* child : body_stmts) {
    if (child->kind == slang::ast::StatementKind::VariableDeclaration) {
      auto local_stmt = proc.LowerStmt(*child, frame);
      if (!local_stmt) {
        return std::unexpected(std::move(local_stmt.error()));
      }
      locals.push_back(
          frame.current_procedural_body->AddStmt(*std::move(local_stmt)));
    } else {
      branch_stmts.push_back(child);
    }
  }

  std::vector<hir::StmtId> branches;
  branches.reserve(branch_stmts.size());
  const WalkFrame branch_frame = frame.WithForkBranch();
  for (const auto* child : branch_stmts) {
    auto child_stmt = proc.LowerStmt(*child, branch_frame);
    if (!child_stmt) return std::unexpected(std::move(child_stmt.error()));
    branches.push_back(
        branch_frame.current_procedural_body->AddStmt(*std::move(child_stmt)));
  }

  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::ForkStmt{
              .mode = mode,
              .locals = std::move(locals),
              .branches = std::move(branches)},
      .span = span};
}

}  // namespace

auto LowerStatementListStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::StatementList& list, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  std::vector<hir::StmtId> kids;
  kids.reserve(list.list.size());
  for (const auto* child : list.list) {
    auto child_stmt = proc.LowerStmt(*child, frame);
    if (!child_stmt) return std::unexpected(std::move(child_stmt.error()));
    kids.push_back(
        frame.current_procedural_body->AddStmt(*std::move(child_stmt)));
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::BlockStmt{.statements = std::move(kids)},
      .span = span};
}

auto LowerBlockStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::BlockStatement& block, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  if (block.blockKind != slang::ast::StatementBlockKind::Sequential) {
    return LowerForkStmt(proc, frame, block, span);
  }
  std::vector<hir::StmtId> kids;
  if (block.body.kind == slang::ast::StatementKind::List) {
    const auto& list = block.body.as<slang::ast::StatementList>();
    kids.reserve(list.list.size());
    for (const auto* child : list.list) {
      auto child_stmt = proc.LowerStmt(*child, frame);
      if (!child_stmt) return std::unexpected(std::move(child_stmt.error()));
      kids.push_back(
          frame.current_procedural_body->AddStmt(*std::move(child_stmt)));
    }
  } else {
    auto child_stmt = proc.LowerStmt(block.body, frame);
    if (!child_stmt) return std::unexpected(std::move(child_stmt.error()));
    kids.push_back(
        frame.current_procedural_body->AddStmt(*std::move(child_stmt)));
  }
  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::BlockStmt{.statements = std::move(kids)},
      .span = span};
}

}  // namespace lyra::lowering::ast_to_hir
