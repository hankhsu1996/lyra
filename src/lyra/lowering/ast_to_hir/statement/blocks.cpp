#include "lyra/lowering/ast_to_hir/statement/blocks.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/structural_scope.hpp"

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
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedForkJoinForm,
        "a fork-join block inside a function is not yet supported");
  }
  if (block.blockSymbol != nullptr && !block.blockSymbol->name.empty()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedForkJoinForm,
        "a named fork-join block is not yet supported");
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
  // they form a prefix; each remaining statement is a branch. The fork
  // introduces its own lexical declaration scope owning those locals; locals
  // lower in the parent execution context but attach to the fork scope, and
  // only the branches enter the fork-branch execution scope.
  std::vector<hir::ProceduralVarId> fork_declarations;
  std::vector<hir::ProceduralScopeId> fork_children;
  const WalkFrame fork_scope_frame =
      frame.WithProceduralScopeAccumulators(&fork_declarations, &fork_children);

  std::vector<hir::StmtId> locals;
  std::vector<const slang::ast::Statement*> branch_stmts;
  for (const auto* child : body_stmts) {
    if (child->kind == slang::ast::StatementKind::VariableDeclaration) {
      auto local_stmt = proc.LowerStmt(*child, fork_scope_frame);
      if (!local_stmt) {
        return std::unexpected(std::move(local_stmt.error()));
      }
      locals.push_back(fork_scope_frame.current_procedural_body->stmts.Add(
          *std::move(local_stmt)));
    } else {
      branch_stmts.push_back(child);
    }
  }

  std::vector<hir::StmtId> branches;
  branches.reserve(branch_stmts.size());
  const WalkFrame branch_frame = fork_scope_frame.WithForkBranch();
  for (const auto* child : branch_stmts) {
    auto child_stmt = proc.LowerStmt(*child, branch_frame);
    if (!child_stmt) return std::unexpected(std::move(child_stmt.error()));
    branches.push_back(branch_frame.current_procedural_body->stmts.Add(
        *std::move(child_stmt)));
  }

  if (frame.current_structural_scope == nullptr) {
    throw InternalError(
        "LowerForkStmt: fork has no enclosing structural scope to register "
        "its lexical scope against");
  }
  const hir::ProceduralScopeId scope_id =
      frame.current_structural_scope->procedural_scopes.Add(
          hir::ProceduralScopeDecl{
              .kind = hir::ProceduralScopeKind::kForkJoin,
              .label = std::nullopt,
              .direct_declarations = std::move(fork_declarations),
              .direct_child_scopes = std::move(fork_children)});

  frame.current_scope_children->push_back(scope_id);

  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::ForkStmt{
              .mode = mode,
              .locals = std::move(locals),
              .branches = std::move(branches),
              .scope = scope_id},
      .span = span};
}

}  // namespace

auto LowerStatementListStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::StatementList& list, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  // A bare slang `StatementList` (multiple statements without a source-level
  // `begin ... end`) lowers to an unnamed begin/end scope -- semantically
  // equivalent for declaration ownership, with no SV identifier and no
  // hierarchical addressability.
  std::vector<hir::ProceduralVarId> nested_declarations;
  std::vector<hir::ProceduralScopeId> nested_children;
  const WalkFrame body_frame = frame.WithProceduralScopeAccumulators(
      &nested_declarations, &nested_children);

  std::vector<hir::StmtId> kids;
  kids.reserve(list.list.size());
  for (const auto* child : list.list) {
    auto child_stmt = proc.LowerStmt(*child, body_frame);
    if (!child_stmt) return std::unexpected(std::move(child_stmt.error()));
    kids.push_back(
        body_frame.current_procedural_body->stmts.Add(*std::move(child_stmt)));
  }

  if (frame.current_structural_scope == nullptr) {
    throw InternalError(
        "LowerStatementListStmt: statement list has no enclosing structural "
        "scope to register its lexical scope against");
  }
  const hir::ProceduralScopeId scope_id =
      frame.current_structural_scope->procedural_scopes.Add(
          hir::ProceduralScopeDecl{
              .kind = hir::ProceduralScopeKind::kBeginEndBlock,
              .label = std::nullopt,
              .direct_declarations = std::move(nested_declarations),
              .direct_child_scopes = std::move(nested_children)});

  frame.current_scope_children->push_back(scope_id);

  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::BlockStmt{.statements = std::move(kids), .scope = scope_id},
      .span = span};
}

auto LowerBlockStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::BlockStatement& block, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  if (block.blockKind != slang::ast::StatementBlockKind::Sequential) {
    return LowerForkStmt(proc, frame, block, span);
  }
  // Every `begin ... end` introduces a lexical declaration scope (LRM
  // 9.3.4); the label (LRM 9.3.5) is optional. The scope's own declarations
  // and nested child scopes are collected here through a fresh accumulator
  // pair, then sealed into a `ProceduralScopeDecl` and appended to the
  // enclosing structural scope's arena when the body walk returns -- the
  // arena is append-only, so the scope must be assembled before its first
  // `Add`. A named scope is also registered as a runtime-addressable
  // owned child so a reference whose leading component slang resolves to
  // this block's symbol can route through the same head-binding machinery
  // that instance and generate heads use.
  std::optional<std::string> label;
  if (block.blockSymbol != nullptr && !block.blockSymbol->name.empty()) {
    label = std::string{block.blockSymbol->name};
  }

  std::vector<hir::ProceduralVarId> nested_declarations;
  std::vector<hir::ProceduralScopeId> nested_children;
  const WalkFrame body_frame = frame.WithProceduralScopeAccumulators(
      &nested_declarations, &nested_children);

  std::vector<hir::StmtId> kids;
  if (block.body.kind == slang::ast::StatementKind::List) {
    const auto& list = block.body.as<slang::ast::StatementList>();
    kids.reserve(list.list.size());
    for (const auto* child : list.list) {
      auto child_stmt = proc.LowerStmt(*child, body_frame);
      if (!child_stmt) return std::unexpected(std::move(child_stmt.error()));
      kids.push_back(body_frame.current_procedural_body->stmts.Add(
          *std::move(child_stmt)));
    }
  } else {
    auto child_stmt = proc.LowerStmt(block.body, body_frame);
    if (!child_stmt) return std::unexpected(std::move(child_stmt.error()));
    kids.push_back(
        body_frame.current_procedural_body->stmts.Add(*std::move(child_stmt)));
  }

  if (frame.current_structural_scope == nullptr) {
    throw InternalError(
        "LowerBlockStmt: begin/end body has no enclosing structural scope "
        "to register its lexical scope against");
  }
  const hir::ProceduralScopeId scope_id =
      frame.current_structural_scope->procedural_scopes.Add(
          hir::ProceduralScopeDecl{
              .kind = hir::ProceduralScopeKind::kBeginEndBlock,
              .label = label,
              .direct_declarations = std::move(nested_declarations),
              .direct_child_scopes = std::move(nested_children)});

  frame.current_scope_children->push_back(scope_id);

  if (label.has_value() && block.blockSymbol != nullptr) {
    proc.Module().MapOwnedChildBinding(
        *block.blockSymbol, frame.Current(),
        hir::DownwardHead{.child = scope_id});
  }

  return hir::Stmt{
      .label = std::nullopt,
      .data = hir::BlockStmt{.statements = std::move(kids), .scope = scope_id},
      .span = span};
}

}  // namespace lyra::lowering::ast_to_hir
