#include "lyra/lowering/ast_to_hir/statement/blocks.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/statements/MiscStatements.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/structural_scope.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The lexical declaration scope a block-like construct opens (LRM 9.3.4). A
// `begin` / `fork` that declares nothing and carries no name introduces no
// scope -- slang records none for it -- and its statements belong to the
// enclosing scope; so does anything lowered outside a structural scope, whose
// scope record would have no consumer. Whether a scope exists is settled once
// here, so a lowering opens one, lowers into it, and seals it the same way
// either way.
class ProceduralScope {
 public:
  ProceduralScope(
      const WalkFrame& enclosing,
      const slang::ast::StatementBlockSymbol* symbol)
      : symbol_(
            enclosing.current_structural_scope != nullptr ? symbol : nullptr),
        enclosing_(enclosing),
        frame_(
            symbol_ != nullptr ? enclosing.WithProceduralScopeAccumulators(
                                     &declarations_, &children_)
                               : enclosing) {
  }

  ProceduralScope(const ProceduralScope&) = delete;
  auto operator=(const ProceduralScope&) -> ProceduralScope& = delete;
  ProceduralScope(ProceduralScope&&) = delete;
  auto operator=(ProceduralScope&&) -> ProceduralScope& = delete;
  ~ProceduralScope() = default;

  // The frame the construct's statements lower under.
  [[nodiscard]] auto Frame() const -> const WalkFrame& {
    return frame_;
  }

  // Fills in the identity the declaration pass minted and links the scope under
  // the enclosing one, yielding the scope the construct names, or nullopt when
  // it opened none.
  auto Seal(ProcessLowerer& proc, std::optional<std::string> label)
      -> std::optional<hir::ProceduralScopeId> {
    if (symbol_ == nullptr) {
      return std::nullopt;
    }
    auto& scopes = enclosing_.current_structural_scope->procedural_scopes;
    const hir::ProceduralScopeId id =
        proc.Owner().LookupProceduralScope(*symbol_);
    scopes.Define(
        id, hir::ProceduralScopeDecl{
                .label = std::move(label),
                .direct_declarations = std::move(declarations_),
                .direct_child_scopes = std::move(children_)});
    enclosing_.current_scope_children->push_back(id);
    return id;
  }

 private:
  const slang::ast::StatementBlockSymbol* symbol_;
  std::vector<hir::ProceduralVarId> declarations_;
  std::vector<hir::ProceduralScopeId> children_;
  WalkFrame enclosing_;
  WalkFrame frame_;
};

// LRM 9.3.2 parallel block. Each parallel statement becomes one branch. A
// function body cannot suspend, so the frontend rejects `join` / `join_any`
// there; `join_none` spawns without awaiting and needs no coroutine host.
auto LowerForkStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::BlockStatement& block, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  // A fork block may carry a name (LRM 9.3.4) or a statement label (LRM 9.3.5);
  // both name the fork's lexical scope.
  std::optional<std::string> label;
  if (block.blockSymbol != nullptr && !block.blockSymbol->name.empty()) {
    label = std::string{block.blockSymbol->name};
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
  // the parent execution context but attach to the fork scope, and only the
  // branches enter the fork-branch execution scope.
  ProceduralScope scope(frame, block.blockSymbol);
  const WalkFrame& fork_scope_frame = scope.Frame();

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

  scope.Seal(proc, std::move(label));

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
  // A bare slang `StatementList` (multiple statements without a source-level
  // `begin ... end`) introduces no declaration scope: whatever it declares
  // belongs to the construct that encloses it, which is where slang places it
  // too. It groups statements and nothing else.
  std::vector<hir::StmtId> kids;
  kids.reserve(list.list.size());
  for (const auto* child : list.list) {
    auto child_stmt = proc.LowerStmt(*child, frame);
    if (!child_stmt) return std::unexpected(std::move(child_stmt.error()));
    kids.push_back(
        frame.current_procedural_body->stmts.Add(*std::move(child_stmt)));
  }

  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::BlockStmt{.statements = std::move(kids), .scope = std::nullopt},
      .span = span};
}

auto LowerBlockStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::BlockStatement& block, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  if (block.blockKind != slang::ast::StatementBlockKind::Sequential) {
    return LowerForkStmt(proc, frame, block, span);
  }
  // A `begin ... end` that declares something or carries a label (LRM 9.3.4 /
  // 9.3.5) introduces a lexical declaration scope; one that does neither is
  // transparent.
  std::optional<std::string> label;
  if (block.blockSymbol != nullptr && !block.blockSymbol->name.empty()) {
    label = std::string{block.blockSymbol->name};
  }

  ProceduralScope scope(frame, block.blockSymbol);
  const WalkFrame& body_frame = scope.Frame();

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

  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::BlockStmt{
              .statements = std::move(kids),
              .scope = scope.Seal(proc, std::move(label))},
      .span = span};
}

}  // namespace lyra::lowering::ast_to_hir
