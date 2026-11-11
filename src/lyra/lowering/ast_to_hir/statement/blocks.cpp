#include "lyra/lowering/ast_to_hir/statement/blocks.hpp"

#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/syntax/AllSyntax.h>
#include <slang/syntax/SyntaxKind.h>
#include <slang/syntax/SyntaxNode.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/lowering/ast_to_hir/name_in_a_path.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The identifier the source gave a `begin` / `fork`, absent when it gave none.
// A block slang recorded no symbol for and one whose symbol carries no name are
// the same case: LRM 9.3.5 makes the `block_identifier` what a name outside can
// reach, and neither has one. A statement label (LRM 9.3.5) arrives here too,
// since slang records it as the block symbol's name.
auto SourceBlockName(const slang::ast::StatementBlockSymbol* symbol)
    -> std::optional<std::string> {
  if (symbol == nullptr || symbol->name.empty()) {
    return std::nullopt;
  }
  return NameInAPath(*symbol);
}

// The lexical declaration scope a `begin` / `fork` opens (LRM 9.3.4 / 9.3.2).
// Every one of them opens exactly one: of the construct's kind where the front
// end made the block a scope, which it does where the source named it or it
// directly declares an item (LRM 9.3.4, 37.12 detail 1), and one no name can
// reach where it made none.
//
// Whether the source named it is asked once, and that one answer decides both
// halves: the name the scope carries, and where its identity comes from. A
// named block is one a peer can name before this walk reaches it, so the
// compilation unit's declaration pass has already minted its identity; an
// unnamed one can be named by nothing, so it mints its own here. Asking the two
// separately is what lets them drift into looking up an identity nobody
// declared.
class ProceduralScope {
 public:
  ProceduralScope(
      ProcessLowerer& proc, const WalkFrame& enclosing,
      const slang::ast::StatementBlockSymbol* symbol,
      hir::ProceduralScopeKind kind)
      : ProceduralScope(
            proc, enclosing, symbol,
            symbol == nullptr ? hir::ProceduralScopeKind::kImplicit : kind,
            SourceBlockName(symbol)) {
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

  // Fills the finished contents into the scope's identity and hangs it under
  // the enclosing one, yielding the scope the construct opened.
  auto Seal() -> hir::ProceduralScopeId {
    return enclosing_.SealScope(std::move(open_));
  }

 private:
  ProceduralScope(
      ProcessLowerer& proc, const WalkFrame& enclosing,
      const slang::ast::StatementBlockSymbol* symbol,
      hir::ProceduralScopeKind kind, std::optional<std::string> source_name)
      : enclosing_(enclosing),
        open_(
            source_name.has_value()
                ? proc.Owner().LookupProceduralScope(*symbol)
                : enclosing.ProceduralScopes().Declare(),
            kind, std::move(source_name)),
        frame_(enclosing.WithOpenScope(&open_)) {
    // A block the front end made a scope of that the source gave no name
    // mints its identity here, and records it by the block, so what a value
    // dump records of the block lies within it.
    if (symbol != nullptr && !open_.source_name.has_value()) {
      proc.Owner().DeclareProceduralScope(
          *symbol, enclosing.ProceduralScopeOwner(), open_.id);
    }
  }

  WalkFrame enclosing_;
  OpenProceduralScope open_;
  WalkFrame frame_;
};

// What opened a sequential block: a `for` loop declaring its own variables, a
// `foreach` loop, which LRM 12.7.1 and 12.7.3 make an implicit block around the
// loop, or a `begin ... end` the source wrote or a statement label opened (LRM
// 9.3.5).
auto SequentialKindOf(const slang::ast::BlockStatement& block)
    -> hir::ProceduralScopeKind {
  const slang::syntax::SyntaxNode* syntax =
      block.blockSymbol != nullptr ? block.blockSymbol->getSyntax() : nullptr;
  if (syntax == nullptr) return hir::ProceduralScopeKind::kBegin;
  // A label on a `for` declaring no variable opens a block of its own around
  // the loop (LRM 9.3.5), whose syntax the front end still gives as the
  // loop's, so the two are told apart as the front end tells them: by whether
  // the loop's first initializer declares a variable.
  if (syntax->kind == slang::syntax::SyntaxKind::ForLoopStatement) {
    const auto& loop = syntax->as<slang::syntax::ForLoopStatementSyntax>();
    if (!loop.initializers.empty() &&
        loop.initializers[0]->kind ==
            slang::syntax::SyntaxKind::ForVariableDeclaration) {
      return hir::ProceduralScopeKind::kFor;
    }
  }
  if (syntax->kind == slang::syntax::SyntaxKind::ForeachLoopStatement) {
    return hir::ProceduralScopeKind::kForeach;
  }
  return hir::ProceduralScopeKind::kBegin;
}

// The join keyword a fork's block kind is (LRM 9.3.2 Table 9-1).
auto JoinModeOf(slang::ast::StatementBlockKind kind) -> hir::JoinMode {
  switch (kind) {
    case slang::ast::StatementBlockKind::JoinAll:
      return hir::JoinMode::kAll;
    case slang::ast::StatementBlockKind::JoinAny:
      return hir::JoinMode::kAny;
    case slang::ast::StatementBlockKind::JoinNone:
      return hir::JoinMode::kNone;
    case slang::ast::StatementBlockKind::Sequential:
      break;
  }
  throw InternalError("JoinModeOf: a sequential block is no fork");
}

// The kind of scope a fork joining as `mode` opens.
auto ForkScopeKind(hir::JoinMode mode) -> hir::ProceduralScopeKind {
  switch (mode) {
    case hir::JoinMode::kAll:
      return hir::ProceduralScopeKind::kForkJoin;
    case hir::JoinMode::kAny:
      return hir::ProceduralScopeKind::kForkJoinAny;
    case hir::JoinMode::kNone:
      return hir::ProceduralScopeKind::kForkJoinNone;
  }
  throw InternalError("ForkScopeKind: unknown join mode");
}

// LRM 9.3.2 parallel block. Each parallel statement becomes one branch. A
// function body cannot suspend, so the frontend rejects `join` / `join_any`
// there; `join_none` spawns without awaiting and needs no coroutine host.
auto LowerForkStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::BlockStatement& block, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  const hir::JoinMode mode = JoinModeOf(block.blockKind);
  const hir::ProceduralScopeKind kind = ForkScopeKind(mode);

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
  // they form a prefix; each remaining statement is a branch. The fork's own
  // lexical declaration scope owns those locals; they lower in the parent
  // execution context but attach to the fork scope, and only the branches enter
  // the fork-branch execution scope.
  ProceduralScope scope(proc, frame, block.blockSymbol, kind);
  const WalkFrame& fork_scope_frame = scope.Frame();
  if (block.blockSymbol != nullptr) {
    auto constants =
        proc.DeclarePerObjectConstants(*block.blockSymbol, fork_scope_frame);
    if (!constants) return std::unexpected(std::move(constants.error()));
  }

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

  return hir::Stmt{
      .label = std::nullopt,
      .data =
          hir::ForkStmt{
              .mode = mode,
              .locals = std::move(locals),
              .branches = std::move(branches),
              .scope = scope.Seal()},
      .span = span};
}

}  // namespace

auto LowerStatementListStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::StatementList& list, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  // Statements the source wrote without a `begin ... end` around them, which
  // LRM 13.4 gives the same sequential meaning as a begin-end group. The
  // sequence is a statement and not a declaration scope: what a task or an
  // `initial` writes at its own level belongs to that construct, whose scope
  // the walk is already in, and LRM 6.21 makes which scope a declaration sits
  // in the difference between a name outside reaching it and not.
  const hir::ProceduralScopeId scope = frame.SealScope(
      OpenProceduralScope{
          frame.ProceduralScopes().Declare(),
          hir::ProceduralScopeKind::kImplicit, std::nullopt});

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
      .data = hir::BlockStmt{.statements = std::move(kids), .scope = scope},
      .span = span};
}

auto LowerBlockStmt(
    ProcessLowerer& proc, WalkFrame frame,
    const slang::ast::BlockStatement& block, diag::SourceSpan span)
    -> diag::Result<hir::Stmt> {
  if (block.blockKind != slang::ast::StatementBlockKind::Sequential) {
    return LowerForkStmt(proc, frame, block, span);
  }
  ProceduralScope scope(
      proc, frame, block.blockSymbol, SequentialKindOf(block));
  const WalkFrame& body_frame = scope.Frame();
  if (block.blockSymbol != nullptr) {
    auto constants =
        proc.DeclarePerObjectConstants(*block.blockSymbol, body_frame);
    if (!constants) return std::unexpected(std::move(constants.error()));
  }

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
          hir::BlockStmt{.statements = std::move(kids), .scope = scope.Seal()},
      .span = span};
}

}  // namespace lyra::lowering::ast_to_hir
