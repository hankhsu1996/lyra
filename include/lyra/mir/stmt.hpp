#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"

namespace lyra::mir {

struct Stmt;
struct Block;

struct StmtId {
  std::uint32_t value;

  auto operator<=>(const StmtId&) const -> std::strong_ordering = default;
};

// Identifies a loop as a non-local break target (the outermost loop of a
// multi-dimensional `foreach`). The universal labeled-break primitive; the C++
// backend renders it as a `goto` to a label after the loop, an LLVM backend as
// a branch.
struct LoopLabelId {
  std::uint32_t value;

  auto operator<=>(const LoopLabelId&) const -> std::strong_ordering = default;
};

struct BlockId {
  std::uint32_t value;

  auto operator<=>(const BlockId&) const -> std::strong_ordering = default;
};

struct EmptyStmt {};

struct LocalDeclStmt {
  LocalId target;
  ExprId init;
};

struct ExprStmt {
  ExprId expr;
};

struct BlockStmt {
  BlockId scope;
};

struct IfStmt {
  ExprId condition;
  BlockId then_scope;
  std::optional<BlockId> else_scope;
};

struct ForInitDecl {
  LocalId induction_var = {};
  ExprId init = {};
};

struct ForInitExpr {
  ExprId expr;
};

using ForInit = std::variant<ForInitDecl, ForInitExpr>;

struct ForStmt {
  std::vector<ForInit> init;
  std::optional<ExprId> condition;
  std::vector<ExprId> step;
  BlockId scope;
  std::optional<LoopLabelId> break_label = std::nullopt;
};

struct WhileStmt {
  ExprId condition;
  BlockId scope;
};

struct DoWhileStmt {
  ExprId condition;
  BlockId scope;
};

struct BreakStmt {
  std::optional<LoopLabelId> target = std::nullopt;
};

struct ContinueStmt {};

// LRM 13.4.1 `return [expr];`. `value` carries the returned expression for a
// value-returning function; it is absent for `return;` and for void functions
// / tasks. Whether this return completes a coroutine is the enclosing
// callable's result type, which every consumer already reads; the statement
// does not restate it.
struct ReturnStmt {
  std::optional<ExprId> value;
};

using StmtData = std::variant<
    EmptyStmt, LocalDeclStmt, ExprStmt, BlockStmt, IfStmt, ForStmt, WhileStmt,
    DoWhileStmt, BreakStmt, ContinueStmt, ReturnStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
};

// `{...}` in MIR: arenas for the locally-owned IR nodes (exprs, stmts, child
// scopes) plus the ordered execution sequence at this brace level
// (`root_stmts`). `ExprId`, `StmtId`, and `BlockId` are scope-local: they
// resolve against the Block that introduces them. Bindings are not block-local:
// a `LocalRef` names a binding in the enclosing callable's `locals` arena (a
// captured binding is a field access over the closure receiver), so a
// declaration statement placed in a nested block registers its binding in the
// callable, not in the block.
struct Block {
  base::Arena<Expr, ExprId> exprs;
  base::Arena<Stmt, StmtId> stmts;
  base::Arena<Block, BlockId> child_scopes;
  std::vector<StmtId> root_stmts;

  [[nodiscard]] auto GetExprType(ExprId id) const -> TypeId {
    return exprs.Get(id).type;
  }

  void AddRootStmt(StmtId id) {
    root_stmts.push_back(id);
  }
  auto AppendStmt(Stmt stmt) -> StmtId {
    const StmtId id = stmts.Add(std::move(stmt));
    root_stmts.push_back(id);
    return id;
  }

  // Append a label-less statement to the body in one step: stage it in the
  // stmts arena and register it as a root statement. Encapsulates the
  // stage-then-register pairing so a synthesized statement cannot be added to
  // the arena yet left out of the executed sequence.
  auto AppendStmt(StmtData data) -> StmtId {
    return AppendStmt(Stmt{.label = std::nullopt, .data = std::move(data)});
  }

  // Append an `if (cond) <then_body>` statement, registering `then_body` as a
  // child scope of this scope and consuming it. The IfStmt's then_scope id
  // resolves against this scope's child_scopes.
  auto AppendIfThen(ExprId cond, Block then_body) -> StmtId {
    const BlockId then_scope_id = child_scopes.Add(std::move(then_body));
    return AppendStmt(
        IfStmt{
            .condition = cond,
            .then_scope = then_scope_id,
            .else_scope = std::nullopt});
  }
};

}  // namespace lyra::mir
