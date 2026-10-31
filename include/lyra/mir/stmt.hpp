#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/pool_id.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/mir/block_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct Stmt;
struct Block;

struct StmtId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const StmtId&) const -> std::strong_ordering = default;
};

// Names a statement a `break` leaves: a loop, or a block, whose continuation
// is the statement after it, as a Rust label names a loop or a block. The C++
// backend renders it as a label after the statement, which a `break` naming it
// reaches by `goto`; an LLVM backend as a branch.
struct LabelId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const LabelId&) const -> std::strong_ordering = default;
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
  std::optional<LabelId> break_label = std::nullopt;
};

// A region whose body can be left by a control effect, and whose normal
// continuation is the statement after it. An effect leaving the body binds to
// `caught` and runs `handler`; falling off the end of the handler consumes the
// effect, so execution continues past the region, and raising it again sends
// it on to the region that does consume it.
//
// This is how a program says "this scope can be left from anywhere within it,
// including from a callable it invoked, and execution continues here" -- the
// shape SystemVerilog's `disable` of a named block or task needs (LRM 9.6.2),
// expressed as ordinary structured control flow rather than as a jump whose
// origin the source has to name.
struct TryStmt {
  BlockId body;
  // Binds the effect for the handler, the way a loop binds its induction
  // variable: the handler reads it as an ordinary local.
  LocalId caught;
  BlockId handler;
};

// Leaves the enclosing region carrying a control effect, which then travels
// outward until a region consumes it. The effect is one already in hand: an
// effect arises where an execution regains control, so no statement a program
// can write creates one, and the only statement that carries one is a handler
// passing on an effect it declined.
struct RaiseStmt {
  ExprId effect;
};

// A body paired with a cleanup that runs on every way out of it -- falling off
// its end, a return, a break or continue leaving it, or a control effect
// passing through. It is how a program states an extent whose entry and exit
// are both effects, rather than leaving the exit to a target language that
// destroys a value at scope exit; a language without that facility can still
// realize the extent, because the ways out of a body are enumerable.
//
// The cleanup itself cannot depart. It runs while a departure may already be
// on its way out, and a second one would have nowhere to go: a target that
// runs cleanups as part of unwinding cannot start another unwind from one, and
// a landing inside the cleanup would stand among the very scopes being left.
// A lowering that would have to give a cleanup's code a landing refuses it.
struct FinallyStmt {
  BlockId body;
  BlockId cleanup;
};

struct IfStmt {
  ExprId condition;
  BlockId then_scope;
  std::optional<BlockId> else_scope;
};

// The values one case of a switch is taken at, and what it runs.
struct SwitchCase {
  std::vector<std::int64_t> values;
  BlockId body;
};

// Runs the case one of whose values `discriminant`, a machine integer, holds,
// or `default_body` where none does, as clang's `SwitchStmt` and rustc's
// `SwitchInt` do. No value appears twice, and a case ends at the end of its
// body, so the order of the cases means nothing.
struct SwitchStmt {
  ExprId discriminant;
  std::vector<SwitchCase> cases;
  std::optional<BlockId> default_body;
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
  std::optional<LabelId> break_label = std::nullopt;
};

struct WhileStmt {
  ExprId condition;
  BlockId scope;
  std::optional<LabelId> break_label = std::nullopt;
};

struct DoWhileStmt {
  ExprId condition;
  BlockId scope;
  std::optional<LabelId> break_label = std::nullopt;
};

// Leaves the statement `target` names.
struct BreakStmt {
  LabelId target;
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
    EmptyStmt, LocalDeclStmt, ExprStmt, BlockStmt, TryStmt, RaiseStmt,
    FinallyStmt, IfStmt, SwitchStmt, ForStmt, WhileStmt, DoWhileStmt, BreakStmt,
    ContinueStmt, ReturnStmt>;

// `span` is the piece of source the statement was lowered from, as an
// expression's is.
struct Stmt {
  std::optional<std::string> label;
  StmtData data;
  diag::SourceSpan span{};
};

// Whether `expr` names storage holding a value, which is what a read reads.
[[nodiscard]] inline auto IsReadablePlace(
    const TypePool& types, const Expr& expr) -> bool {
  return IsPlace(expr.data) && !types.Get(expr.type).IsAddressOnly();
}

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
};

}  // namespace lyra::mir
