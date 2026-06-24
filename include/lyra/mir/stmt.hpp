#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/value_ref.hpp"

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
  LocalRef target;
  ExprId init;
};

struct ExprStmt {
  ExprId expr;
};

struct BlockStmt {
  BlockId scope;
};

// LRM 9.3.2 Table 9-1: when the forking process resumes relative to its
// branches.
enum class JoinMode : std::uint8_t {
  kAll,
  kAny,
  kNone,
};

// LRM 9.3.2 parallel block. The fork is itself a block: `scope` (in the
// enclosing block's child_scopes) holds the block_item_declaration locals,
// which are initialized at block entry -- in the
// parent, before any branch spawns -- giving each spawned branch a by-value
// snapshot. Each branch is a coroutine-typed ClosureExpr in `scope`'s expr
// arena, referenced here by id; the branch captures its environment (its
// receiver `self`, a fork-scope local by value, an enclosing variable by
// reference through a `Ref<T>`, LRM 6.21). The backend spawns each branch's
// coroutine and the parent waits per `mode`. The branch runs as a coroutine
// because its result type is the coroutine type, not because of any flag on the
// closure node.
struct ForkStmt {
  JoinMode mode;
  BlockId scope;
  std::vector<ExprId> branches;
};

struct IfStmt {
  ExprId condition;
  BlockId then_scope;
  std::optional<BlockId> else_scope;
};

struct ConstructOwnedObjectStmt {
  MemberId target;
  ClassId scope_id;
  std::vector<ExprId> args;
};

// The cross-unit twin of ConstructOwnedObjectStmt: it carries no scope_id
// because the child is a separate compilation unit, not a nested scope of this
// one. `dims` is empty for a scalar instance and holds one element count per
// array dimension, outermost first; the backend materializes the nested vector
// by replication over these counts.
struct ConstructExternalUnitStmt {
  MemberId target;
  std::string unit_name;
  std::vector<std::uint32_t> dims;
};

struct ForInitDecl {
  LocalRef induction_var = {};
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

enum class EventEdge : std::uint8_t {
  kAnyChange,
  kPosedge,
  kNegedge,
  kBothEdges,
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
// / tasks. The semantic concept is one across all consumers (LRM, LIR, LLVM
// `ret`); `is_coroutine_return` is a render hint for the C++ backend only,
// distinguishing `co_return` from plain `return`. HIR-to-MIR sets it from
// whether the enclosing callable body is a coroutine; LIR / LLVM ignore it.
struct ReturnStmt {
  std::optional<ExprId> value;
  bool is_coroutine_return = false;
};

// One leaf entry of a wait's projection set: an expression yielding a
// borrowed pointer to the observable cell the leaf subscribes to, plus the
// observed bit projection of its packed encoding as a `(lsb_bit_offset,
// bit_width)` pair, and the edge polarity the leaf was subscribed under (LRM
// 9.4.2 / 9.4.2.2 / 9.4.3). A `bit_width` of 0 is the whole signal observed
// on any change; an edge reduces to the bit at `lsb_bit_offset`. The
// projection is resolved at HIR-to-MIR from the SV-level footprint so a
// backend emits the pair directly. `kAnyChange` is the edge for implicit
// sensitivity. The pointer expression's exact shape -- `AddressOf` of a
// place, a bare borrowed-pointer member access, or a call that resolves an
// upward reference to its current observable -- is stated by HIR-to-MIR so
// the backend never re-derives it from the leaf's type.
struct SensitivityRead {
  ExprId observable_ptr{};
  std::uint64_t lsb_bit_offset = 0;
  std::uint64_t bit_width = 0;
  EventEdge edge_kind = EventEdge::kAnyChange;
};

// Suspends the enclosing process until any signal in `reads` changes. Lowered
// only at the tail of an always_comb / always_latch body's forever loop. An
// empty `reads` list legitimately means "never wake up": the body runs once
// (at time 0) then the process hangs forever.
struct SensitivityWaitStmt {
  std::vector<SensitivityRead> reads;
};

using StmtData = std::variant<
    EmptyStmt, LocalDeclStmt, ExprStmt, BlockStmt, ForkStmt, IfStmt,
    ConstructOwnedObjectStmt, ConstructExternalUnitStmt, ForStmt, WhileStmt,
    DoWhileStmt, BreakStmt, ContinueStmt, ReturnStmt, SensitivityWaitStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
};

// `{...}` in MIR: arenas for the four kinds of locally-owned IR nodes (vars,
// exprs, stmts, child scopes) plus the ordered execution sequence at this
// brace level (`root_stmts`). All four IDs are scope-local: `ExprId`, `StmtId`,
// `LocalId`, and `BlockId` resolve against the same
// Block that introduces them. Cross-scope variable references carry
// hops; cross-scope statement / expression / scope references do not exist --
// each Block is a self-contained subtree.
struct Block {
  base::Arena<LocalDecl, LocalId> vars;
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

  // Declare a body-local variable: register it in the var arena and emit its
  // declaration statement in the body. The two must co-occur for a genuine
  // local, so they are exposed as one operation. (A method formal is a local
  // declared in the signature, not the body, so it is registered without a
  // declaration statement.)
  auto AppendLocal(LocalDecl decl, ExprId init) -> LocalRef {
    const LocalId var = vars.Add(std::move(decl));
    const LocalRef ref{.hops = BlockHops{.value = 0}, .var = var};
    AppendStmt(LocalDeclStmt{.target = ref, .init = init});
    return ref;
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
