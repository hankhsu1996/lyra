#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/structural_scope_id.hpp"
#include "lyra/mir/structural_var.hpp"
#include "lyra/mir/value_ref.hpp"

namespace lyra::mir {

struct Stmt;
struct ProceduralScope;

struct StmtId {
  std::uint32_t value;

  auto operator<=>(const StmtId&) const -> std::strong_ordering = default;
};

// Identifies a loop as a non-local break target (the outermost loop of a
// multi-dimensional `foreach`). The universal labeled-break primitive; the C++
// backend renders it as a `goto` to a label after the loop, an LLVM backend as
// a branch. See `docs/decisions/foreach-lowering.md`.
struct LoopLabelId {
  std::uint32_t value;

  auto operator<=>(const LoopLabelId&) const -> std::strong_ordering = default;
};

struct ProceduralScopeId {
  std::uint32_t value;

  auto operator<=>(const ProceduralScopeId&) const
      -> std::strong_ordering = default;
};

struct EmptyStmt {};

struct ProceduralVarDeclStmt {
  ProceduralVarRef target;
  ExprId init;
};

struct ExprStmt {
  ExprId expr;
};

struct BlockStmt {
  ProceduralScopeId scope;
};

// LRM 9.3.2 Table 9-1: when the forking process resumes relative to its
// branches.
enum class JoinMode : std::uint8_t {
  kAll,
  kAny,
  kNone,
};

// LRM 9.3.2 parallel block. The fork is itself a procedural scope: `scope`
// (in the enclosing procedural scope's child_scopes) holds the
// block_item_declaration locals, which are initialized at block entry -- in the
// parent, before any branch spawns -- giving each spawned branch a by-value
// snapshot. Each branch is a closure (a captured callable value) in `scope`'s
// expr arena, referenced here by id; a branch captures a fork-scope local by
// value (the snapshot) and an enclosing-process variable by reference
// (LRM 6.21). The backend spawns each branch as a concurrent process and the
// parent waits per `mode`. Being a fork branch is what makes the closure run as
// a coroutine -- the closure node itself carries no such property.
struct ForkStmt {
  JoinMode mode;
  ProceduralScopeId scope;
  std::vector<ExprId> branches;
};

struct IfStmt {
  ExprId condition;
  ProceduralScopeId then_scope;
  std::optional<ProceduralScopeId> else_scope;
};

struct ConstructOwnedObjectStmt {
  StructuralVarId target;
  StructuralScopeId scope_id;
  std::vector<ExprId> args;
};

// The cross-unit twin of ConstructOwnedObjectStmt: it carries no scope_id
// because the child is a separate compilation unit, not a nested scope of this
// one. `dims` is empty for a scalar instance and holds one element count per
// array dimension, outermost first; the backend materializes the nested vector
// by replication over these counts.
struct ConstructExternalUnitStmt {
  StructuralVarId target;
  std::string unit_name;
  std::vector<std::uint32_t> dims;
};

struct ForInitDecl {
  ProceduralVarRef induction_var = {};
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
  ProceduralScopeId scope;
  std::optional<LoopLabelId> break_label = std::nullopt;
};

enum class EventEdge : std::uint8_t {
  kAnyChange,
  kPosedge,
  kNegedge,
  kBothEdges,
};

// LRM 9.4.1 `#N`. A time-anchored suspension: the engine schedules the next
// resume at `now + duration`. Standalone -- the controlled body (if any) is
// a separate statement in the enclosing block. Disjoint from the value-change
// suspension family (`SensitivityWaitStmt`) and the named-event method-call
// family: those are data-dependency anchored, this one is clock anchored.
// See `docs/decisions/event-control-unification.md` for the rationale.
struct DelayStmt {
  // `duration` is in the declaring scope's time-precision steps. The scope owns
  // its precision (emitted as a class constant); the runtime scales from there
  // to the design-global tick (LRM 3.14.3).
  SimDuration duration;
};

struct WhileStmt {
  ExprId condition;
  ProceduralScopeId scope;
};

struct DoWhileStmt {
  ExprId condition;
  ProceduralScopeId scope;
};

struct BreakStmt {
  std::optional<LoopLabelId> target = std::nullopt;
};

struct ContinueStmt {};

// LRM 13.4.1 `return [expr];`. `value` carries the returned expression for a
// value-returning function; it is absent for `return;` and for void functions
// / tasks. The semantic concept is one across all consumers (LRM, LIR, LLVM
// `ret`); `is_coroutine_return` is a render hint for the C++ backend only,
// distinguishing `co_return` from plain `return`. HIR-to-MIR sets it from the
// enclosing callable's `is_coroutine`; LIR / LLVM ignore it.
struct ReturnStmt {
  std::optional<ExprId> value;
  bool is_coroutine_return = false;
};

// `await <awaitable>;` -- a suspension point: the enclosing process yields
// control until the awaited operand is ready, then resumes where it left off.
// In SV the only constructs that suspend are void and statement-positioned --
// LRM 13.4 makes a function, the sole value-yielding call form, unable to
// suspend -- so await is a statement, the sibling of `return`, never an
// expression. `awaitable` is the suspension-producing expression (a `$finish`,
// task, or named-event-await call). The semantic concept is one across
// consumers (LIR, LLVM); the C++ backend realizes it as `co_await`.
struct AwaitStmt {
  ExprId awaitable;
};

// One leaf entry of a wait's projection set. Identity-only: which structural
// variable, which flat bit range of its packed encoding, and what edge
// polarity the leaf was subscribed under (LRM 9.4.2 / 9.4.2.2 / 9.4.3). slang
// DFA produces the (var, bit_range) pairs; the SV edge identifier (or
// `kAnyChange` for implicit sensitivity) attaches per leaf at AST lowering.
struct SensitivityRead {
  SensitivityRef ref;
  std::pair<std::uint64_t, std::uint64_t> bit_range;
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
    EmptyStmt, ProceduralVarDeclStmt, ExprStmt, BlockStmt, ForkStmt, IfStmt,
    ConstructOwnedObjectStmt, ConstructExternalUnitStmt, ForStmt, DelayStmt,
    WhileStmt, DoWhileStmt, BreakStmt, ContinueStmt, ReturnStmt, AwaitStmt,
    SensitivityWaitStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
};

// `{...}` in MIR: an arena for the four kinds of locally-owned IR nodes (vars,
// exprs, stmts, child scopes) plus the ordered execution sequence at this
// brace level (`root_stmts`). All four IDs are scope-local: `ExprId`, `StmtId`,
// `ProceduralVarId`, and `ProceduralScopeId` resolve against the same
// ProceduralScope that introduces them. Cross-scope variable references carry
// hops; cross-scope statement / expression / scope references do not exist --
// each ProceduralScope is a self-contained subtree.
struct ProceduralScope {
  std::vector<ProceduralVarDecl> vars;
  std::vector<Expr> exprs;
  std::vector<Stmt> stmts;
  std::vector<ProceduralScope> child_scopes;
  std::vector<StmtId> root_stmts;

  [[nodiscard]] auto GetExpr(ExprId id) const -> const Expr& {
    return exprs.at(id.value);
  }

  [[nodiscard]] auto GetExprType(ExprId id) const -> TypeId {
    return GetExpr(id).type;
  }

  [[nodiscard]] auto GetStmt(StmtId id) const -> const Stmt& {
    return stmts.at(id.value);
  }

  [[nodiscard]] auto GetChildScope(ProceduralScopeId id) const
      -> const ProceduralScope& {
    return child_scopes.at(id.value);
  }

  auto AddProceduralVar(ProceduralVarDecl decl) -> ProceduralVarId {
    const ProceduralVarId id{static_cast<std::uint32_t>(vars.size())};
    vars.push_back(std::move(decl));
    return id;
  }
  [[nodiscard]] auto GetProceduralVar(ProceduralVarId id) const
      -> const ProceduralVarDecl& {
    return vars.at(id.value);
  }
  auto AddExpr(Expr expr) -> ExprId {
    const ExprId id{static_cast<std::uint32_t>(exprs.size())};
    exprs.push_back(std::move(expr));
    return id;
  }
  auto AddStmt(Stmt stmt) -> StmtId {
    const StmtId id{static_cast<std::uint32_t>(stmts.size())};
    stmts.push_back(std::move(stmt));
    return id;
  }
  auto AddChildScope(ProceduralScope scope) -> ProceduralScopeId {
    const ProceduralScopeId id{static_cast<std::uint32_t>(child_scopes.size())};
    child_scopes.push_back(std::move(scope));
    return id;
  }
  void AddRootStmt(StmtId id) {
    root_stmts.push_back(id);
  }
  auto AppendStmt(Stmt stmt) -> StmtId {
    const StmtId id = AddStmt(std::move(stmt));
    root_stmts.push_back(id);
    return id;
  }

  // Append a label-less statement to the body in one step: stage it in the
  // stmts arena and register it as a root statement. Encapsulates the
  // AddStmt-then-push pairing so a synthesized statement cannot be added to
  // the arena yet left out of the executed sequence.
  auto AppendStmt(StmtData data) -> StmtId {
    return AppendStmt(Stmt{.label = std::nullopt, .data = std::move(data)});
  }

  // Declare a body-local variable: register it in the var arena and emit its
  // declaration statement in the body. The two must co-occur for a genuine
  // local, so they are exposed as one operation. (A subroutine formal is a
  // ProceduralVar declared in the signature, not the body, and uses bare
  // AddProceduralVar instead.)
  auto AppendLocal(ProceduralVarDecl decl, ExprId init) -> ProceduralVarRef {
    const ProceduralVarId var = AddProceduralVar(std::move(decl));
    const ProceduralVarRef ref{.hops = ProceduralHops{.value = 0}, .var = var};
    AppendStmt(ProceduralVarDeclStmt{.target = ref, .init = init});
    return ref;
  }

  // Append an `if (cond) <then_body>` statement, registering `then_body` as a
  // child scope of this scope and consuming it. The IfStmt's then_scope id
  // resolves against this scope's child_scopes.
  auto AppendIfThen(ExprId cond, ProceduralScope then_body) -> StmtId {
    const ProceduralScopeId then_scope_id = AddChildScope(std::move(then_body));
    return AppendStmt(
        IfStmt{
            .condition = cond,
            .then_scope = then_scope_id,
            .else_scope = std::nullopt});
  }
};

}  // namespace lyra::mir
