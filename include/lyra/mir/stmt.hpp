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

struct StmtId {
  std::uint32_t value;

  auto operator<=>(const StmtId&) const -> std::strong_ordering = default;
};

struct ProceduralScopeId {
  std::uint32_t value;

  auto operator<=>(const ProceduralScopeId&) const
      -> std::strong_ordering = default;
};

struct ProceduralScope {
  std::vector<ProceduralVarDecl> vars;
  std::vector<Expr> exprs;
  std::vector<Stmt> stmts;
  std::vector<StmtId> root_stmts;

  [[nodiscard]] auto GetExpr(ExprId id) const -> const Expr& {
    return exprs.at(id.value);
  }

  [[nodiscard]] auto GetExprType(ExprId id) const -> TypeId {
    return GetExpr(id).type;
  }
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

// Resolves a cross-unit reference slot once at construction, after the target
// child is built (reference_resolution.md): the backend materializes a stored
// direct reference to the child's member. The navigation recipe lives in the
// enclosing scope's `cross_unit_refs` table, keyed by `slot`.
struct ResolveCrossUnitRefStmt {
  CrossUnitRefId slot;
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

struct BreakStmt {};

struct ContinueStmt {};

// LRM 13.4.1 `return [expr];`. `value` carries the returned expression for a
// value-returning function; it is absent for `return;` and for void functions
// / tasks. MIR keeps the structured statement; the backend renders it as a
// C++ `return`.
struct ReturnStmt {
  std::optional<ExprId> value;
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
    EmptyStmt, ProceduralVarDeclStmt, ExprStmt, BlockStmt, IfStmt,
    ConstructOwnedObjectStmt, ConstructExternalUnitStmt,
    ResolveCrossUnitRefStmt, ForStmt, DelayStmt, WhileStmt, DoWhileStmt,
    BreakStmt, ContinueStmt, ReturnStmt, SensitivityWaitStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
  std::vector<ProceduralScope> child_procedural_scopes;
};

}  // namespace lyra::mir
