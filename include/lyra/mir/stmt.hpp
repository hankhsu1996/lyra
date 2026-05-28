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
  std::optional<ExprId> init;
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

struct ForInitDecl {
  ProceduralVarRef induction_var = {};
  std::optional<ExprId> init;
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

struct DelayControl {
  SimDuration duration;
};

enum class EventEdge : std::uint8_t {
  kAnyChange,
  kPosedge,
  kNegedge,
  kBothEdges,
};

struct EventTrigger {
  ExprId signal;
  EventEdge edge;
};

struct EventControl {
  std::vector<EventTrigger> triggers;
};

using TimingControl = std::variant<DelayControl, EventControl>;

struct TimedStmt {
  TimingControl timing;
  StmtId stmt;
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

// One entry of the LRM 9.2.2.2.1 implicit sensitivity list. Identity-only:
// which structural variable, which flat bit range of its packed encoding.
// Distinct from EventTrigger by design -- EventTrigger carries an expression
// (explicit `@(...)`); SensitivityRead carries identity (slang DFA result,
// never reassembled into an expression).
struct SensitivityRead {
  StructuralVarRef ref;
  std::pair<std::uint64_t, std::uint64_t> bit_range;
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
    ConstructOwnedObjectStmt, ForStmt, TimedStmt, WhileStmt, DoWhileStmt,
    BreakStmt, ContinueStmt, SensitivityWaitStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
  std::vector<ProceduralScope> child_procedural_scopes;
};

}  // namespace lyra::mir
