#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/time.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/structural_scope_id.hpp"
#include "lyra/mir/structural_var.hpp"

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

struct SwitchCase {
  std::vector<ExprId> labels;
  ProceduralScopeId scope;
};

struct SwitchStmt {
  ExprId condition;
  std::vector<SwitchCase> cases;
  std::optional<ProceduralScopeId> default_scope;
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

using TimingControl = std::variant<DelayControl>;

struct TimedStmt {
  TimingControl timing;
  StmtId stmt;
};

struct WhileStmt {
  ExprId condition;
  ProceduralScopeId scope;
};

enum class AwaitKind : std::uint8_t {
  kAlwaysBackedge,
};

struct AwaitStmt {
  AwaitKind kind;
};

using StmtData = std::variant<
    EmptyStmt, ProceduralVarDeclStmt, ExprStmt, BlockStmt, IfStmt, SwitchStmt,
    ConstructOwnedObjectStmt, ForStmt, TimedStmt, WhileStmt, AwaitStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
  std::vector<ProceduralScope> child_procedural_scopes;
};

}  // namespace lyra::mir
