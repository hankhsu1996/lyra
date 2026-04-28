#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/class_decl_id.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local_var.hpp"
#include "lyra/mir/member_var.hpp"

namespace lyra::mir {

struct Stmt;

struct StmtId {
  std::uint32_t value;

  auto operator<=>(const StmtId&) const -> std::strong_ordering = default;
};

struct BodyId {
  std::uint32_t value;

  auto operator<=>(const BodyId&) const -> std::strong_ordering = default;
};

struct Body {
  std::vector<LocalScope> local_scopes;
  LocalScopeId root_scope;

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

struct LocalVarDeclStmt {
  LocalVarRef target;
};

struct ExprStmt {
  ExprId expr;
};

struct BlockStmt {
  std::vector<StmtId> statements;
};

struct IfStmt {
  ExprId condition;
  BodyId then_body;
  std::optional<BodyId> else_body;
};

struct SwitchCase {
  std::vector<ExprId> labels;
  BodyId body;
};

struct SwitchStmt {
  ExprId condition;
  std::vector<SwitchCase> cases;
  std::optional<BodyId> default_body;
};

struct ConstructOwnedObjectStmt {
  MemberVarId target;
  ClassDeclId class_id;
};

struct ForInitDecl {
  LocalVarRef local;
  std::optional<ExprId> init;
};

struct ForInitExpr {
  ExprId expr;
};

using ForInit = std::variant<ForInitDecl, ForInitExpr>;

struct ForStmt {
  LocalScopeId scope;
  std::vector<ForInit> init;
  std::optional<ExprId> condition;
  std::vector<ExprId> step;
  BodyId body;
};

using StmtData = std::variant<
    LocalVarDeclStmt, ExprStmt, BlockStmt, IfStmt, SwitchStmt,
    ConstructOwnedObjectStmt, ForStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
  std::vector<Body> child_bodies;
};

}  // namespace lyra::mir
