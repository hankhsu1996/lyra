#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/expr.hpp"
#include "lyra/mir/local_var.hpp"

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
  std::vector<LocalVar> local_vars;
  std::vector<Expr> exprs;
  std::vector<Stmt> stmts;
  std::vector<StmtId> root_stmts;
};

struct LocalVarDeclStmt {
  LocalVarId local_var;
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

using StmtData =
    std::variant<LocalVarDeclStmt, ExprStmt, BlockStmt, IfStmt, SwitchStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
  std::vector<Body> child_bodies;
};

}  // namespace lyra::mir
