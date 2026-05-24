#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/procedural_var.hpp"

namespace lyra::hir {

struct StmtId {
  std::uint32_t value;

  auto operator<=>(const StmtId&) const -> std::strong_ordering = default;
};

struct EmptyStmt {};

// VarDeclStmt has ordering semantics in HIR -- its position in the statement
// stream marks the SystemVerilog point of declaration. The actual storage is
// allocated on Process.procedural_vars; ProceduralVarId.value indexes into
// that vector. The optional init expression holds the initial value written in
// the declaration (e.g. `int x = 7`); it is evaluated at the point of the
// statement.
struct VarDeclStmt {
  ProceduralVarId var = {};
  std::optional<ExprId> init;
};

struct ExprStmt {
  ExprId expr;
};

struct BlockStmt {
  std::vector<StmtId> statements;
};

struct IfStmt {
  ExprId condition;
  StmtId then_stmt;
  std::optional<StmtId> else_stmt;
};

struct CaseItem {
  std::vector<ExprId> labels;
  StmtId stmt;
};

struct CaseStmt {
  ExprId condition;
  std::vector<CaseItem> items;
  std::optional<StmtId> default_stmt;
};

struct ForInitDecl {
  ProceduralVarId var = {};
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
  StmtId body;
};

struct DelayControl {
  ExprId duration;
};

struct EventControl {};

using TimingControl = std::variant<DelayControl, EventControl>;

struct TimedStmt {
  TimingControl timing;
  StmtId stmt;
};

using StmtData = std::variant<
    EmptyStmt, VarDeclStmt, ExprStmt, BlockStmt, IfStmt, CaseStmt, ForStmt,
    TimedStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
  diag::SourceSpan span;
};

}  // namespace lyra::hir
