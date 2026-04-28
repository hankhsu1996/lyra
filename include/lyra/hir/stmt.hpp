#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/local_var.hpp"

namespace lyra::hir {

struct StmtId {
  std::uint32_t value;

  auto operator<=>(const StmtId&) const -> std::strong_ordering = default;
};

// VarDeclStmt has ordering semantics in HIR -- its position in the statement
// stream marks the SystemVerilog point of declaration. The actual storage is
// allocated on Process.local_vars; LocalVarId.value indexes into that vector.
struct VarDeclStmt {
  LocalVarId local_var;
};

struct ExprStmt {
  ExprId expr;
};

struct BlockStmt {
  std::vector<StmtId> statements;
};

using StmtData = std::variant<VarDeclStmt, ExprStmt, BlockStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
  diag::SourceSpan span;
};

}  // namespace lyra::hir
