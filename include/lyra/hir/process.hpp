#pragma once

#include <compare>
#include <cstdint>
#include <variant>
#include <vector>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/stmt.hpp"

namespace lyra::hir {

struct ProcessId {
  std::uint32_t value;

  auto operator<=>(const ProcessId&) const -> std::strong_ordering = default;
};

struct Initial {
  StmtId body;
};

using ProcessData = std::variant<Initial>;

struct Process {
  ProcessData data;
  std::vector<Expr> exprs;
  std::vector<Stmt> stmts;
};

}  // namespace lyra::hir
