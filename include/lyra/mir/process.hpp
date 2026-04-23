#pragma once

#include <compare>
#include <cstdint>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::mir {

struct ProcessId {
  std::uint32_t value;

  auto operator<=>(const ProcessId&) const -> std::strong_ordering = default;
};

struct Initial {
  StmtId body;
};

using ProcessData = std::variant<Initial>;

struct Process {
  std::string name;
  ProcessData data;
  std::vector<Expr> exprs;
  std::vector<Stmt> stmts;
};

}  // namespace lyra::mir
