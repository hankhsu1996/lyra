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

// A process owns the statements and expressions that make up its body.
// Process is a value: it carries its body content but not its own identity,
// because ProcessId is produced by the owning ModuleUnit at installation time.
struct Process {
  ProcessData data;
  std::vector<Expr> exprs;
  std::vector<Stmt> stmts;
};

}  // namespace lyra::hir
