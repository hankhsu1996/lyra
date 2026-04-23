#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/lvalue.hpp"

namespace lyra::hir {

struct StmtId {
  std::uint32_t value;

  auto operator<=>(const StmtId&) const -> std::strong_ordering = default;
};

struct BlockingAssignment {
  Lvalue target;
  ExprId value;
};

struct BlockStmt {
  std::vector<StmtId> statements;
};

using StmtData = std::variant<BlockingAssignment, BlockStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
};

}  // namespace lyra::hir
