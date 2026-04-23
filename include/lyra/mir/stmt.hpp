#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/mir/expr.hpp"
#include "lyra/mir/member.hpp"

namespace lyra::mir {

struct StmtId {
  std::uint32_t value;

  auto operator<=>(const StmtId&) const -> std::strong_ordering = default;
};

struct Assignment {
  MemberId target;
  ExprId value;
};

struct BlockStmt {
  std::vector<StmtId> statements;
};

using StmtData = std::variant<Assignment, BlockStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
};

}  // namespace lyra::mir
