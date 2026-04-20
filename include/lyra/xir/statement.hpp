#pragma once

#include <optional>
#include <variant>
#include <vector>

#include "lyra/common/source_span.hpp"
#include "lyra/xir/fwd.hpp"

namespace lyra::xir {

struct WriteVariable {
  VariableId target;
  ExprId value;
};

struct Block {
  std::vector<StmtId> children;
};

struct IfThenElse {
  ExprId condition;
  StmtId then_branch;
  std::optional<StmtId> else_branch;
};

using StatementData = std::variant<WriteVariable, Block, IfThenElse>;

struct Statement {
  SourceSpan span;
  StatementData data;
};

}  // namespace lyra::xir
