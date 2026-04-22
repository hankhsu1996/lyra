#pragma once

#include <utility>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/xir/expression.hpp"
#include "lyra/xir/statement.hpp"

namespace lyra::xir {

class Arena {
 public:
  auto Add(Expression expr) -> ExprId {
    ExprId id{static_cast<uint32_t>(expressions_.size())};
    expressions_.push_back(std::move(expr));
    return id;
  }

  auto Add(Statement stmt) -> StmtId {
    StmtId id{static_cast<uint32_t>(statements_.size())};
    statements_.push_back(std::move(stmt));
    return id;
  }

  [[nodiscard]] auto operator[](ExprId id) const -> const Expression& {
    if (id.value >= expressions_.size()) {
      throw common::InternalError("xir::Arena", "ExprId out of range");
    }
    return expressions_[id.value];
  }

  [[nodiscard]] auto operator[](StmtId id) const -> const Statement& {
    if (id.value >= statements_.size()) {
      throw common::InternalError("xir::Arena", "StmtId out of range");
    }
    return statements_[id.value];
  }

 private:
  std::vector<Expression> expressions_;
  std::vector<Statement> statements_;
};

}  // namespace lyra::xir
