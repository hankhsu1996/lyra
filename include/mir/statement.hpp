#pragma once

#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "common/timing_control.hpp"
#include "mir/expression.hpp"

namespace lyra::mir {

class Statement {
 public:
  enum class Kind {
    kAssign,
    kBlock,
    kIf,
    kExpr,
  };

  Kind kind;
  std::optional<common::TimingControl> timing_control;

  Statement(const Statement&) = default;
  Statement(Statement&&) = delete;
  auto operator=(const Statement&) -> Statement& = default;
  auto operator=(Statement&&) -> Statement& = delete;

  explicit Statement(
      Kind kind, std::optional<common::TimingControl> timing = std::nullopt)
      : kind(kind), timing_control(std::move(timing)) {
  }

  virtual ~Statement() = default;
};

class AssignStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kAssign;
  std::string target;
  std::shared_ptr<Expression> value;

  AssignStatement(
      std::string t, std::shared_ptr<Expression> v,
      std::optional<common::TimingControl> tc = std::nullopt)
      : Statement(kKindValue, std::move(tc)),
        target(std::move(t)),
        value(std::move(v)) {
  }
};

class BlockStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kBlock;
  std::vector<std::shared_ptr<Statement>> statements;

  explicit BlockStatement(
      std::optional<common::TimingControl> tc = std::nullopt)
      : Statement(kKindValue, std::move(tc)) {
  }
};

class IfStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kIf;
  std::shared_ptr<Expression> condition;
  std::shared_ptr<Statement> then_branch;
  std::shared_ptr<Statement> else_branch;

  IfStatement(
      std::shared_ptr<Expression> cond, std::shared_ptr<Statement> then_b,
      std::shared_ptr<Statement> else_b,
      std::optional<common::TimingControl> tc = std::nullopt)
      : Statement(kKindValue, std::move(tc)),
        condition(std::move(cond)),
        then_branch(std::move(then_b)),
        else_branch(std::move(else_b)) {
  }
};

// Helper function for safely casting statements
template <typename T>
auto As(const Statement& stmt) -> const T& {
  if (stmt.kind != T::kKindValue) {
    throw std::runtime_error("Statement kind mismatch in As<T>() cast");
  }
  return static_cast<const T&>(stmt);
}

}  // namespace lyra::mir
