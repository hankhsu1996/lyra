#pragma once

#include <memory>
#include <string>
#include <vector>

#include "mir/expression.hpp"

namespace lyra::mir {

class Statement {
 public:
  enum class Kind {
    kAssign,
    kBlock,
    kIf,
    kExpression,
    kDelay,
  };

  Kind kind;

  Statement(const Statement&) = default;
  Statement(Statement&&) = delete;
  auto operator=(const Statement&) -> Statement& = default;
  auto operator=(Statement&&) -> Statement& = delete;

  explicit Statement(Kind kind) : kind(kind) {
  }

  virtual ~Statement() = default;
};

class AssignStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kAssign;
  std::string target;
  std::unique_ptr<Expression> value;

  AssignStatement(std::string t, std::unique_ptr<Expression> v)
      : Statement(kKindValue), target(std::move(t)), value(std::move(v)) {
  }
};

class BlockStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kBlock;
  std::vector<std::unique_ptr<Statement>> statements;

  BlockStatement() : Statement(kKindValue) {
  }
};

class IfStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kIf;
  std::unique_ptr<Expression> condition;
  std::unique_ptr<Statement> then_branch;
  std::unique_ptr<Statement> else_branch;

  IfStatement(
      std::unique_ptr<Expression> cond, std::unique_ptr<Statement> then_b,
      std::unique_ptr<Statement> else_b)
      : Statement(kKindValue),
        condition(std::move(cond)),
        then_branch(std::move(then_b)),
        else_branch(std::move(else_b)) {
  }
};

class DelayStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kDelay;
  int64_t delay_amount;

  explicit DelayStatement(int64_t amount)
      : Statement(kKindValue), delay_amount(amount) {
  }
};

class ExpressionStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kExpression;
  std::unique_ptr<Expression> expression;

  explicit ExpressionStatement(std::unique_ptr<Expression> expression)
      : Statement(kKindValue), expression(std::move(expression)) {
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
