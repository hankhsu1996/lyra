#pragma once

#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <fmt/format.h>

#include "common/trigger.hpp"
#include "mir/expression.hpp"
#include "mir/visitor.hpp"

namespace lyra::mir {

class Statement {
 public:
  enum class Kind {
    kAssign,
    kExpression,
    kWaitEvent,
    kDelay,
    kConditional,
    kWhile,
    kDoWhile,
    kBlock,
  };

  Kind kind;

  Statement(const Statement&) = default;
  Statement(Statement&&) = delete;
  auto operator=(const Statement&) -> Statement& = default;
  auto operator=(Statement&&) -> Statement& = delete;

  explicit Statement(Kind kind) : kind(kind) {
  }

  virtual ~Statement() = default;
  virtual void Accept(MirVisitor& visitor) const = 0;
};

// Convert Statement::Kind to string
inline auto ToString(Statement::Kind kind) -> std::string {
  switch (kind) {
    case Statement::Kind::kAssign:
      return "Assign";
    case Statement::Kind::kExpression:
      return "Expression";
    case Statement::Kind::kWaitEvent:
      return "WaitEvent";
    case Statement::Kind::kDelay:
      return "Delay";
    case Statement::Kind::kConditional:
      return "Conditional";
    case Statement::Kind::kWhile:
      return "While";
    case Statement::Kind::kDoWhile:
      return "DoWhile";
    case Statement::Kind::kBlock:
      return "Block";
  }
}

// Add operator<< for Statement::Kind
inline auto operator<<(std::ostream& os, const Statement::Kind& kind)
    -> std::ostream& {
  return os << ToString(kind);
}

class AssignStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kAssign;
  std::string target;
  std::unique_ptr<Expression> value;

  AssignStatement(std::string t, std::unique_ptr<Expression> v)
      : Statement(kKindValue), target(std::move(t)), value(std::move(v)) {
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

class ExpressionStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kExpression;
  std::unique_ptr<Expression> expression;

  explicit ExpressionStatement(std::unique_ptr<Expression> expression)
      : Statement(kKindValue), expression(std::move(expression)) {
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

class WaitEventStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kWaitEvent;

  std::vector<common::Trigger<std::string>> triggers;

  explicit WaitEventStatement(
      std::vector<common::Trigger<std::string>> trigger_list)
      : Statement(kKindValue), triggers(std::move(trigger_list)) {
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

class DelayStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kDelay;
  uint64_t delay_amount;

  explicit DelayStatement(uint64_t amount)
      : Statement(kKindValue), delay_amount(amount) {
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

class ConditionalStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kConditional;
  std::unique_ptr<Expression> condition;
  std::unique_ptr<Statement> then_branch;
  std::unique_ptr<Statement> else_branch;

  ConditionalStatement(
      std::unique_ptr<Expression> cond, std::unique_ptr<Statement> then_b,
      std::unique_ptr<Statement> else_b)
      : Statement(kKindValue),
        condition(std::move(cond)),
        then_branch(std::move(then_b)),
        else_branch(std::move(else_b)) {
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

class WhileStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kWhile;
  std::unique_ptr<Expression> condition;
  std::unique_ptr<Statement> body;

  WhileStatement(
      std::unique_ptr<Expression> condition, std::unique_ptr<Statement> body)
      : Statement(kKindValue),
        condition(std::move(condition)),
        body(std::move(body)) {
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

class DoWhileStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kDoWhile;
  std::unique_ptr<Expression> condition;
  std::unique_ptr<Statement> body;

  DoWhileStatement(
      std::unique_ptr<Expression> condition, std::unique_ptr<Statement> body)
      : Statement(kKindValue),
        condition(std::move(condition)),
        body(std::move(body)) {
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }
};

class BlockStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kBlock;
  std::vector<std::unique_ptr<Statement>> statements;

  BlockStatement() : Statement(kKindValue) {
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
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

// Add formatter for Statement::Kind
template <>
struct fmt::formatter<lyra::mir::Statement::Kind> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(
      const lyra::mir::Statement::Kind& kind, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", lyra::mir::ToString(kind));
  }
};
