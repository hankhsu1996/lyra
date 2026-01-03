#pragma once

#include <format>
#include <memory>
#include <ostream>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <fmt/format.h>

#include "lyra/common/formatting.hpp"
#include "lyra/common/trigger.hpp"
#include "lyra/common/variable.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/visitor.hpp"

namespace lyra::mir {

class Statement {
 public:
  enum class Kind {
    // Core statements
    kVariableDeclaration,
    kAssign,
    kExpression,

    // Timing control statements
    kWaitEvent,
    kDelay,

    // Control flow statements
    kConditional,
    kWhile,
    kDoWhile,
    kFor,
    kBreak,
    kContinue,

    // Statement blocks
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
  [[nodiscard]] virtual auto ToString(int indent) const -> std::string = 0;
};

// Convert Statement::Kind to string
inline auto ToString(Statement::Kind kind) -> std::string {
  switch (kind) {
    case Statement::Kind::kVariableDeclaration:
      return "VariableDeclaration";
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
    case Statement::Kind::kFor:
      return "For";
    case Statement::Kind::kBreak:
      return "Break";
    case Statement::Kind::kContinue:
      return "Continue";
    case Statement::Kind::kBlock:
      return "Block";
  }
}

// Add operator<< for Statement::Kind
inline auto operator<<(std::ostream& os, const Statement::Kind& kind)
    -> std::ostream& {
  return os << ToString(kind);
}

class VariableDeclarationStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kVariableDeclaration;

  common::Variable variable;
  std::unique_ptr<Expression> initializer;

  VariableDeclarationStatement(
      common::Variable v, std::unique_ptr<Expression> i)
      : Statement(kKindValue),
        variable(std::move(v)),
        initializer(std::move(i)) {
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }

  [[nodiscard]] auto ToString(int indent) const -> std::string override {
    auto init = initializer ? " = " + initializer->ToString() : "";
    return std::format(
        "{}var {}{}\n", common::Indent(indent), variable.symbol->name, init);
  }
};

class AssignStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kAssign;
  SymbolRef target;
  std::unique_ptr<Expression> value;

  AssignStatement(SymbolRef t, std::unique_ptr<Expression> v)
      : Statement(kKindValue), target(std::move(t)), value(std::move(v)) {
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }

  [[nodiscard]] auto ToString(int indent) const -> std::string override {
    return std::format(
        "{}{} = {}\n", common::Indent(indent), target->name, value->ToString());
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

  [[nodiscard]] auto ToString(int indent) const -> std::string override {
    return std::format(
        "{}{}\n", common::Indent(indent), expression->ToString());
  }
};

class WaitEventStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kWaitEvent;

  std::vector<common::Trigger> triggers;

  explicit WaitEventStatement(std::vector<common::Trigger> trigger_list)
      : Statement(kKindValue), triggers(std::move(trigger_list)) {
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }

  [[nodiscard]] auto ToString(int indent) const -> std::string override {
    return std::format("{}@(...)\n", common::Indent(indent));
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

  [[nodiscard]] auto ToString(int indent) const -> std::string override {
    return std::format("{}#{}\n", common::Indent(indent), delay_amount);
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

  [[nodiscard]] auto ToString(int indent) const -> std::string override {
    std::string result =
        std::format("{}if {}\n", common::Indent(indent), condition->ToString());
    if (then_branch) {
      result += then_branch->ToString(indent + 1);
    }
    if (else_branch) {
      result += std::format("{}else\n", common::Indent(indent));
      result += else_branch->ToString(indent + 1);
    }
    return result;
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

  [[nodiscard]] auto ToString(int indent) const -> std::string override {
    auto result = std::format(
        "{}while {}\n", common::Indent(indent), condition->ToString());
    if (body) {
      result += body->ToString(indent + 1);
    }
    return result;
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

  [[nodiscard]] auto ToString(int indent) const -> std::string override {
    std::string result = std::format("{}do\n", common::Indent(indent));
    if (body) {
      result += body->ToString(indent + 1);
    }
    result += std::format(
        "{}while {}\n", common::Indent(indent), condition->ToString());
    return result;
  }
};

class ForStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kFor;
  // Loop variable declarations (e.g., int i = 0)
  std::vector<std::unique_ptr<Statement>> initializers;
  // Condition expression (nullptr = infinite loop)
  std::unique_ptr<Expression> condition;
  // Step expressions (e.g., i = i + 1)
  std::vector<std::unique_ptr<Expression>> steps;
  // Loop body
  std::unique_ptr<Statement> body;

  ForStatement(
      std::vector<std::unique_ptr<Statement>> initializers,
      std::unique_ptr<Expression> condition,
      std::vector<std::unique_ptr<Expression>> steps,
      std::unique_ptr<Statement> body)
      : Statement(kKindValue),
        initializers(std::move(initializers)),
        condition(std::move(condition)),
        steps(std::move(steps)),
        body(std::move(body)) {
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }

  [[nodiscard]] auto ToString(int indent) const -> std::string override {
    auto result = std::format("{}for (...)\n", common::Indent(indent));
    if (body) {
      result += body->ToString(indent + 1);
    }
    return result;
  }
};

class BreakStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kBreak;

  BreakStatement() : Statement(kKindValue) {
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }

  [[nodiscard]] auto ToString(int indent) const -> std::string override {
    return std::format("{}break\n", common::Indent(indent));
  }
};

class ContinueStatement : public Statement {
 public:
  static constexpr Kind kKindValue = Kind::kContinue;

  ContinueStatement() : Statement(kKindValue) {
  }

  void Accept(MirVisitor& visitor) const override {
    visitor.Visit(*this);
  }

  [[nodiscard]] auto ToString(int indent) const -> std::string override {
    return std::format("{}continue\n", common::Indent(indent));
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

  [[nodiscard]] auto ToString(int indent) const -> std::string override {
    std::string result;
    for (const auto& stmt : statements) {
      result += stmt->ToString(indent);
    }
    return result;
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
