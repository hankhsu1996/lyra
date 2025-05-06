#pragma once

#include <string>
#include <unordered_set>

#include "mir/statement.hpp"
#include "mir/visitor.hpp"

namespace lyra::lowering {

// Collects all variable names used in the right-hand side of MIR statements.
class SensitivityCollector : public mir::MirVisitor {
 public:
  SensitivityCollector() = default;

  void Visit(const mir::LiteralExpression& /*unused*/) override {
    // No variable involved
  }

  void Visit(const mir::IdentifierExpression& expression) override {
    variable_names_.insert(expression.name);
  }

  void Visit(const mir::UnaryExpression& expression) override {
    expression.operand->Accept(*this);
  }

  void Visit(const mir::BinaryExpression& expression) override {
    expression.left->Accept(*this);
    expression.right->Accept(*this);
  }

  void Visit(const mir::AssignmentExpression& expression) override {
    expression.value->Accept(*this);
  }

  void Visit(const mir::ConversionExpression& expression) override {
    expression.value->Accept(*this);
  }

  void Visit(const mir::SystemCallExpression& expression) override {
    for (const auto& argument : expression.arguments) {
      argument->Accept(*this);
    }
  }

  void Visit(const mir::AssignStatement& statement) override {
    statement.value->Accept(*this);
  }

  void Visit(const mir::ExpressionStatement& statement) override {
    statement.expression->Accept(*this);
  }

  void Visit(const mir::WaitEventStatement& /*unused*/) override {
    // No variable involved in triggers
  }

  void Visit(const mir::DelayStatement& /*unused*/) override {
  }

  void Visit(const mir::ConditionalStatement& statement) override {
    statement.condition->Accept(*this);
    statement.then_branch->Accept(*this);
    if (statement.else_branch) {
      statement.else_branch->Accept(*this);
    }
  }

  void Visit(const mir::WhileStatement& statement) override {
    statement.condition->Accept(*this);
    statement.body->Accept(*this);
  }

  void Visit(const mir::DoWhileStatement& statement) override {
    statement.condition->Accept(*this);
    statement.body->Accept(*this);
  }

  void Visit(const mir::BlockStatement& statement) override {
    for (const auto& inner_statement : statement.statements) {
      inner_statement->Accept(*this);
    }
  }

  [[nodiscard]] auto TakeVariableNames() && -> std::unordered_set<std::string> {
    return std::move(variable_names_);
  }

 private:
  std::unordered_set<std::string> variable_names_;
};

// Entry point
inline auto CollectSensitivityList(const mir::Statement& statement)
    -> std::unordered_set<std::string> {
  SensitivityCollector collector;
  statement.Accept(collector);
  return std::move(collector).TakeVariableNames();
}

}  // namespace lyra::lowering
