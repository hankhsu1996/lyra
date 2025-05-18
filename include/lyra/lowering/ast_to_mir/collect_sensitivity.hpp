#pragma once

#include <unordered_set>

#include "lyra/common/symbol.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/visitor.hpp"

namespace lyra::lowering {

using SymbolRef = common::SymbolRef;

// Collects all variable names used in the right-hand side of MIR statements.
class SensitivityCollector : public mir::MirVisitor {
 public:
  SensitivityCollector() = default;

  void Visit(const mir::LiteralExpression& /*unused*/) override {
    // No variable involved
  }

  void Visit(const mir::IdentifierExpression& expression) override {
    variable_names_.insert(expression.symbol);
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

  void Visit(const mir::TernaryExpression& expression) override {
    expression.condition->Accept(*this);
    expression.true_expression->Accept(*this);
    expression.false_expression->Accept(*this);
  }

  void Visit(const mir::VariableDeclarationStatement& statement) override {
    if (statement.initializer) {
      statement.initializer->Accept(*this);
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

  [[nodiscard]] auto TakeVariableNames() && -> std::unordered_set<SymbolRef> {
    return std::move(variable_names_);
  }

 private:
  std::unordered_set<SymbolRef> variable_names_;
};

// Entry point
inline auto CollectSensitivityList(const mir::Statement& statement)
    -> std::unordered_set<SymbolRef> {
  SensitivityCollector collector;
  statement.Accept(collector);
  return std::move(collector).TakeVariableNames();
}

}  // namespace lyra::lowering
