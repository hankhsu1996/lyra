#pragma once

#include <vector>

#include "lyra/common/symbol.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/statement.hpp"
#include "lyra/mir/visitor.hpp"

namespace lyra::lowering::ast_to_mir {

using SymbolRef = common::SymbolRef;

// Represents a sensitivity item: a symbol with optional instance path.
// For local variables, instance_path is empty.
// For hierarchical references, instance_path contains the traversal symbols.
struct SensitivityItem {
  SymbolRef symbol;
  std::vector<SymbolRef> instance_path;

  auto operator==(const SensitivityItem& other) const -> bool {
    return symbol == other.symbol && instance_path == other.instance_path;
  }
};

// Collects all variable names used in the right-hand side of MIR statements.
class SensitivityCollector : public mir::MirVisitor {
 public:
  SensitivityCollector() = default;

  void Visit(const mir::LiteralExpression& /*unused*/) override {
    // No variable involved
  }

  void Visit(const mir::EnumValueExpression& /*unused*/) override {
    // Enum values are constants, no variable involved
  }

  void Visit(const mir::IdentifierExpression& expression) override {
    items_.push_back({expression.symbol, {}});
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

  void Visit(const mir::ElementSelectExpression& expression) override {
    expression.value->Accept(*this);
    expression.selector->Accept(*this);
  }

  void Visit(const mir::RangeSelectExpression& expression) override {
    expression.value->Accept(*this);
    // left and right are constant integers, not expressions
  }

  void Visit(const mir::IndexedRangeSelectExpression& expression) override {
    expression.value->Accept(*this);
    expression.start->Accept(*this);
    // width is a constant integer, not an expression
  }

  void Visit(const mir::HierarchicalReferenceExpression& expression) override {
    // Add hierarchical reference with instance path for sensitivity tracking
    items_.push_back({expression.target_symbol, expression.instance_path});
  }

  void Visit(const mir::ConcatenationExpression& expression) override {
    for (const auto& operand : expression.operands) {
      operand->Accept(*this);
    }
  }

  void Visit(const mir::ReplicationExpression& expression) override {
    expression.operand->Accept(*this);
  }

  void Visit(const mir::FunctionCallExpression& expression) override {
    for (const auto& argument : expression.arguments) {
      argument->Accept(*this);
    }
  }

  void Visit(const mir::MemberAccessExpression& expression) override {
    expression.value->Accept(*this);
  }

  void Visit(const mir::NewArrayExpression& expression) override {
    expression.size_expr->Accept(*this);
    if (expression.init_expr) {
      expression.init_expr->Accept(*this);
    }
  }

  void Visit(const mir::MethodCallExpression& expression) override {
    // Visit receiver and arguments to collect sensitivity
    expression.receiver->Accept(*this);
    for (const auto& arg : expression.args) {
      arg->Accept(*this);
    }
  }

  void Visit(const mir::UnpackedStructLiteralExpression& expression) override {
    for (const auto& field_value : expression.field_values) {
      field_value->Accept(*this);
    }
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

  void Visit(const mir::BreakStatement& /*unused*/) override {
    // No variable involved in break
  }

  void Visit(const mir::ContinueStatement& /*unused*/) override {
    // No variable involved in continue
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

  void Visit(const mir::ForStatement& statement) override {
    for (const auto& init : statement.initializers) {
      init->Accept(*this);
    }
    if (statement.condition) {
      statement.condition->Accept(*this);
    }
    for (const auto& step : statement.steps) {
      step->Accept(*this);
    }
    statement.body->Accept(*this);
  }

  void Visit(const mir::RepeatStatement& statement) override {
    statement.count->Accept(*this);
    statement.body->Accept(*this);
  }

  void Visit(const mir::CaseStatement& statement) override {
    statement.condition->Accept(*this);
    for (const auto& item : statement.items) {
      for (const auto& expr : item.expressions) {
        expr->Accept(*this);
      }
      if (item.statement) {
        item.statement->Accept(*this);
      }
    }
    if (statement.default_case) {
      statement.default_case->Accept(*this);
    }
  }

  void Visit(const mir::BlockStatement& statement) override {
    for (const auto& inner_statement : statement.statements) {
      inner_statement->Accept(*this);
    }
  }

  void Visit(const mir::ReturnStatement& statement) override {
    if (statement.value) {
      statement.value->Accept(*this);
    }
  }

  [[nodiscard]] auto TakeItems() && -> std::vector<SensitivityItem> {
    return std::move(items_);
  }

 private:
  std::vector<SensitivityItem> items_;
};

// Entry point for statements
inline auto CollectSensitivityList(const mir::Statement& statement)
    -> std::vector<SensitivityItem> {
  SensitivityCollector collector;
  statement.Accept(collector);
  return std::move(collector).TakeItems();
}

// Entry point for expressions
inline auto CollectSensitivityList(const mir::Expression& expression)
    -> std::vector<SensitivityItem> {
  SensitivityCollector collector;
  expression.Accept(collector);
  return std::move(collector).TakeItems();
}

}  // namespace lyra::lowering::ast_to_mir
