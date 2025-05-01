#include "lowering/mir_to_lir/expression.hpp"

#include <stdexcept>

#include "lowering/mir_to_lir/lir_builder.hpp"
#include "mir/expression.hpp"

namespace lyra::lowering {

auto LowerExpression(const mir::Expression& expression, LirBuilder& builder)
    -> lir::Value {
  switch (expression.kind) {
    case mir::Expression::Kind::kLiteral: {
      const auto& literal = mir::As<mir::LiteralExpression>(expression);
      auto tmp = builder.MakeTemp("lit");
      builder.AddInstruction(
          lir::InstructionKind::kLiteralInt, tmp,
          {lir::Value::MakeLiteralInt(literal.value)});
      return lir::Value::MakeTemp(tmp);
    }

    case mir::Expression::Kind::kIdentifier: {
      const auto& identifier = mir::As<mir::IdentifierExpression>(expression);
      if (identifier.name.empty()) {
        throw std::runtime_error("Identifier has empty name");
      }
      auto tmp = builder.MakeTemp("load");
      builder.AddInstruction(
          lir::InstructionKind::kLoadSignal, tmp,
          {lir::Value::MakeSignal(identifier.name)});
      return lir::Value::MakeTemp(tmp);
    }

    case mir::Expression::Kind::kBinary: {
      const auto& binary = mir::As<mir::BinaryExpression>(expression);

      if (binary.op != mir::BinaryExpression::Operator::kAdd) {
        throw std::runtime_error("Unsupported binary operator");
      }

      // Handle left operand
      if (!binary.left) {
        throw std::runtime_error("Binary left operand is null");
      }
      auto lhs_value = LowerExpression(*binary.left, builder);

      // Handle right operand
      if (!binary.right) {
        throw std::runtime_error("Binary right operand is null");
      }
      auto rhs_value = LowerExpression(*binary.right, builder);

      // Perform the addition
      auto sum_tmp = builder.MakeTemp("add");
      builder.AddInstruction(
          lir::InstructionKind::kBinaryAdd, sum_tmp, {lhs_value, rhs_value});

      return lir::Value::MakeTemp(sum_tmp);
    }

    case mir::Expression::Kind::kAssignment: {
      const auto& assign = mir::As<mir::AssignmentExpression>(expression);

      const auto& target = assign.target;
      if (target.empty()) {
        throw std::runtime_error("AssignmentExpression has empty target");
      }

      if (!assign.value) {
        throw std::runtime_error("AssignmentExpression has null value");
      }

      // Lower the right-hand side expression
      auto value_result = LowerExpression(*assign.value, builder);

      // Store the result to the target signal
      builder.AddInstruction(
          lir::InstructionKind::kStoreSignal, "",
          {lir::Value::MakeSignal(target), value_result});

      // The result of an assignment is the assigned value
      return value_result;
    }

    default:
      throw std::runtime_error("Unsupported expression kind");
  }
}
}  // namespace lyra::lowering
