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
      auto result = builder.MakeTemp("lit");
      builder.AddInstruction(
          lir::InstructionKind::kLiteralInt, result,
          {lir::Value::MakeLiteralInt(literal.value)});
      return lir::Value::MakeTemp(result);
    }

    case mir::Expression::Kind::kIdentifier: {
      const auto& identifier = mir::As<mir::IdentifierExpression>(expression);
      if (identifier.name.empty()) {
        throw std::runtime_error("Identifier has empty name");
      }
      auto result = builder.MakeTemp("load");
      builder.AddInstruction(
          lir::InstructionKind::kLoadVariable, result,
          {lir::Value::MakeVariable(identifier.name)});
      return lir::Value::MakeTemp(result);
    }

    case mir::Expression::Kind::kBinary: {
      const auto& binary = mir::As<mir::BinaryExpression>(expression);

      if (binary.op != mir::BinaryExpression::Operator::kAdd) {
        throw std::runtime_error(fmt::format(
            "Unsupported binary operator {} in MIR to LIR LowerExpression",
            binary.op));
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
      auto result = builder.MakeTemp("add");
      builder.AddInstruction(
          lir::InstructionKind::kBinaryAdd, result, {lhs_value, rhs_value});

      return lir::Value::MakeTemp(result);
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

      // Store the result to the target variable
      builder.AddInstruction(
          lir::InstructionKind::kStoreVariable, "",
          {lir::Value::MakeVariable(target), value_result});

      // The result of an assignment is the assigned value
      return value_result;
    }

    case mir::Expression::Kind::kSystemCall: {
      const auto& syscall = mir::As<mir::SystemCallExpression>(expression);

      // Create a system call instruction
      lir::Instruction instr;
      instr.kind = lir::InstructionKind::kSystemCall;
      instr.system_call_name = syscall.name;

      // Add any arguments to the system call
      std::vector<lir::Value> operands;
      for (const auto& arg : syscall.arguments) {
        if (arg) {
          operands.push_back(LowerExpression(*arg, builder));
        }
      }

      // If $finish with no arguments, add a default argument value of 1
      if (syscall.name == "$finish" && operands.empty()) {
        operands.push_back(lir::Value::MakeLiteralInt(1));
      }

      builder.AddInstruction(
          lir::InstructionKind::kSystemCall, "", operands, syscall.name);

      // System calls don't produce a value in our current implementation
      // Return a dummy value (this could be improved in the future)
      auto result = builder.MakeTemp("syscall_result");
      return lir::Value::MakeTemp(result);
    }

    default:
      throw std::runtime_error(fmt::format(
          "Unsupported expression kind {} in MIR to LIR LowerExpression",
          expression.kind));
  }
}
}  // namespace lyra::lowering
