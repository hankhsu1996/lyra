#include "lowering/mir_to_lir/expression.hpp"

#include <cassert>
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

      if (!binary.left || !binary.right) {
        throw std::runtime_error("Binary expression operands cannot be null");
      }

      auto lhs_value = LowerExpression(*binary.left, builder);
      auto rhs_value = LowerExpression(*binary.right, builder);

      return LowerBinaryExpression(binary, lhs_value, rhs_value, builder);
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
auto LowerBinaryExpression(
    const mir::BinaryExpression& expression, lir::Value lhs, lir::Value rhs,
    LirBuilder& builder) -> lir::Value {
  using lir::InstructionKind;
  using mir::Operator;

  const bool is_lhs_string = lhs.kind == lir::Value::Kind::kLiteralString;
  const bool is_rhs_string = rhs.kind == lir::Value::Kind::kLiteralString;
  const bool is_string = is_lhs_string || is_rhs_string;

  InstructionKind kind = InstructionKind::kBinaryAdd;

  // Special case handling: equality/inequality with string
  if (is_string) {
    switch (expression.op) {
      case Operator::kEquality:
        kind = InstructionKind::kBinaryEqualString;
        break;
      case Operator::kInequality:
        kind = InstructionKind::kBinaryNotEqualString;
        break;
      default:
        throw std::runtime_error(fmt::format(
            "Operator {} is not supported for string operands", expression.op));
    }
  } else {
    switch (expression.op) {
      case Operator::kAddition:
        kind = InstructionKind::kBinaryAdd;
        break;
      case Operator::kSubtraction:
        kind = InstructionKind::kBinarySubtract;
        break;
      case Operator::kMultiplication:
        kind = InstructionKind::kBinaryMultiply;
        break;
      case Operator::kDivision:
        kind = InstructionKind::kBinaryDivide;
        break;
      case Operator::kModulo:
        kind = InstructionKind::kBinaryModulo;
        break;
      case Operator::kEquality:
        kind = InstructionKind::kBinaryEqualInt;
        break;
      case Operator::kInequality:
        kind = InstructionKind::kBinaryNotEqualInt;
        break;
      case Operator::kLessThan:
        kind = InstructionKind::kBinaryLessThan;
        break;
      case Operator::kLessThanEqual:
        kind = InstructionKind::kBinaryLessThanEqual;
        break;
      case Operator::kGreaterThan:
        kind = InstructionKind::kBinaryGreaterThan;
        break;
      case Operator::kGreaterThanEqual:
        kind = InstructionKind::kBinaryGreaterThanEqual;
        break;
      case Operator::kPower:
      case Operator::kBitwiseAnd:
      case Operator::kBitwiseOr:
      case Operator::kBitwiseXor:
      case Operator::kBitwiseXnor:
      case Operator::kLogicalAnd:
      case Operator::kLogicalOr:
      case Operator::kLogicalImplication:
      case Operator::kLogicalEquivalence:
      case Operator::kCaseEquality:
      case Operator::kCaseInequality:
      case Operator::kWildcardEquality:
      case Operator::kWildcardInequality:
      case Operator::kLogicalShiftLeft:
      case Operator::kLogicalShiftRight:
      case Operator::kArithmeticShiftLeft:
      case Operator::kArithmeticShiftRight:
        throw std::runtime_error(fmt::format(
            "Operator {} is not supported (yet) in LowerBinaryExpression",
            expression.op));
    }
  }

  auto result = builder.MakeTemp("bin");
  builder.AddInstruction(kind, result, {lhs, rhs});
  return lir::Value::MakeTemp(result);
}

}  // namespace lyra::lowering
