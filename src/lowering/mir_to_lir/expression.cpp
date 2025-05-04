#include "lowering/mir_to_lir/expression.hpp"

#include <cassert>

#include "lowering/mir_to_lir/lir_builder.hpp"
#include "mir/expression.hpp"

namespace lyra::lowering {

using Type = common::Type;
using Literal = common::Literal;

auto LowerExpression(const mir::Expression& expression, LirBuilder& builder)
    -> lir::Operand {
  using lir::Instruction;
  using lir::InstructionKind;

  switch (expression.kind) {
    case mir::Expression::Kind::kLiteral: {
      const auto& literal_expression =
          mir::As<mir::LiteralExpression>(expression);
      std::string result_name = builder.MakeTemp("lit");
      Instruction instruction = Instruction::Basic(
          InstructionKind::kLiteral, result_name,
          {lir::Operand::Literal(literal_expression.literal)});
      builder.AddInstruction(std::move(instruction));
      return lir::Operand::Temp(result_name);
    }

    case mir::Expression::Kind::kIdentifier: {
      const auto& identifier = mir::As<mir::IdentifierExpression>(expression);
      assert(!identifier.name.empty());
      std::string result_name = builder.MakeTemp("load");
      Instruction instruction = Instruction::Basic(
          InstructionKind::kLoadVariable, result_name,
          {lir::Operand::Variable(identifier.name)});
      builder.AddInstruction(std::move(instruction));
      return lir::Operand::Temp(result_name);
    }

    case mir::Expression::Kind::kAssignment: {
      const auto& assignment = mir::As<mir::AssignmentExpression>(expression);
      assert(!assignment.target.empty());
      assert(assignment.value);
      lir::Operand value = LowerExpression(*assignment.value, builder);
      Instruction instruction = Instruction::Basic(
          InstructionKind::kStoreVariable, "",
          {lir::Operand::Variable(assignment.target), value});
      builder.AddInstruction(std::move(instruction));
      return value;
    }

    case mir::Expression::Kind::kConversion: {
      const auto& conversion = mir::As<mir::ConversionExpression>(expression);
      lir::Operand input = LowerExpression(*conversion.value, builder);
      std::string result_name = builder.MakeTemp("cvt");
      Instruction instruction = Instruction::WithType(
          InstructionKind::kConversion, result_name, {input},
          conversion.target_type);
      builder.AddInstruction(std::move(instruction));
      return lir::Operand::Temp(result_name);
    }

    case mir::Expression::Kind::kSystemCall: {
      const auto& system_call = mir::As<mir::SystemCallExpression>(expression);

      if (system_call.name != "$finish") {
        throw std::runtime_error(
            fmt::format("Unsupported system call: {}", system_call.name));
      }

      std::vector<lir::Operand> arguments;
      for (const auto& argument : system_call.arguments) {
        if (argument) {
          arguments.push_back(LowerExpression(*argument, builder));
        }
      }

      // Add default argument for `$finish` if empty
      if (arguments.empty()) {
        arguments.push_back(lir::Operand::Literal(Literal::Int(1)));
      }

      Instruction instruction =
          Instruction::SystemCall(system_call.name, std::move(arguments));
      builder.AddInstruction(std::move(instruction));

      std::string result_name = builder.MakeTemp("sys");
      return lir::Operand::Temp(result_name);
    }

    case mir::Expression::Kind::kBinary: {
      const auto& binary = mir::As<mir::BinaryExpression>(expression);
      assert(binary.left && binary.right);
      lir::Operand lhs = LowerExpression(*binary.left, builder);
      lir::Operand rhs = LowerExpression(*binary.right, builder);
      return LowerBinaryExpression(binary, lhs, rhs, builder);
    }

    default:
      assert(false && "Unsupported MIR expression kind in LowerExpression");
      return lir::Operand::Temp("invalid");
  }
}

auto LowerBinaryExpression(
    const mir::BinaryExpression& expression, lir::Operand lhs, lir::Operand rhs,
    LirBuilder& builder) -> lir::Operand {
  using lir::InstructionKind;
  using mir::Operator;

  InstructionKind kind = InstructionKind::kBinaryAdd;

  const bool is_lhs_string = lhs.literal.type == Type::String();
  const bool is_rhs_string = rhs.literal.type == Type::String();
  const bool is_string = is_lhs_string || is_rhs_string;

  if (is_string) {
    switch (expression.op) {
      case Operator::kEquality:
        kind = InstructionKind::kBinaryEqual;
        break;
      case Operator::kInequality:
        kind = InstructionKind::kBinaryNotEqual;
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
        kind = InstructionKind::kBinaryEqual;
        break;
      case Operator::kInequality:
        kind = InstructionKind::kBinaryNotEqual;
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

  std::string result_name = builder.MakeTemp("bin");
  lir::Instruction instruction =
      lir::Instruction::Basic(kind, result_name, {lhs, rhs});
  builder.AddInstruction(std::move(instruction));
  return lir::Operand::Temp(result_name);
}

}  // namespace lyra::lowering
