#include "lyra/lowering/mir_to_lir/expression.hpp"

#include <cassert>

#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/expression.hpp"

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
      std::string result_name = builder.MakeTemp("load");
      Instruction instruction = Instruction::Basic(
          InstructionKind::kLoadVariable, result_name,
          {lir::Operand::Variable(identifier.symbol)});
      builder.AddInstruction(std::move(instruction));
      return lir::Operand::Temp(result_name);
    }

    case mir::Expression::Kind::kUnary: {
      const auto& unary = mir::As<mir::UnaryExpression>(expression);
      assert(unary.operand);
      lir::Operand operand = LowerExpression(*unary.operand, builder);
      return LowerUnaryExpression(unary, operand, builder);
    }

    case mir::Expression::Kind::kBinary: {
      const auto& binary = mir::As<mir::BinaryExpression>(expression);
      assert(binary.left && binary.right);
      lir::Operand lhs = LowerExpression(*binary.left, builder);
      lir::Operand rhs = LowerExpression(*binary.right, builder);
      return LowerBinaryExpression(binary, lhs, rhs, builder);
    }

    case mir::Expression::Kind::kAssignment: {
      const auto& assignment = mir::As<mir::AssignmentExpression>(expression);
      assert(assignment.target);
      assert(assignment.value);
      auto value = LowerExpression(*assignment.value, builder);
      auto variable = lir::Operand::Variable(assignment.target);

      auto instruction = Instruction::StoreVariable(
          variable, value, assignment.is_non_blocking);
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

    default:
      assert(false && "Unsupported MIR expression kind in LowerExpression");
      return lir::Operand::Temp("invalid");
  }
}

auto LowerUnaryExpression(
    const mir::UnaryExpression& expression, lir::Operand operand,
    LirBuilder& builder) -> lir::Operand {
  using lir::Instruction;
  using lir::InstructionKind;
  using Operator = mir::UnaryOperator;

  // Handle unary operations
  InstructionKind kind{};

  switch (expression.op) {
    case Operator::kPlus:
      // Unary plus is a no-op, just return the operand
      return operand;

    case Operator::kMinus:
      kind = InstructionKind::kUnaryMinus;
      break;

    case Operator::kLogicalNot:
      kind = InstructionKind::kUnaryLogicalNot;
      break;

    case Operator::kBitwiseNot:
      kind = InstructionKind::kUnaryBitwiseNot;
      break;

    // Reduction operations
    case Operator::kReductionAnd:
      kind = InstructionKind::kReductionAnd;
      break;

    case Operator::kReductionNand:
      kind = InstructionKind::kReductionNand;
      break;

    case Operator::kReductionOr:
      kind = InstructionKind::kReductionOr;
      break;

    case Operator::kReductionNor:
      kind = InstructionKind::kReductionNor;
      break;

    case Operator::kReductionXor:
      kind = InstructionKind::kReductionXor;
      break;

    case Operator::kReductionXnor:
      kind = InstructionKind::kReductionXnor;
      break;

    // Handle increment/decrement with helper function
    case Operator::kPreincrement:
    case Operator::kPostincrement:
    case Operator::kPredecrement:
    case Operator::kPostdecrement:
      return LowerIncrementDecrementExpression(expression, builder);
  }

  std::string result_name = builder.MakeTemp("una");
  lir::Instruction instruction =
      lir::Instruction::Basic(kind, result_name, {operand});
  builder.AddInstruction(std::move(instruction));
  return lir::Operand::Temp(result_name);
}

auto LowerBinaryExpression(
    const mir::BinaryExpression& expression, lir::Operand lhs, lir::Operand rhs,
    LirBuilder& builder) -> lir::Operand {
  using lir::InstructionKind;
  using Operator = mir::BinaryOperator;

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

auto LowerIncrementDecrementExpression(
    const mir::UnaryExpression& expression, LirBuilder& builder)
    -> lir::Operand {
  using lir::Instruction;
  using lir::InstructionKind;
  using Operator = mir::UnaryOperator;

  // Check if the operand is a variable reference (identifier)
  if (expression.operand->kind != mir::Expression::Kind::kIdentifier) {
    throw std::runtime_error(
        "Increment/decrement operations require a variable operand");
  }

  const auto& identifier =
      mir::As<mir::IdentifierExpression>(*expression.operand);

  // Load the current value
  std::string load_temp = builder.MakeTemp("load");
  Instruction load_instruction = Instruction::Basic(
      InstructionKind::kLoadVariable, load_temp,
      {lir::Operand::Variable(identifier.symbol)});
  builder.AddInstruction(std::move(load_instruction));

  // Create a literal instruction for the constant 1
  std::string const_one_temp = builder.MakeTemp("const");
  Instruction const_instruction = Instruction::Basic(
      InstructionKind::kLiteral, const_one_temp,
      {lir::Operand::Literal(common::Literal::Int(1))});
  builder.AddInstruction(std::move(const_instruction));

  // Compute the new value (add/subtract 1)
  std::string operation_temp = builder.MakeTemp("op");
  InstructionKind operation_kind{};

  if (expression.op == Operator::kPreincrement ||
      expression.op == Operator::kPostincrement) {
    operation_kind = InstructionKind::kBinaryAdd;
  } else {
    operation_kind = InstructionKind::kBinarySubtract;
  }

  Instruction operation_instruction = Instruction::Basic(
      operation_kind, operation_temp,
      {lir::Operand::Temp(load_temp), lir::Operand::Temp(const_one_temp)});
  builder.AddInstruction(std::move(operation_instruction));

  // Store the updated value
  Instruction store_instruction = Instruction::Basic(
      InstructionKind::kStoreVariable, "",
      {lir::Operand::Variable(identifier.symbol),
       lir::Operand::Temp(operation_temp)});
  builder.AddInstruction(std::move(store_instruction));

  // Return either the old or new value based on pre/post operation
  if (expression.op == Operator::kPreincrement ||
      expression.op == Operator::kPredecrement) {
    // Pre-operations return the new value
    return lir::Operand::Temp(operation_temp);
  }
  // Post-operations return the original value
  return lir::Operand::Temp(load_temp);
}

}  // namespace lyra::lowering
