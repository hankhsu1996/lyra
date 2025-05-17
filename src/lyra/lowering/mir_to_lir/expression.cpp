#include "lyra/lowering/mir_to_lir/expression.hpp"

#include <cassert>

#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::lowering {

using Type = common::Type;
using Literal = common::Literal;
using Operand = lir::Operand;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

auto LowerExpression(const mir::Expression& expression, LirBuilder& builder)
    -> Operand {
  switch (expression.kind) {
    case mir::Expression::Kind::kLiteral: {
      const auto& literal_expression =
          mir::As<mir::LiteralExpression>(expression);
      auto result = builder.AllocateTemp("lit");
      auto literal = builder.InternLiteral(literal_expression.literal);
      Instruction instruction =
          Instruction::Basic(IK::kLiteral, result, Operand::Literal(literal));
      builder.AddInstruction(std::move(instruction));
      return lir::Operand::Temp(result);
    }

    case mir::Expression::Kind::kIdentifier: {
      const auto& identifier = mir::As<mir::IdentifierExpression>(expression);
      auto result = builder.AllocateTemp("load");
      Instruction instruction = Instruction::Basic(
          IK::kLoadVariable, result, Operand::Variable(identifier.symbol));
      builder.AddInstruction(std::move(instruction));
      return lir::Operand::Temp(result);
    }

    case mir::Expression::Kind::kUnary: {
      const auto& unary = mir::As<mir::UnaryExpression>(expression);
      assert(unary.operand);
      auto operand = LowerExpression(*unary.operand, builder);
      return LowerUnaryExpression(unary, operand, builder);
    }

    case mir::Expression::Kind::kBinary: {
      const auto& binary = mir::As<mir::BinaryExpression>(expression);
      assert(binary.left && binary.right);
      auto lhs = LowerExpression(*binary.left, builder);
      auto rhs = LowerExpression(*binary.right, builder);
      return LowerBinaryExpression(binary, lhs, rhs, builder);
    }

    case mir::Expression::Kind::kTernary: {
      const auto& ternary = mir::As<mir::TernaryExpression>(expression);
      assert(ternary.condition);
      assert(ternary.true_expression);
      assert(ternary.false_expression);
      return LowerTernaryExpression(ternary, builder);
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
      auto input = LowerExpression(*conversion.value, builder);
      auto result = builder.AllocateTemp("cvt");
      auto instruction = Instruction::WithType(
          IK::kConversion, result, input, conversion.target_type);
      builder.AddInstruction(std::move(instruction));
      return Operand::Temp(result);
    }

    case mir::Expression::Kind::kSystemCall: {
      const auto& system_call = mir::As<mir::SystemCallExpression>(expression);

      if (system_call.name != "$finish") {
        throw std::runtime_error(
            fmt::format("Unsupported system call: {}", system_call.name));
      }

      std::vector<Operand> arguments;
      for (const auto& argument : system_call.arguments) {
        if (argument) {
          arguments.push_back(LowerExpression(*argument, builder));
        }
      }

      // Add default argument for `$finish` if empty
      if (arguments.empty()) {
        auto literal = builder.InternLiteral(Literal::Int(1));
        arguments.push_back(Operand::Literal(literal));
      }

      Instruction instruction =
          Instruction::SystemCall(system_call.name, std::move(arguments));
      builder.AddInstruction(std::move(instruction));

      auto result = builder.AllocateTemp("sys");
      return Operand::Temp(result);
    }

    default:
      assert(false && "Unsupported MIR expression kind in LowerExpression");
      return Operand::Temp(builder.AllocateTemp("invalid"));
  }
}

auto LowerUnaryExpression(
    const mir::UnaryExpression& expression, Operand operand,
    LirBuilder& builder) -> Operand {
  using Operator = mir::UnaryOperator;

  // Handle unary operations
  IK kind{};

  switch (expression.op) {
    case Operator::kPlus:
      // Unary plus is a no-op, just return the operand
      return operand;

    case Operator::kMinus:
      kind = IK::kUnaryMinus;
      break;

    case Operator::kLogicalNot:
      kind = IK::kUnaryLogicalNot;
      break;

    case Operator::kBitwiseNot:
      kind = IK::kUnaryBitwiseNot;
      break;

    // Reduction operations
    case Operator::kReductionAnd:
      kind = IK::kReductionAnd;
      break;

    case Operator::kReductionNand:
      kind = IK::kReductionNand;
      break;

    case Operator::kReductionOr:
      kind = IK::kReductionOr;
      break;

    case Operator::kReductionNor:
      kind = IK::kReductionNor;
      break;

    case Operator::kReductionXor:
      kind = IK::kReductionXor;
      break;

    case Operator::kReductionXnor:
      kind = IK::kReductionXnor;
      break;

    // Handle increment/decrement with helper function
    case Operator::kPreincrement:
    case Operator::kPostincrement:
    case Operator::kPredecrement:
    case Operator::kPostdecrement:
      return LowerIncrementDecrementExpression(expression, builder);
  }

  auto result = builder.AllocateTemp("una");
  Instruction instruction = Instruction::Basic(kind, result, operand);
  builder.AddInstruction(std::move(instruction));
  return Operand::Temp(result);
}

auto LowerBinaryExpression(
    const mir::BinaryExpression& expression, Operand lhs, Operand rhs,
    LirBuilder& builder) -> Operand {
  using Operator = mir::BinaryOperator;

  IK kind{};

  const bool is_lhs_string =
      lhs.IsLiteral() &&
      std::get<lir::LiteralRef>(lhs.value)->type == Type::String();
  const bool is_rhs_string =
      rhs.IsLiteral() &&
      std::get<lir::LiteralRef>(rhs.value)->type == Type::String();
  const bool is_string = is_lhs_string || is_rhs_string;

  if (is_string) {
    switch (expression.op) {
      case Operator::kEquality:
        kind = IK::kBinaryEqual;
        break;
      case Operator::kInequality:
        kind = IK::kBinaryNotEqual;
        break;
      default:
        throw std::runtime_error(fmt::format(
            "Operator {} is not supported for string operands", expression.op));
    }
  } else {
    switch (expression.op) {
      case Operator::kAddition:
        kind = IK::kBinaryAdd;
        break;
      case Operator::kSubtraction:
        kind = IK::kBinarySubtract;
        break;
      case Operator::kMultiplication:
        kind = IK::kBinaryMultiply;
        break;
      case Operator::kDivision:
        kind = IK::kBinaryDivide;
        break;
      case Operator::kModulo:
        kind = IK::kBinaryModulo;
        break;
      case Operator::kEquality:
        kind = IK::kBinaryEqual;
        break;
      case Operator::kInequality:
        kind = IK::kBinaryNotEqual;
        break;
      case Operator::kLessThan:
        kind = IK::kBinaryLessThan;
        break;
      case Operator::kLessThanEqual:
        kind = IK::kBinaryLessThanEqual;
        break;
      case Operator::kGreaterThan:
        kind = IK::kBinaryGreaterThan;
        break;
      case Operator::kGreaterThanEqual:
        kind = IK::kBinaryGreaterThanEqual;
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

  auto result = builder.AllocateTemp("bin");
  Instruction instruction = Instruction::Basic(kind, result, {lhs, rhs});
  builder.AddInstruction(std::move(instruction));
  return Operand::Temp(result);
}

auto LowerTernaryExpression(
    const mir::TernaryExpression& expression, LirBuilder& builder) -> Operand {
  auto condition = LowerExpression(*expression.condition, builder);

  // Generate unique labels for the various blocks
  auto true_label = builder.MakeLabel("ternary.true");
  auto false_label = builder.MakeLabel("ternary.false");
  auto end_label = builder.MakeLabel("ternary.end");

  // Create a temporary variable to hold the result
  auto result = builder.AllocateTemp("ternary");

  // Branch based on condition
  auto branch = Instruction::Branch(condition, true_label, false_label);
  builder.AddInstruction(std::move(branch));

  // True branch
  builder.StartBlock(true_label);
  auto true_value = LowerExpression(*expression.true_expression, builder);
  // Copy the true value to the result temp
  auto copy_true = Instruction::Basic(IK::kMove, result, {true_value});
  builder.AddInstruction(std::move(copy_true));
  auto jump_to_end = Instruction::Jump(end_label);
  builder.AddInstruction(std::move(jump_to_end));
  builder.EndBlock();

  // False branch
  builder.StartBlock(false_label);
  auto false_value = LowerExpression(*expression.false_expression, builder);
  // Copy the false value to the result temp
  auto copy_false = Instruction::Basic(IK::kMove, result, {false_value});
  builder.AddInstruction(std::move(copy_false));
  auto jump_to_end_from_false = Instruction::Jump(end_label);
  builder.AddInstruction(std::move(jump_to_end_from_false));
  builder.EndBlock();

  // End block - control rejoins here
  builder.StartBlock(end_label);

  // Return the result temporary
  return Operand::Temp(result);
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
  auto load_temp = builder.AllocateTemp("load");
  Instruction load_instruction = Instruction::Basic(
      IK::kLoadVariable, load_temp, Operand::Variable(identifier.symbol));
  builder.AddInstruction(std::move(load_instruction));

  // Create a literal instruction for the constant 1
  auto const_one_temp = builder.AllocateTemp("const");
  auto const_one = builder.InternLiteral(Literal::Int(1));
  Instruction const_instruction = Instruction::Basic(
      IK::kLiteral, const_one_temp, Operand::Literal(const_one));
  builder.AddInstruction(std::move(const_instruction));

  // Compute the new value (add/subtract 1)
  auto operation_temp = builder.AllocateTemp("op");
  IK operation_kind{};

  if (expression.op == Operator::kPreincrement ||
      expression.op == Operator::kPostincrement) {
    operation_kind = IK::kBinaryAdd;
  } else {
    operation_kind = IK::kBinarySubtract;
  }

  Instruction operation_instruction = Instruction::Basic(
      operation_kind, operation_temp,
      {Operand::Temp(load_temp), Operand::Temp(const_one_temp)});
  builder.AddInstruction(std::move(operation_instruction));

  // Store the updated value
  Instruction store_instruction = Instruction::StoreVariable(
      Operand::Variable(identifier.symbol), Operand::Temp(operation_temp),
      false);
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
