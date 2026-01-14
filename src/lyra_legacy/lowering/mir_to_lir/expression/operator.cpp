#include <cassert>
#include <utility>

#include "lyra/common/constant.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/expression/expression.hpp"
#include "lyra/lowering/mir_to_lir/expression/internal.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/operators.hpp"

namespace lyra::lowering::mir_to_lir {

using Type = common::Type;
using Constant = common::Constant;
using Operand = lir::Operand;
using TempRef = lir::TempRef;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

auto LowerConversionExpression(
    const mir::ConversionExpression& conversion, LirBuilder& builder)
    -> lir::TempRef {
  auto input = LowerExpression(*conversion.value, builder);
  auto result = builder.AllocateTemp("cvt", conversion.target_type);
  auto instruction = Instruction::WithType(
      IK::kConversion, result, input, conversion.target_type);
  builder.AddInstruction(std::move(instruction));
  return result;
}

auto LowerUnaryExpression(
    const mir::UnaryExpression& expression, lir::TempRef operand,
    LirBuilder& builder) -> lir::TempRef {
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

  auto result = builder.AllocateTemp("una", expression.type);
  auto instruction = Instruction::Basic(kind, result, operand);
  builder.AddInstruction(std::move(instruction));
  return result;
}

auto LowerBinaryExpression(
    const mir::BinaryExpression& expression, TempRef lhs, TempRef rhs,
    LirBuilder& builder) -> TempRef {
  using Operator = mir::BinaryOperator;

  IK kind{};

  const bool is_lhs_string = expression.left->type == Type::String();
  const bool is_rhs_string = expression.right->type == Type::String();
  const bool is_string = is_lhs_string || is_rhs_string;

  // String operand restrictions and unsupported operators are validated in
  // AST->MIR. For strings, only equality/inequality are allowed.
  if (is_string) {
    assert(
        expression.op == Operator::kEquality ||
        expression.op == Operator::kInequality);
  }

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
      kind = IK::kBinaryPower;
      break;
    case Operator::kBitwiseAnd:
      kind = IK::kBinaryBitwiseAnd;
      break;
    case Operator::kBitwiseOr:
      kind = IK::kBinaryBitwiseOr;
      break;
    case Operator::kBitwiseXor:
      kind = IK::kBinaryBitwiseXor;
      break;
    case Operator::kBitwiseXnor:
      kind = IK::kBinaryBitwiseXnor;
      break;
    case Operator::kLogicalAnd:
      kind = IK::kBinaryLogicalAnd;
      break;
    case Operator::kLogicalOr:
      kind = IK::kBinaryLogicalOr;
      break;
    case Operator::kLogicalShiftLeft:
      kind = IK::kBinaryLogicalShiftLeft;
      break;
    case Operator::kLogicalShiftRight:
      kind = IK::kBinaryLogicalShiftRight;
      break;
    case Operator::kArithmeticShiftLeft:
      kind = IK::kBinaryArithmeticShiftLeft;
      break;
    case Operator::kArithmeticShiftRight:
      kind = IK::kBinaryArithmeticShiftRight;
      break;

    // Unsupported operators - rejected in AST->MIR
    case Operator::kLogicalImplication:
    case Operator::kLogicalEquivalence:
    case Operator::kCaseEquality:
    case Operator::kCaseInequality:
    case Operator::kWildcardEquality:
    case Operator::kWildcardInequality:
      assert(false && "unsupported operator should be rejected in AST->MIR");
  }

  auto result = builder.AllocateTemp("bin", expression.type);
  auto instruction = Instruction::Basic(
      kind, result, {Operand::Temp(lhs), Operand::Temp(rhs)});
  builder.AddInstruction(std::move(instruction));
  return result;
}

auto LowerIncrementDecrementExpression(
    const mir::UnaryExpression& expression, LirBuilder& builder) -> TempRef {
  using lir::Instruction;
  using lir::InstructionKind;
  using Operator = mir::UnaryOperator;

  // Operand must be a variable reference - validated in AST->MIR
  assert(expression.operand->kind == mir::Expression::Kind::kIdentifier);

  const auto& identifier =
      mir::As<mir::IdentifierExpression>(*expression.operand);

  // Resolve variable pointer (used for both load and store)
  const auto* pointee = builder.GetContext()->InternType(identifier.type);
  auto ptr = builder.AllocateTemp("ptr", common::Type::Pointer(pointee));
  builder.AddInstruction(Instruction::ResolveVar(ptr, identifier.symbol));

  // Load the current value
  auto load_temp = builder.AllocateTemp("load", identifier.type);
  builder.AddInstruction(Instruction::Load(load_temp, ptr));

  // Create a literal instruction for the constant 1
  auto const_one_temp = builder.AllocateTemp("const", identifier.type);
  auto const_one = builder.InternConstant(Constant::Int(1));
  builder.AddInstruction(Instruction::Constant(const_one_temp, const_one));

  // Compute the new value (add/subtract 1)
  auto operation_temp = builder.AllocateTemp("op", identifier.type);
  IK operation_kind{};

  if (expression.op == Operator::kPreincrement ||
      expression.op == Operator::kPostincrement) {
    operation_kind = IK::kBinaryAdd;
  } else {
    operation_kind = IK::kBinarySubtract;
  }

  auto operation_instruction = Instruction::Basic(
      operation_kind, operation_temp,
      {Operand::Temp(load_temp), Operand::Temp(const_one_temp)});
  builder.AddInstruction(std::move(operation_instruction));

  // Store the updated value (reuse ptr from above)
  builder.AddInstruction(Instruction::Store(ptr, operation_temp));

  // Return either the old or new value based on pre/post operation
  if (expression.op == Operator::kPreincrement ||
      expression.op == Operator::kPredecrement) {
    // Pre-operations return the new value
    return operation_temp;
  }
  // Post-operations return the original value
  return load_temp;
}

}  // namespace lyra::lowering::mir_to_lir
