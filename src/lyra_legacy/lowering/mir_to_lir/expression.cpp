#include "lyra/lowering/mir_to_lir/expression/expression.hpp"

#include <cassert>
#include <utility>

#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/expression/internal.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/expression.hpp"

namespace lyra::lowering::mir_to_lir {

using Operand = lir::Operand;
using TempRef = lir::TempRef;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

auto LowerExpression(const mir::Expression& expression, LirBuilder& builder)
    -> lir::TempRef {
  switch (expression.kind) {
    case mir::Expression::Kind::kConstant:
      return LowerConstantExpression(
          mir::As<mir::ConstantExpression>(expression), builder);

    case mir::Expression::Kind::kIdentifier:
      return LowerIdentifierExpression(
          mir::As<mir::IdentifierExpression>(expression), builder);

    case mir::Expression::Kind::kEnumValue:
      return LowerEnumValueExpression(
          mir::As<mir::EnumValueExpression>(expression), builder);

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

    case mir::Expression::Kind::kAssignment:
      return LowerAssignmentExpression(
          mir::As<mir::AssignmentExpression>(expression), builder);

    case mir::Expression::Kind::kElementSelect:
      return LowerElementSelectExpression(
          mir::As<mir::ElementSelectExpression>(expression), builder);

    case mir::Expression::Kind::kConversion:
      return LowerConversionExpression(
          mir::As<mir::ConversionExpression>(expression), builder);

    case mir::Expression::Kind::kSystemCall:
      return LowerSystemCallExpression(
          mir::As<mir::SystemCallExpression>(expression), builder);

    case mir::Expression::Kind::kRangeSelect:
      return LowerRangeSelectExpression(
          mir::As<mir::RangeSelectExpression>(expression), builder);

    case mir::Expression::Kind::kIndexedRangeSelect:
      return LowerIndexedRangeSelectExpression(
          mir::As<mir::IndexedRangeSelectExpression>(expression), builder);

    case mir::Expression::Kind::kHierarchicalReference:
      return LowerHierarchicalReferenceExpression(
          mir::As<mir::HierarchicalReferenceExpression>(expression), builder);

    case mir::Expression::Kind::kConcatenation:
      return LowerConcatenationExpression(
          mir::As<mir::ConcatenationExpression>(expression), builder);

    case mir::Expression::Kind::kReplication:
      return LowerReplicationExpression(
          mir::As<mir::ReplicationExpression>(expression), builder);

    case mir::Expression::Kind::kFunctionCall:
      return LowerFunctionCallExpression(
          mir::As<mir::FunctionCallExpression>(expression), builder);

    case mir::Expression::Kind::kMemberAccess:
      return LowerMemberAccessExpression(
          mir::As<mir::MemberAccessExpression>(expression), builder);

    case mir::Expression::Kind::kNewArray:
      return LowerNewArrayExpression(
          mir::As<mir::NewArrayExpression>(expression), builder);

    case mir::Expression::Kind::kMethodCall:
      return LowerMethodCallExpression(
          mir::As<mir::MethodCallExpression>(expression), builder);

    case mir::Expression::Kind::kUnpackedStructLiteral:
      return LowerUnpackedStructLiteralExpression(
          mir::As<mir::UnpackedStructLiteralExpression>(expression), builder);

    case mir::Expression::Kind::kArrayLiteral:
      return LowerArrayLiteralExpression(
          mir::As<mir::ArrayLiteralExpression>(expression), builder);
  }
  // All expression kinds must be handled above - if we reach here, a new
  // expression kind was added without updating this function
  throw common::InternalError(
      "LowerExpression",
      "unhandled MIR expression kind in MIR-to-LIR lowering");
}

auto LowerTernaryExpression(
    const mir::TernaryExpression& expression, LirBuilder& builder) -> TempRef {
  auto condition = LowerExpression(*expression.condition, builder);

  // Use branches for proper short-circuit evaluation
  auto true_label = builder.MakeLabel("ternary.true");
  auto false_label = builder.MakeLabel("ternary.false");
  auto end_label = builder.MakeLabel("ternary.end");

  auto result = builder.AllocateTemp("ternary", expression.type);

  auto branch = Instruction::Branch(condition, true_label, false_label);
  builder.AddInstruction(std::move(branch));

  // True branch
  builder.StartBlock(true_label);
  auto true_value = LowerExpression(*expression.true_expression, builder);

  auto copy_true = Instruction::Basic(IK::kMove, result, true_value);
  builder.AddInstruction(std::move(copy_true));

  auto jump_to_end = Instruction::Jump(end_label);
  builder.AddInstruction(std::move(jump_to_end));
  builder.EndBlock();

  // False branch
  builder.StartBlock(false_label);
  auto false_value = LowerExpression(*expression.false_expression, builder);

  auto copy_false = Instruction::Basic(IK::kMove, result, false_value);
  builder.AddInstruction(std::move(copy_false));

  auto jump_to_end_from_false = Instruction::Jump(end_label);
  builder.AddInstruction(std::move(jump_to_end_from_false));
  builder.EndBlock();

  // End block - control rejoins here
  builder.StartBlock(end_label);

  return result;
}

}  // namespace lyra::lowering::mir_to_lir
