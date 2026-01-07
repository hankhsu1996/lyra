#include "lyra/lowering/mir_to_lir/expression.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/common/literal.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/helpers.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/operators.hpp"

namespace lyra::lowering::mir_to_lir {

using Type = common::Type;
using Literal = common::Literal;
using Operand = lir::Operand;
using TempRef = lir::TempRef;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

auto LowerExpression(const mir::Expression& expression, LirBuilder& builder)
    -> lir::TempRef {
  switch (expression.kind) {
    case mir::Expression::Kind::kLiteral: {
      const auto& literal_expression =
          mir::As<mir::LiteralExpression>(expression);
      auto result =
          builder.AllocateTemp("lit", literal_expression.literal.type);
      auto literal = builder.InternLiteral(literal_expression.literal);
      auto instruction = Instruction::Basic(IK::kLiteral, result, literal);
      builder.AddInstruction(std::move(instruction));
      return result;
    }

    case mir::Expression::Kind::kIdentifier: {
      const auto& identifier = mir::As<mir::IdentifierExpression>(expression);
      auto result = builder.AllocateTemp("load", identifier.type);
      auto instruction =
          Instruction::Basic(IK::kLoadVariable, result, identifier.symbol);
      builder.AddInstruction(std::move(instruction));
      return result;
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
      assert(assignment.value);
      auto value = LowerExpression(*assignment.value, builder);

      if (assignment.target.IsHierarchical()) {
        // Hierarchical assignment not yet supported in interpreter.
        // Will be implemented in Phase 16.
        throw std::runtime_error(
            "Hierarchical assignment is not supported by the interpreter");
      }

      if (assignment.target.IsElementSelect()) {
        auto index = LowerExpression(*assignment.target.element_index, builder);

        if (assignment.target.IsPacked()) {
          // Packed element assignment: vec[index] = value
          size_t element_width = assignment.target.base_type->GetElementWidth();
          int32_t lower = assignment.target.base_type->GetElementLower();
          auto adjusted_index = AdjustForNonZeroLower(index, lower, builder);

          auto instruction = Instruction::StorePackedElement(
              assignment.target.symbol, adjusted_index, value, element_width);
          builder.AddInstruction(std::move(instruction));
        } else {
          // Unpacked array element assignment: array[index] = value
          auto instruction = Instruction::StoreUnpackedElement(
              assignment.target.symbol, index, value);
          builder.AddInstruction(std::move(instruction));
        }
        return value;
      }

      // Simple variable assignment
      auto instruction = Instruction::StoreVariable(
          assignment.target.symbol, value, assignment.is_non_blocking);
      builder.AddInstruction(std::move(instruction));
      return value;
    }

    case mir::Expression::Kind::kElementSelect: {
      const auto& select = mir::As<mir::ElementSelectExpression>(expression);
      assert(select.value);
      assert(select.selector);

      auto index = LowerExpression(*select.selector, builder);
      auto result = builder.AllocateTemp("elem", expression.type);

      // Check if this is packed type (value) or unpacked array (variable)
      if (select.value->type.kind == common::Type::Kind::kIntegral) {
        // Packed vector: lower the value, then select element/bit
        auto value = LowerExpression(*select.value, builder);
        int32_t lower = select.value->type.GetElementLower();
        auto adjusted_index = AdjustForNonZeroLower(index, lower, builder);

        auto instruction = Instruction::LoadPackedElement(
            result, value, adjusted_index, expression.type);
        builder.AddInstruction(std::move(instruction));
        return result;
      }

      // Unpacked array: must be a direct variable reference
      if (select.value->kind != mir::Expression::Kind::kIdentifier) {
        assert(false && "only direct array variable access is supported");
      }

      const auto& array_id = mir::As<mir::IdentifierExpression>(*select.value);
      auto instruction = Instruction::LoadUnpackedElement(
          result, array_id.symbol, index, expression.type);
      builder.AddInstruction(std::move(instruction));
      return result;
    }

    case mir::Expression::Kind::kConversion: {
      const auto& conversion = mir::As<mir::ConversionExpression>(expression);
      auto input = LowerExpression(*conversion.value, builder);
      auto result = builder.AllocateTemp("cvt", conversion.target_type);
      auto instruction = Instruction::WithType(
          IK::kConversion, result, input, conversion.target_type);
      builder.AddInstruction(std::move(instruction));
      return result;
    }

    case mir::Expression::Kind::kSystemCall: {
      const auto& system_call = mir::As<mir::SystemCallExpression>(expression);

      // Supported system calls are validated in AST→MIR
      // System tasks: $finish, $stop, $exit, $display, $timeformat,
      //   $printtimescale (no return)
      // System functions: $time, $stime, $realtime, $timeunit, $timeprecision
      //   $timeunit_root, $timeprecision_root (transformed from $root arg)
      assert(
          system_call.name == "$finish" || system_call.name == "$stop" ||
          system_call.name == "$exit" || system_call.name == "$display" ||
          system_call.name == "$timeformat" ||
          system_call.name == "$printtimescale" ||
          system_call.name == "$printtimescale_root" ||
          system_call.name == "$time" || system_call.name == "$stime" ||
          system_call.name == "$realtime" || system_call.name == "$timeunit" ||
          system_call.name == "$timeprecision" ||
          system_call.name == "$timeunit_root" ||
          system_call.name == "$timeprecision_root");

      std::vector<TempRef> arguments;
      for (const auto& argument : system_call.arguments) {
        if (argument) {
          arguments.push_back(LowerExpression(*argument, builder));
        }
      }

      // Add default argument (1) for $finish and $stop if not provided
      // $exit takes no arguments per LRM
      if ((system_call.name == "$finish" || system_call.name == "$stop") &&
          arguments.empty()) {
        auto temp = builder.AllocateTemp("sys", system_call.type);
        auto const_one = builder.InternLiteral(Literal::Int(1));
        auto instruction = Instruction::Basic(IK::kLiteral, temp, const_one);
        builder.AddInstruction(std::move(instruction));
        arguments.push_back(temp);
      }

      // System functions return a value, system tasks do not
      bool is_function =
          system_call.name == "$time" || system_call.name == "$stime" ||
          system_call.name == "$realtime" || system_call.name == "$timeunit" ||
          system_call.name == "$timeprecision" ||
          system_call.name == "$timeunit_root" ||
          system_call.name == "$timeprecision_root";

      auto result = builder.AllocateTemp("sys", system_call.type);

      if (is_function) {
        // Pass result temp to store the return value
        auto instruction = Instruction::SystemCall(
            system_call.name, std::move(arguments), result, system_call.type);
        builder.AddInstruction(std::move(instruction));
      } else {
        // No result for system tasks
        auto instruction =
            Instruction::SystemCall(system_call.name, std::move(arguments));
        builder.AddInstruction(std::move(instruction));
      }

      return result;
    }

<<<<<<< HEAD
    case mir::Expression::Kind::kRangeSelect: {
      const auto& range = mir::As<mir::RangeSelectExpression>(expression);
      assert(range.value);

      auto value = LowerExpression(*range.value, builder);

      // Compute LSB: for [7:4], LSB is 4
      int32_t lsb = std::min(range.left, range.right);

      // Adjust for non-zero-based ranges (e.g., bit [63:32])
      if (range.value->type.kind == common::Type::Kind::kIntegral) {
        const auto& two_state =
            std::get<common::IntegralData>(range.value->type.data);
        lsb -= two_state.element_lower;
      }

      // Create a literal for the LSB shift amount
      auto lsb_temp = builder.AllocateTemp("lsb", Type::Int());
      auto lsb_literal = builder.InternLiteral(Literal::Int(lsb));
      auto lsb_instruction =
          Instruction::Basic(IK::kLiteral, lsb_temp, lsb_literal);
      builder.AddInstruction(std::move(lsb_instruction));

      auto result = builder.AllocateTemp("slice", expression.type);
      auto instruction = Instruction::LoadPackedSlice(
          result, value, lsb_temp, expression.type);
      builder.AddInstruction(std::move(instruction));
      return result;
    }

    case mir::Expression::Kind::kIndexedRangeSelect: {
      const auto& indexed =
          mir::As<mir::IndexedRangeSelectExpression>(expression);
      assert(indexed.value);
      assert(indexed.start);

      auto value = LowerExpression(*indexed.value, builder);
      auto start = LowerExpression(*indexed.start, builder);

      TempRef lsb_temp;
      if (indexed.is_ascending) {
        // a[i+:4]: lsb = i (start index is the LSB)
        lsb_temp = start;
      } else {
        // a[i-:4]: lsb = i - width + 1
        auto offset_temp = builder.AllocateTemp("offset", Type::Int());
        auto offset_literal =
            builder.InternLiteral(Literal::Int(indexed.width - 1));
        builder.AddInstruction(
            Instruction::Basic(IK::kLiteral, offset_temp, offset_literal));

        lsb_temp = builder.AllocateTemp("lsb", Type::Int());
        builder.AddInstruction(
            Instruction::Basic(
                IK::kBinarySubtract, lsb_temp,
                {Operand::Temp(start), Operand::Temp(offset_temp)}));
      }

      // Adjust for non-zero-based ranges if needed
      int32_t lower = indexed.value->type.GetElementLower();
      lsb_temp = AdjustForNonZeroLower(lsb_temp, lower, builder);

      auto result = builder.AllocateTemp("slice", expression.type);
      builder.AddInstruction(
          Instruction::LoadPackedSlice(
              result, value, lsb_temp, expression.type));
      return result;
    }

    case mir::Expression::Kind::kPortDriver: {
      // PortDriverExpression is only supported by C++ codegen, not the
      // interpreter.
      throw std::runtime_error(
          "PortDriverExpression is not supported by the interpreter");
    }

    case mir::Expression::Kind::kHierarchicalReference: {
      // HierarchicalReferenceExpression is not yet supported by the
      // interpreter. Will be implemented in Phase 16.
      throw std::runtime_error(
          "HierarchicalReferenceExpression is not supported by the "
          "interpreter");
    }
  }
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

  const bool is_lhs_string = lhs->type == Type::String();
  const bool is_rhs_string = rhs->type == Type::String();
  const bool is_string = is_lhs_string || is_rhs_string;

  // String operand restrictions and unsupported operators are validated in
  // AST→MIR. For strings, only equality/inequality are allowed.
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

    // Unsupported operators - rejected in AST→MIR
    case Operator::kLogicalImplication:
    case Operator::kLogicalEquivalence:
    case Operator::kCaseEquality:
    case Operator::kCaseInequality:
    case Operator::kWildcardEquality:
    case Operator::kWildcardInequality:
      assert(false && "unsupported operator should be rejected in AST→MIR");
  }

  auto result = builder.AllocateTemp("bin", expression.type);
  auto instruction = Instruction::Basic(
      kind, result, {Operand::Temp(lhs), Operand::Temp(rhs)});
  builder.AddInstruction(std::move(instruction));
  return result;
}

auto LowerTernaryExpression(
    const mir::TernaryExpression& expression, LirBuilder& builder) -> TempRef {
  auto condition = LowerExpression(*expression.condition, builder);

  // Generate unique labels for the various blocks
  auto true_label = builder.MakeLabel("ternary.true");
  auto false_label = builder.MakeLabel("ternary.false");
  auto end_label = builder.MakeLabel("ternary.end");

  // Create a temporary variable to hold the result
  auto result = builder.AllocateTemp("ternary", expression.type);

  // Branch based on condition
  auto branch = Instruction::Branch(condition, true_label, false_label);
  builder.AddInstruction(std::move(branch));

  // True branch
  builder.StartBlock(true_label);
  auto true_value = LowerExpression(*expression.true_expression, builder);

  // Copy the true value to the result temp
  auto copy_true = Instruction::Basic(IK::kMove, result, true_value);
  builder.AddInstruction(std::move(copy_true));

  auto jump_to_end = Instruction::Jump(end_label);
  builder.AddInstruction(std::move(jump_to_end));
  builder.EndBlock();

  // False branch
  builder.StartBlock(false_label);
  auto false_value = LowerExpression(*expression.false_expression, builder);

  // Copy the false value to the result temp
  auto copy_false = Instruction::Basic(IK::kMove, result, false_value);
  builder.AddInstruction(std::move(copy_false));

  auto jump_to_end_from_false = Instruction::Jump(end_label);
  builder.AddInstruction(std::move(jump_to_end_from_false));
  builder.EndBlock();

  // End block - control rejoins here
  builder.StartBlock(end_label);

  // Return the result temporary
  return result;
}

auto LowerIncrementDecrementExpression(
    const mir::UnaryExpression& expression, LirBuilder& builder) -> TempRef {
  using lir::Instruction;
  using lir::InstructionKind;
  using Operator = mir::UnaryOperator;

  // Operand must be a variable reference - validated in AST→MIR
  assert(expression.operand->kind == mir::Expression::Kind::kIdentifier);

  const auto& identifier =
      mir::As<mir::IdentifierExpression>(*expression.operand);

  // Load the current value
  auto load_temp = builder.AllocateTemp("load", identifier.type);
  auto load_instruction =
      Instruction::Basic(IK::kLoadVariable, load_temp, identifier.symbol);
  builder.AddInstruction(std::move(load_instruction));

  // Create a literal instruction for the constant 1
  auto const_one_temp = builder.AllocateTemp("const", identifier.type);
  auto const_one = builder.InternLiteral(Literal::Int(1));
  auto const_instruction =
      Instruction::Basic(IK::kLiteral, const_one_temp, const_one);
  builder.AddInstruction(std::move(const_instruction));

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

  // Store the updated value
  auto store_instruction =
      Instruction::StoreVariable(identifier.symbol, operation_temp, false);
  builder.AddInstruction(std::move(store_instruction));

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
