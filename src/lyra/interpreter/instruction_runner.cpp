#include "lyra/interpreter/instruction_runner.hpp"

#include <cassert>

#include <fmt/core.h>

#include "lyra/interpreter/builtin_ops.hpp"
#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::interpreter {

InstructionRunner::InstructionRunner(SimulationContext& context)
    : ctx_(context) {
}

// Execute a single instruction in the given context
auto InstructionRunner::RunInstruction(const lir::Instruction& instr)
    -> InstructionResult {
  auto& temp_table = ctx_.get().temp_table;
  auto& variable_table = ctx_.get().variable_table;

  auto get_temp = [&](const lir::Operand& operand) -> RuntimeValue {
    return temp_table.Read(operand.name);
  };

  auto get_variable = [&](const lir::Operand& operand) -> RuntimeValue {
    return variable_table.Read(operand.name);
  };

  auto eval_unary_op = [&](const lir::Operand& operand,
                           const std::function<RuntimeValue(RuntimeValue)>& op)
      -> InstructionResult {
    const auto result = op(get_temp(operand));
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  };

  auto eval_binary_op =
      [&](const lir::Operand& lhs, const lir::Operand& rhs,
          const std::function<RuntimeValue(RuntimeValue, RuntimeValue)>& op)
      -> InstructionResult {
    const auto result = op(get_temp(lhs), get_temp(rhs));
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  };

  switch (instr.kind) {
    // Memory operations
    case lir::InstructionKind::kLiteral: {
      assert(instr.operands.size() == 1);
      assert(instr.operands[0].IsLiteral());
      assert(instr.result.has_value());

      const auto& literal = instr.operands[0].literal;
      RuntimeValue value = RuntimeValue::FromLiteral(literal);
      temp_table.Write(instr.result.value(), value);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadVariable: {
      const auto& src_variable = get_variable(instr.operands[0]);
      temp_table.Write(instr.result.value(), src_variable);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreVariable: {
      const auto variable_name = instr.operands[0].name;
      const auto value = get_temp(instr.operands[1]);
      variable_table.Write(variable_name, value);
      return InstructionResult::Continue(variable_name);
    }

    case lir::InstructionKind::kStoreVariableNonBlocking: {
      const auto variable_name = instr.operands[0].name;
      const auto value = get_temp(instr.operands[1]);
      return InstructionResult::NbaAction(
          NbaAction{.variable = variable_name, .value = value});
    }

    // Unary operations
    case lir::InstructionKind::kUnaryPlus: {
      return eval_unary_op(instr.operands[0], UnaryPlus);
    }

    case lir::InstructionKind::kUnaryMinus: {
      return eval_unary_op(instr.operands[0], UnaryMinus);
    }

    case lir::InstructionKind::kUnaryLogicalNot: {
      return eval_unary_op(instr.operands[0], UnaryLogicalNot);
    }

    case lir::InstructionKind::kUnaryBitwiseNot: {
      return eval_unary_op(instr.operands[0], UnaryBitwiseNot);
    }

    // Reduction operations
    case lir::InstructionKind::kReductionAnd: {
      return eval_unary_op(instr.operands[0], ReductionAnd);
    }

    case lir::InstructionKind::kReductionNand: {
      return eval_unary_op(instr.operands[0], ReductionNand);
    }

    case lir::InstructionKind::kReductionOr: {
      return eval_unary_op(instr.operands[0], ReductionOr);
    }

    case lir::InstructionKind::kReductionNor: {
      return eval_unary_op(instr.operands[0], ReductionNor);
    }

    case lir::InstructionKind::kReductionXor: {
      return eval_unary_op(instr.operands[0], ReductionXor);
    }

    case lir::InstructionKind::kReductionXnor: {
      return eval_unary_op(instr.operands[0], ReductionXnor);
    }

    // Binary operations
    case lir::InstructionKind::kBinaryAdd: {
      return eval_binary_op(instr.operands[0], instr.operands[1], BinaryAdd);
    }

    case lir::InstructionKind::kBinarySubtract: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinarySubtract);
    }

    case lir::InstructionKind::kBinaryMultiply: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryMultiply);
    }

    case lir::InstructionKind::kBinaryDivide: {
      return eval_binary_op(instr.operands[0], instr.operands[1], BinaryDivide);
    }

    case lir::InstructionKind::kBinaryModulo: {
      return eval_binary_op(instr.operands[0], instr.operands[1], BinaryModulo);
    }

    case lir::InstructionKind::kBinaryEqual: {
      return eval_binary_op(instr.operands[0], instr.operands[1], BinaryEqual);
    }

    case lir::InstructionKind::kBinaryNotEqual: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryNotEqual);
    }

    case lir::InstructionKind::kBinaryLessThan: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryLessThan);
    }

    case lir::InstructionKind::kBinaryLessThanEqual: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryLessThanEqual);
    }

    case lir::InstructionKind::kBinaryGreaterThan: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryGreaterThan);
    }

    case lir::InstructionKind::kBinaryGreaterThanEqual: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryGreaterThanEqual);
    }

    // Type operations
    case lir::InstructionKind::kConversion: {
      // Read the source value from temp table
      const auto& src = get_temp(instr.operands[0]);
      const auto& target_type = instr.result_type.value();

      // Handle string to string as a no-op conversion
      if (src.type == common::Type::String() &&
          target_type == common::Type::String()) {
        temp_table.Write(instr.result.value(), src);
        return InstructionResult::Continue();
      }

      // Ensure both source and target types are two-state
      if (src.type.kind != common::Type::Kind::kTwoState ||
          target_type.kind != common::Type::Kind::kTwoState) {
        throw std::runtime_error(fmt::format(
            "Conversion only supports two-state types, got: {} -> {}", src.type,
            target_type));
      }

      auto two_state_data = std::get<common::TwoStateData>(target_type.data);

      // Reject bit width greater than 64
      if (two_state_data.bit_width > 64) {
        throw std::runtime_error(
            fmt::format("Unsupported target bit width > 64: {}", target_type));
      }

      // Extract source value as int64
      int64_t raw_value = src.AsInt64();

      // Apply sign/bitwidth conversion
      RuntimeValue result =
          two_state_data.is_signed
              ? RuntimeValue::TwoStateSigned(
                    raw_value, two_state_data.bit_width)
              : RuntimeValue::TwoStateUnsigned(
                    static_cast<uint64_t>(raw_value), two_state_data.bit_width);

      // Write result to destination temp
      temp_table.Write(instr.result.value(), result);
      return InstructionResult::Continue();
    }

    // Control flow
    case lir::InstructionKind::kComplete: {
      return InstructionResult::Complete();
    }

    case lir::InstructionKind::kWaitEvent: {
      return InstructionResult::WaitEvent(instr.wait_triggers);
    }

    case lir::InstructionKind::kDelay: {
      assert(instr.operands[0].IsLiteral());
      const auto& literal = instr.operands[0].literal;
      const auto delay_amount = RuntimeValue::FromLiteral(literal).AsUInt64();
      return InstructionResult::Delay(delay_amount);
    }

    case lir::InstructionKind::kSystemCall: {
      if (instr.system_call_name == "$finish") {
        // If there's an argument, we could use it to determine
        // the level of diagnostic info to print (future enhancement)
        // 0 = no info, 1 = minimal info, 2 = full stats

        // For now, we just terminate the simulation
        return InstructionResult::Finish();
      }

      throw std::runtime_error(
          fmt::format("Unsupported system call: {}", instr.system_call_name));
    }

    case lir::InstructionKind::kJump: {
      assert(instr.operands.size() == 1);
      assert(instr.operands[0].IsLabel());
      const auto& target = instr.operands[0].name;
      return InstructionResult::Jump(target);
    }

    case lir::InstructionKind::kBranch: {
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsTemp());
      assert(instr.operands[1].IsLabel());
      assert(instr.operands[2].IsLabel());

      const auto& condition = get_temp(instr.operands[0]);
      const auto& true_target = instr.operands[1].name;
      const auto& false_target = instr.operands[2].name;

      assert(condition.IsTwoState());
      bool condition_result = condition.AsInt64() != 0;

      const auto& next_label = condition_result ? true_target : false_target;
      return InstructionResult::Jump(next_label);
    }
  }
}

}  // namespace lyra::interpreter
