#include "lyra/interpreter/instruction_runner.hpp"

#include <cassert>
#include <format>
#include <iostream>

#include <fmt/core.h>

#include "lyra/common/sv_format.hpp"
#include "lyra/interpreter/builtin_ops.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

namespace {

// Format a RuntimeValue according to a format specifier
// spec: 'd' = decimal, 'x'/'h' = hex, 'b' = binary, 'o' = octal, 's' = string
auto FormatValue(const RuntimeValue& value, char spec) -> std::string {
  if (value.IsString()) {
    return value.AsString();
  }

  auto v = static_cast<uint64_t>(value.AsInt64());
  switch (spec) {
    case 'x':
    case 'h':
      return std::format("{:x}", v);
    case 'b':
      return std::format("{:b}", v);
    case 'o':
      return std::format("{:o}", v);
    default:  // 'd' or 's'
      return value.ToString();
  }
}

// Parse SV format string and format arguments
// Returns formatted output string
auto FormatDisplay(
    const std::string& fmt_str, const std::vector<RuntimeValue>& args)
    -> std::string {
  std::string result;
  size_t arg_idx = 0;
  size_t i = 0;

  while (i < fmt_str.size()) {
    if (fmt_str[i] == '%') {
      if (i + 1 >= fmt_str.size()) {
        throw std::runtime_error("Invalid format string: trailing %");
      }
      char spec = fmt_str[i + 1];
      if (spec == '%') {
        result += '%';
        i += 2;
      } else if (
          spec == 'd' || spec == 'h' || spec == 'x' || spec == 'b' ||
          spec == 'o' || spec == 's') {
        if (arg_idx >= args.size()) {
          throw std::runtime_error("Not enough arguments for format string");
        }
        result += FormatValue(args[arg_idx], spec);
        arg_idx++;
        i += 2;
      } else if (spec >= '0' && spec <= '9') {
        throw std::runtime_error(
            fmt::format(
                "Unsupported format specifier: width modifiers not yet "
                "supported "
                "(found %{}...)",
                spec));
      } else {
        throw std::runtime_error(
            fmt::format("Unsupported format specifier: %{}", spec));
      }
    } else {
      result += fmt_str[i];
      i++;
    }
  }
  return result;
}

}  // namespace

// Execute a single instruction in the given context
auto RunInstruction(
    const lir::Instruction& instr, SimulationContext& simulation_context,
    ProcessContext& process_context, ProcessEffect& effect)
    -> InstructionResult {
  auto& temp_table = process_context.temp_table;
  auto& module_variable_table = simulation_context.variable_table;
  auto& process_variable_table = process_context.variable_table;

  auto get_temp = [&](const lir::Operand& operand) -> RuntimeValue {
    assert(operand.IsTemp());
    return temp_table.Read(std::get<lir::TempRef>(operand.value));
  };

  auto read_variable = [&](const lir::Operand& operand) -> RuntimeValue {
    assert(operand.IsVariable());
    const auto* symbol = std::get<lir::SymbolRef>(operand.value);

    if (process_variable_table.Exists(symbol)) {
      return process_variable_table.Read(symbol);
    }
    return module_variable_table.Read(symbol);
  };

  auto store_variable = [&](const lir::Operand& operand,
                            const RuntimeValue& value, bool is_non_blocking) {
    assert(operand.IsVariable());
    const auto* symbol = std::get<lir::SymbolRef>(operand.value);

    if (process_variable_table.Exists(symbol)) {
      process_variable_table.Write(symbol, value);
    } else {
      module_variable_table.Write(symbol, value);
      if (!is_non_blocking) {
        effect.RecordVariableModification(symbol);
      } else {
        effect.RecordNbaAction(NbaAction{.variable = symbol, .value = value});
      }
    }
  };

  auto eval_unary_op = [&](const lir::Operand& operand,
                           const std::function<RuntimeValue(RuntimeValue)>& op)
      -> InstructionResult {
    const auto result = op(get_temp(operand));
    assert(instr.result.has_value());
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  };

  auto eval_binary_op =
      [&](const lir::Operand& lhs, const lir::Operand& rhs,
          const std::function<RuntimeValue(RuntimeValue, RuntimeValue)>& op)
      -> InstructionResult {
    const auto result = op(get_temp(lhs), get_temp(rhs));
    assert(instr.result.has_value());
    temp_table.Write(instr.result.value(), result);
    return InstructionResult::Continue();
  };

  switch (instr.kind) {
    // Memory operations
    case lir::InstructionKind::kLiteral: {
      assert(instr.operands.size() == 1);
      assert(instr.operands[0].IsLiteral());
      assert(instr.result.has_value());

      const auto& literal = std::get<lir::LiteralRef>(instr.operands[0].value);
      RuntimeValue value = RuntimeValue::FromLiteral(literal);
      temp_table.Write(instr.result.value(), value);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadVariable: {
      const auto& src_variable = read_variable(instr.operands[0]);
      temp_table.Write(instr.result.value(), src_variable);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreVariable: {
      const auto variable = instr.operands[0];
      const auto value = get_temp(instr.operands[1]);
      assert(variable.IsVariable());
      store_variable(variable, value, false);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreVariableNonBlocking: {
      const auto variable = instr.operands[0];
      const auto value = get_temp(instr.operands[1]);
      assert(variable.IsVariable());
      store_variable(variable, value, true);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kMove: {
      assert(instr.operands.size() == 1);
      assert(instr.result.has_value());

      const auto value = get_temp(instr.operands[0]);
      temp_table.Write(instr.result.value(), value);
      return InstructionResult::Continue();
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

    case lir::InstructionKind::kBinaryPower: {
      return eval_binary_op(instr.operands[0], instr.operands[1], BinaryPower);
    }

    case lir::InstructionKind::kBinaryBitwiseAnd: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryBitwiseAnd);
    }

    case lir::InstructionKind::kBinaryBitwiseOr: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryBitwiseOr);
    }

    case lir::InstructionKind::kBinaryBitwiseXor: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryBitwiseXor);
    }

    case lir::InstructionKind::kBinaryBitwiseXnor: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryBitwiseXnor);
    }

    case lir::InstructionKind::kBinaryLogicalAnd: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryLogicalAnd);
    }

    case lir::InstructionKind::kBinaryLogicalOr: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryLogicalOr);
    }

    case lir::InstructionKind::kBinaryLogicalShiftLeft: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryLogicalShiftLeft);
    }

    case lir::InstructionKind::kBinaryLogicalShiftRight: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryLogicalShiftRight);
    }

    case lir::InstructionKind::kBinaryArithmeticShiftLeft: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryArithmeticShiftLeft);
    }

    case lir::InstructionKind::kBinaryArithmeticShiftRight: {
      return eval_binary_op(
          instr.operands[0], instr.operands[1], BinaryArithmeticShiftRight);
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
        throw std::runtime_error(
            fmt::format(
                "Conversion only supports two-state types, got: {} -> {}",
                src.type, target_type));
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
      const auto& literal = std::get<lir::LiteralRef>(instr.operands[0].value);
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

      if (instr.system_call_name == "$display") {
        // Empty $display - just print newline
        if (instr.operands.empty()) {
          std::cout << "\n";
          return InstructionResult::Continue();
        }

        // Check if first operand is a format string (string with %)
        const auto& first = get_temp(instr.operands[0]);
        if (first.IsString()) {
          auto fmt_str = first.AsString();
          if (fmt_str.find('%') != std::string::npos) {
            // Collect remaining arguments
            std::vector<RuntimeValue> args;
            for (size_t i = 1; i < instr.operands.size(); ++i) {
              args.push_back(get_temp(instr.operands[i]));
            }
            std::cout << FormatDisplay(fmt_str, args) << "\n";
            return InstructionResult::Continue();
          }
        }

        // No format specifiers - generate format string with %d placeholders
        // No automatic spacing - matches C++ printf behavior
        std::string gen_fmt;
        std::vector<RuntimeValue> args;
        for (size_t i = 0; i < instr.operands.size(); ++i) {
          const auto& value = get_temp(instr.operands[i]);
          if (value.IsString()) {
            gen_fmt += "%s";
          } else {
            gen_fmt += "%d";
          }
          args.push_back(value);
        }
        std::cout << FormatDisplay(gen_fmt, args) << "\n";
        return InstructionResult::Continue();
      }

      throw std::runtime_error(
          fmt::format("Unsupported system call: {}", instr.system_call_name));
    }

    case lir::InstructionKind::kJump: {
      assert(instr.operands.size() == 1);
      assert(instr.operands[0].IsLabel());
      const auto& target = std::get<lir::LabelRef>(instr.operands[0].value);
      return InstructionResult::Jump(target);
    }

    case lir::InstructionKind::kBranch: {
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsTemp());
      assert(instr.operands[1].IsLabel());
      assert(instr.operands[2].IsLabel());

      const auto& condition = get_temp(instr.operands[0]);
      const auto& true_target =
          std::get<lir::LabelRef>(instr.operands[1].value);
      const auto& false_target =
          std::get<lir::LabelRef>(instr.operands[2].value);

      assert(condition.IsTwoState());
      bool condition_result = condition.AsInt64() != 0;

      const auto& next_label = condition_result ? true_target : false_target;
      return InstructionResult::Jump(next_label);
    }
  }
}

}  // namespace lyra::interpreter
