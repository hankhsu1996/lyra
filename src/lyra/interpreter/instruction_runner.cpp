#include "lyra/interpreter/instruction_runner.hpp"

#include <cassert>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <fmt/format.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/time_format.hpp"
#include "lyra/common/type.hpp"
#include "lyra/interpreter/builtin_ops.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"

namespace lyra::interpreter {

namespace {

struct FormatSpec {
  char spec{};
  bool zero_pad = false;
  bool left_align = false;
  std::string width;
  std::string precision;
};

// Format a RuntimeValue according to a format specifier.
// spec: 'd' = decimal, 'x'/'h' = hex, 'b' = binary, 'o' = octal,
// 's' = string, 'f' = real
auto FormatValue(const RuntimeValue& value, const FormatSpec& spec)
    -> std::string {
  if (value.IsString()) {
    if (!spec.width.empty()) {
      std::string fmt = "{:";
      // SV default is right-align, but fmt::format default is left-align
      if (!spec.left_align) {
        fmt += ">";  // Right-align (SV default) in fmt::format
      }
      fmt += spec.width;
      fmt += "}";
      return fmt::format(fmt::runtime(fmt), value.AsString());
    }
    return value.AsString();
  }

  if (value.IsReal() || value.IsShortReal()) {
    if (spec.spec != 'f' && spec.spec != 'd' && spec.spec != 's') {
      throw DiagnosticException(
          Diagnostic::Error(
              {}, fmt::format("unsupported format specifier: %{}", spec.spec)));
    }
    // Convert shortreal to double for formatting
    double dval = value.IsReal() ? value.AsDouble()
                                 : static_cast<double>(value.AsFloat());
    if (spec.spec == 'f') {
      std::string fmt = "{:";
      if (spec.zero_pad && !spec.width.empty()) {
        fmt += "0>";
      }
      if (!spec.width.empty()) {
        fmt += spec.width;
      }
      if (!spec.precision.empty()) {
        fmt += ".";
        fmt += spec.precision;
      }
      fmt += "f}";
      return fmt::format(fmt::runtime(fmt), dval);
    }
    return value.ToString();
  }

  // Build format string with optional width for integer types
  auto build_int_format = [&spec](char type_char) -> std::string {
    std::string fmt = "{:";
    if (spec.zero_pad) {
      fmt += "0";
    }
    if (!spec.width.empty()) {
      fmt += spec.width;
    }
    fmt += type_char;
    fmt += "}";
    return fmt;
  };

  switch (spec.spec) {
    case 'x':
    case 'h': {
      // Unsigned for hex/binary/octal
      auto v = static_cast<uint64_t>(value.AsInt64());
      return fmt::format(fmt::runtime(build_int_format('x')), v);
    }
    case 'b': {
      auto v = static_cast<uint64_t>(value.AsInt64());
      return fmt::format(fmt::runtime(build_int_format('b')), v);
    }
    case 'o': {
      auto v = static_cast<uint64_t>(value.AsInt64());
      return fmt::format(fmt::runtime(build_int_format('o')), v);
    }
    case 'd': {
      // Signed for decimal to preserve negative numbers
      auto v = value.AsInt64();
      return fmt::format(fmt::runtime(build_int_format('d')), v);
    }
    default:  // 's'
      if (!spec.width.empty()) {
        std::string fmt = "{:";
        // SV default is right-align, but fmt::format default is left-align
        if (!spec.left_align) {
          fmt += ">";  // Right-align (SV default) in fmt::format
        }
        fmt += spec.width;
        fmt += "}";
        return fmt::format(fmt::runtime(fmt), value.ToString());
      }
      return value.ToString();
  }
}

// Context for time formatting (%t specifier)
struct TimeFormatContext {
  const common::TimeFormatState& time_format;
  int8_t module_unit_power;  // Module's timeunit (e.g., -9 for 1ns)
  int8_t global_precision_power;
};

// Parse SV format string and format arguments
// Returns formatted output string
// time_ctx: Optional context for %t formatting
auto FormatDisplay(
    const std::string& fmt_str, const std::vector<RuntimeValue>& args,
    const TimeFormatContext* time_ctx = nullptr) -> std::string {
  std::string result;
  size_t arg_idx = 0;
  size_t i = 0;

  while (i < fmt_str.size()) {
    if (fmt_str[i] == '%') {
      if (i + 1 >= fmt_str.size()) {
        throw DiagnosticException(
            Diagnostic::Error({}, "invalid format string: trailing %"));
      }
      if (fmt_str[i + 1] == '%') {
        result += '%';
        i += 2;
      } else {
        FormatSpec spec;
        ++i;  // Consume '%'

        // Check for left-align flag '-'
        if (i < fmt_str.size() && fmt_str[i] == '-') {
          spec.left_align = true;
          ++i;
        }

        if (i < fmt_str.size() && fmt_str[i] == '0') {
          spec.zero_pad = true;
          ++i;
        }

        while (i < fmt_str.size() && (std::isdigit(fmt_str[i]) != 0)) {
          spec.width += fmt_str[i];
          ++i;
        }

        if (i < fmt_str.size() && fmt_str[i] == '.') {
          ++i;
          if (i >= fmt_str.size() || (std::isdigit(fmt_str[i]) == 0)) {
            throw DiagnosticException(
                Diagnostic::Error(
                    {}, "invalid format string: missing precision digits"));
          }
          while (i < fmt_str.size() && (std::isdigit(fmt_str[i]) != 0)) {
            spec.precision += fmt_str[i];
            ++i;
          }
        }

        if (i >= fmt_str.size()) {
          throw DiagnosticException(
              Diagnostic::Error({}, "invalid format string: trailing %"));
        }

        spec.spec = fmt_str[i];
        if (spec.spec != 'd' && spec.spec != 'h' && spec.spec != 'x' &&
            spec.spec != 'b' && spec.spec != 'o' && spec.spec != 's' &&
            spec.spec != 'f' && spec.spec != 't') {
          throw DiagnosticException(
              Diagnostic::Error(
                  {},
                  fmt::format("unsupported format specifier: %{}", spec.spec)));
        }

        // Precision is only valid for %f (floats)
        if (spec.spec != 'f' && !spec.precision.empty()) {
          throw DiagnosticException(
              Diagnostic::Error(
                  {}, fmt::format(
                          "unsupported format specifier: precision not "
                          "supported for "
                          "%{}",
                          spec.spec)));
        }

        if (arg_idx >= args.size()) {
          throw DiagnosticException(
              Diagnostic::Error({}, "not enough arguments for format string"));
        }

        // %t formats time value according to $timeformat settings
        if (spec.spec == 't') {
          if (time_ctx != nullptr) {
            // Format time value using $timeformat settings
            uint64_t time_val = args[arg_idx].AsUInt64();
            result += time_ctx->time_format.FormatModuleTime(
                time_val, time_ctx->module_unit_power,
                time_ctx->global_precision_power);
          } else {
            // No time context - just format as decimal
            result += FormatValue(args[arg_idx], spec);
          }
          arg_idx++;
          ++i;
          continue;
        }

        result += FormatValue(args[arg_idx], spec);
        arg_idx++;
        ++i;
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

    case lir::InstructionKind::kLoadUnpackedElement: {
      // Load element from unpacked array: result = array[index]
      assert(instr.operands.size() == 2);
      assert(instr.operands[0].IsVariable());
      assert(instr.result.has_value());

      auto array_value = read_variable(instr.operands[0]);
      assert(array_value.IsArray());

      auto index_value = get_temp(instr.operands[1]);
      auto index = static_cast<size_t>(index_value.AsInt64());

      // Get lower bound from array type
      const auto& array_type = array_value.type;
      const auto& array_data =
          std::get<common::UnpackedArrayData>(array_type.data);
      int32_t lower_bound = array_data.lower_bound;

      // Adjust index: actual_idx = sv_idx - lower_bound
      auto actual_idx =
          static_cast<size_t>(static_cast<int64_t>(index) - lower_bound);

      // Bounds check
      if (actual_idx >= array_value.AsArray().size()) {
        throw DiagnosticException(
            Diagnostic::Error(
                {}, fmt::format(
                        "array index {} out of bounds (size {})", index,
                        array_value.AsArray().size())));
      }

      const auto& element = array_value.GetElement(actual_idx);
      temp_table.Write(instr.result.value(), element);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreUnpackedElement: {
      // Store element to unpacked array: array[index] = value
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsVariable());

      // RuntimeValue uses shared_ptr for array storage, so modifications
      // through this copy will affect the original in the variable table
      auto array_value = read_variable(instr.operands[0]);
      assert(array_value.IsArray());

      auto index_value = get_temp(instr.operands[1]);
      auto index = static_cast<size_t>(index_value.AsInt64());
      auto value = get_temp(instr.operands[2]);

      // Get lower bound from array type
      const auto& array_type = array_value.type;
      const auto& array_data =
          std::get<common::UnpackedArrayData>(array_type.data);
      int32_t lower_bound = array_data.lower_bound;

      // Adjust index: actual_idx = sv_idx - lower_bound
      auto actual_idx =
          static_cast<size_t>(static_cast<int64_t>(index) - lower_bound);

      // Bounds check
      if (actual_idx >= array_value.AsArray().size()) {
        throw DiagnosticException(
            Diagnostic::Error(
                {}, fmt::format(
                        "array index {} out of bounds (size {})", index,
                        array_value.AsArray().size())));
      }

      array_value.SetElement(actual_idx, value);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadPackedElement: {
      // Load element/bit from packed vector: result = value[index]
      assert(instr.operands.size() == 2);
      assert(instr.operands[0].IsTemp());
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());

      auto value = get_temp(instr.operands[0]);
      assert(value.IsTwoState());

      auto index_value = get_temp(instr.operands[1]);
      auto index = static_cast<size_t>(index_value.AsInt64());

      // Get element width from result type
      const auto& result_type = instr.result_type.value();
      assert(result_type.kind == common::Type::Kind::kIntegral);
      auto result_data = std::get<common::IntegralData>(result_type.data);
      size_t element_width = result_data.bit_width;

      // Compute bit position: start_bit = index * element_width
      size_t start_bit = index * element_width;

      // Extract element: (value >> start_bit) & mask
      uint64_t mask = common::MakeBitMask(static_cast<uint32_t>(element_width));
      auto extracted = (value.AsUInt64() >> start_bit) & mask;
      auto result = RuntimeValue::IntegralUnsigned(extracted, element_width);
      temp_table.Write(instr.result.value(), result);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadPackedSlice: {
      // Load slice from packed vector: result = value[msb:lsb]
      // operands[0] = value (packed vector)
      // operands[1] = lsb (shift amount)
      // result_type contains the width
      assert(instr.operands.size() == 2);
      assert(instr.operands[0].IsTemp());
      assert(instr.operands[1].IsTemp());
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());

      auto value = get_temp(instr.operands[0]);
      auto lsb_value = get_temp(instr.operands[1]);
      auto lsb = static_cast<size_t>(lsb_value.AsInt64());

      const auto& result_type = instr.result_type.value();
      assert(result_type.kind == common::Type::Kind::kIntegral);
      auto result_data = std::get<common::IntegralData>(result_type.data);
      size_t width = result_data.bit_width;

      // Extract slice: (value >> lsb) & mask
      uint64_t mask = common::MakeBitMask(static_cast<uint32_t>(width));
      auto extracted = (value.AsUInt64() >> lsb) & mask;

      auto result = result_data.is_signed
                        ? RuntimeValue::IntegralSigned(
                              static_cast<int64_t>(extracted), width)
                        : RuntimeValue::IntegralUnsigned(extracted, width);
      temp_table.Write(instr.result.value(), result);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStorePackedElement: {
      // Store element to packed vector: variable[index] = value
      // operands[0] = variable (packed vector)
      // operands[1] = index
      // operands[2] = value to store
      // result_type contains element width
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsVariable());
      assert(instr.operands[1].IsTemp());
      assert(instr.operands[2].IsTemp());
      assert(instr.result_type.has_value());

      auto current = read_variable(instr.operands[0]);
      assert(current.IsTwoState());

      auto index_value = get_temp(instr.operands[1]);
      auto index = static_cast<size_t>(index_value.AsInt64());

      auto new_value = get_temp(instr.operands[2]);

      // Get element width from result_type (the element being stored)
      const auto& elem_type = instr.result_type.value();
      size_t element_width = elem_type.GetBitWidth();

      // Compute bit position and masks
      size_t shift = index * element_width;
      uint64_t elem_mask =
          common::MakeBitMask(static_cast<uint32_t>(element_width));
      uint64_t clear_mask = ~(elem_mask << shift);

      // Merge: (current & ~(mask << shift)) | ((new_value & mask) << shift)
      uint64_t merged = (current.AsUInt64() & clear_mask) |
                        ((new_value.AsUInt64() & elem_mask) << shift);

      // Write back with original type's width and signedness
      const auto& current_data =
          std::get<common::IntegralData>(current.type.data);
      auto result =
          current_data.is_signed
              ? RuntimeValue::IntegralSigned(
                    static_cast<int64_t>(merged), current_data.bit_width)
              : RuntimeValue::IntegralUnsigned(merged, current_data.bit_width);
      store_variable(instr.operands[0], result, false);
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

      // Handle string to string and real to real as no-op conversions.
      if ((src.type == common::Type::String() &&
           target_type == common::Type::String()) ||
          (src.type == common::Type::Real() &&
           target_type == common::Type::Real())) {
        temp_table.Write(instr.result.value(), src);
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kIntegral &&
          target_type.kind == common::Type::Kind::kIntegral) {
        auto two_state_data = std::get<common::IntegralData>(target_type.data);

        // Reject bit width greater than 64
        if (two_state_data.bit_width > 64) {
          throw DiagnosticException(
              Diagnostic::Error(
                  {},
                  fmt::format(
                      "unsupported target bit width > 64: {}", target_type)));
        }

        // Extract source value as int64
        int64_t raw_value = src.AsInt64();

        // Apply sign/bitwidth conversion
        RuntimeValue result = two_state_data.is_signed
                                  ? RuntimeValue::IntegralSigned(
                                        raw_value, two_state_data.bit_width)
                                  : RuntimeValue::IntegralUnsigned(
                                        static_cast<uint64_t>(raw_value),
                                        two_state_data.bit_width);

        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kIntegral &&
          target_type.kind == common::Type::Kind::kReal) {
        auto two_state_data = std::get<common::IntegralData>(src.type.data);
        double real_value = two_state_data.is_signed
                                ? static_cast<double>(src.AsInt64())
                                : static_cast<double>(src.AsUInt64());
        temp_table.Write(instr.result.value(), RuntimeValue::Real(real_value));
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kReal &&
          target_type.kind == common::Type::Kind::kIntegral) {
        auto two_state_data = std::get<common::IntegralData>(target_type.data);
        if (two_state_data.bit_width > 64) {
          throw DiagnosticException(
              Diagnostic::Error(
                  {},
                  fmt::format(
                      "unsupported target bit width > 64: {}", target_type)));
        }

        auto raw_value = static_cast<int64_t>(src.AsDouble());
        RuntimeValue result = two_state_data.is_signed
                                  ? RuntimeValue::IntegralSigned(
                                        raw_value, two_state_data.bit_width)
                                  : RuntimeValue::IntegralUnsigned(
                                        static_cast<uint64_t>(raw_value),
                                        two_state_data.bit_width);

        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      // Shortreal conversions
      if (src.type.kind == common::Type::Kind::kIntegral &&
          target_type.kind == common::Type::Kind::kShortReal) {
        auto two_state_data = std::get<common::IntegralData>(src.type.data);
        float float_value = two_state_data.is_signed
                                ? static_cast<float>(src.AsInt64())
                                : static_cast<float>(src.AsUInt64());
        temp_table.Write(
            instr.result.value(), RuntimeValue::ShortReal(float_value));
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kShortReal &&
          target_type.kind == common::Type::Kind::kIntegral) {
        auto two_state_data = std::get<common::IntegralData>(target_type.data);
        if (two_state_data.bit_width > 64) {
          throw DiagnosticException(
              Diagnostic::Error(
                  {},
                  fmt::format(
                      "unsupported target bit width > 64: {}", target_type)));
        }

        auto raw_value = static_cast<int64_t>(src.AsFloat());
        RuntimeValue result = two_state_data.is_signed
                                  ? RuntimeValue::IntegralSigned(
                                        raw_value, two_state_data.bit_width)
                                  : RuntimeValue::IntegralUnsigned(
                                        static_cast<uint64_t>(raw_value),
                                        two_state_data.bit_width);

        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kReal &&
          target_type.kind == common::Type::Kind::kShortReal) {
        // Precision loss: double -> float
        temp_table.Write(
            instr.result.value(),
            RuntimeValue::ShortReal(static_cast<float>(src.AsDouble())));
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kShortReal &&
          target_type.kind == common::Type::Kind::kReal) {
        // Precision gain: float -> double
        temp_table.Write(
            instr.result.value(),
            RuntimeValue::Real(static_cast<double>(src.AsFloat())));
        return InstructionResult::Continue();
      }

      throw DiagnosticException(
          Diagnostic::Error(
              {}, fmt::format(
                      "conversion only supports two-state/real/shortreal "
                      "types, got: {} -> {}",
                      src.type, target_type)));
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
      // Simulation control tasks: $finish, $stop, $exit
      if (instr.system_call_name == "$finish" ||
          instr.system_call_name == "$stop" ||
          instr.system_call_name == "$exit") {
        bool is_stop = (instr.system_call_name == "$stop");

        // Get diagnostic level: 0 = nothing, 1 = time, 2 = time + stats
        // $exit has no argument, treat as level 1
        // $finish and $stop default to 1 if no argument (handled in lowering)
        int level = 1;
        if (!instr.operands.empty()) {
          level = static_cast<int>(get_temp(instr.operands[0]).AsUInt64());
        }

        // Print diagnostics based on level (VCS style)
        if (level >= 1) {
          simulation_context.display_output
              << instr.system_call_name << " called at time "
              << simulation_context.current_time << "\n";
        }

        return InstructionResult::Finish(is_stop);
      }

      if (instr.system_call_name == "$display") {
        // Empty $display - just print newline
        if (instr.operands.empty()) {
          simulation_context.display_output << "\n";
          return InstructionResult::Continue();
        }

        // Create time format context for %t specifier
        TimeFormatContext time_ctx{
            .time_format = simulation_context.time_format,
            .module_unit_power = simulation_context.timescale
                                     ? simulation_context.timescale->unit_power
                                     : common::TimeScale::kDefaultUnitPower,
            .global_precision_power =
                simulation_context.global_precision_power};

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
            simulation_context.display_output
                << FormatDisplay(fmt_str, args, &time_ctx) << "\n";
            return InstructionResult::Continue();
          }
        }

        // No format specifiers - generate format string with %d placeholders
        // No automatic spacing - matches C++ printf behavior
        std::string gen_fmt;
        std::vector<RuntimeValue> args;
        for (const auto& operand : instr.operands) {
          const auto& value = get_temp(operand);
          if (value.IsString()) {
            gen_fmt += "%s";
          } else if (value.IsReal() || value.IsShortReal()) {
            gen_fmt += "%f";
          } else {
            gen_fmt += "%d";
          }
          args.push_back(value);
        }
        simulation_context.display_output
            << FormatDisplay(gen_fmt, args, &time_ctx) << "\n";
        return InstructionResult::Continue();
      }

      // Simulation time functions: $time, $stime, $realtime
      // Scale raw simulation time to module's timeunit per LRM
      auto scale_time = [&simulation_context]() -> uint64_t {
        uint64_t raw_time = simulation_context.current_time;
        if (!simulation_context.timescale) {
          return raw_time;
        }
        uint64_t divisor = simulation_context.timescale->TimeDivisor(
            simulation_context.global_precision_power);
        return raw_time / divisor;
      };

      if (instr.system_call_name == "$time") {
        assert(instr.result.has_value());
        // $time returns 64-bit unsigned time in module's timeunit
        auto result = RuntimeValue::TwoStateUnsigned(scale_time(), 64);
        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (instr.system_call_name == "$stime") {
        assert(instr.result.has_value());
        // $stime returns low 32 bits of scaled time as unsigned
        auto result =
            RuntimeValue::TwoStateUnsigned(scale_time() & 0xFFFFFFFF, 32);
        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (instr.system_call_name == "$realtime") {
        assert(instr.result.has_value());
        // $realtime returns scaled time as real (double)
        // For accurate fractional time, divide raw time as double
        auto scaled_time = static_cast<double>(simulation_context.current_time);
        if (simulation_context.timescale) {
          auto divisor =
              static_cast<double>(simulation_context.timescale->TimeDivisor(
                  simulation_context.global_precision_power));
          scaled_time /= divisor;
        }
        auto result = RuntimeValue::Real(scaled_time);
        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (instr.system_call_name == "$timeformat") {
        // $timeformat(units, precision, suffix, min_width)
        // All arguments are optional; use defaults if not provided
        auto& tf = simulation_context.time_format;

        if (instr.operands.size() >= 1) {
          tf.units = static_cast<int8_t>(get_temp(instr.operands[0]).AsInt64());
        }
        if (instr.operands.size() >= 2) {
          tf.precision =
              static_cast<int>(get_temp(instr.operands[1]).AsInt64());
        }
        if (instr.operands.size() >= 3) {
          tf.suffix = get_temp(instr.operands[2]).AsString();
        }
        if (instr.operands.size() >= 4) {
          tf.min_width =
              static_cast<int>(get_temp(instr.operands[3]).AsInt64());
        }

        return InstructionResult::Continue();
      }

      // Supported system calls are validated in AST→MIR
      assert(false && "unsupported system call should be rejected in AST→MIR");
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

      bool condition_result = false;
      if (condition.IsReal()) {
        condition_result = condition.AsDouble() != 0.0;
      } else if (condition.IsShortReal()) {
        condition_result = condition.AsFloat() != 0.0F;
      } else {
        assert(condition.IsTwoState());
        condition_result = condition.AsInt64() != 0;
      }

      const auto& next_label = condition_result ? true_target : false_target;
      return InstructionResult::Jump(next_label);
    }
  }
}

}  // namespace lyra::interpreter
