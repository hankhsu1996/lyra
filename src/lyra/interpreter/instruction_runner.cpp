#include "lyra/interpreter/instruction_runner.hpp"

#include <algorithm>
#include <bit>
#include <cassert>
#include <cctype>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <format>
#include <functional>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <fmt/core.h>
#include <fmt/format.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/mem_io.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/time_format.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/wide_bit_ops.hpp"
#include "lyra/interpreter/builtin_ops.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/sdk/time_utils.hpp"

namespace lyra::interpreter {

namespace {

struct FormatSpec {
  char spec{};
  bool zero_pad = false;
  bool left_align = false;
  std::string width;
  std::string precision;
};

// Properties for display/write variants
struct DisplayVariantProps {
  char default_format;  // 'd', 'b', 'o', or 'h'
  bool append_newline;  // true for $display*, false for $write*
};

// Get properties for a display/write/strobe variant
auto GetDisplayVariantProps(std::string_view name) -> DisplayVariantProps {
  if (name == "$write") {
    return {.default_format = 'd', .append_newline = false};
  }
  if (name == "$writeb") {
    return {.default_format = 'b', .append_newline = false};
  }
  if (name == "$writeo") {
    return {.default_format = 'o', .append_newline = false};
  }
  if (name == "$writeh") {
    return {.default_format = 'h', .append_newline = false};
  }
  if (name == "$displayb" || name == "$strobeb" || name == "$monitorb") {
    return {.default_format = 'b', .append_newline = true};
  }
  if (name == "$displayo" || name == "$strobeo" || name == "$monitoro") {
    return {.default_format = 'o', .append_newline = true};
  }
  if (name == "$displayh" || name == "$strobeh" || name == "$monitorh") {
    return {.default_format = 'h', .append_newline = true};
  }
  // $display, $strobe, $monitor (default)
  return {.default_format = 'd', .append_newline = true};
}

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

  // Handle wide values (>64 bits)
  if (value.IsWide()) {
    const auto& wide = value.AsWideBit();
    switch (spec.spec) {
      case 'x':
      case 'h':
        return wide.ToHexString();
      case 'b':
        return wide.ToBinaryString();
      case 'o':
        return wide.ToOctalString();
      case 'd':
        return wide.ToDecimalString();
      default:
        return wide.ToHexString();
    }
  }

  auto narrow = value.AsNarrow();

  switch (spec.spec) {
    case 'x':
    case 'h': {
      // Unsigned for hex
      auto v = narrow.AsUInt64();
      return fmt::format(fmt::runtime(build_int_format('x')), v);
    }
    case 'b': {
      // Unsigned for binary
      auto v = narrow.AsUInt64();
      return fmt::format(fmt::runtime(build_int_format('b')), v);
    }
    case 'o': {
      // Unsigned for octal
      auto v = narrow.AsUInt64();
      return fmt::format(fmt::runtime(build_int_format('o')), v);
    }
    case 'd': {
      // Signed for decimal to preserve negative numbers
      auto v = narrow.AsInt64();
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
  common::TimeFormatState time_format;
  int8_t module_unit_power = 0;  // Module's timeunit (e.g., -9 for 1ns)
  int8_t global_precision_power = 0;
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
            uint64_t time_val = args[arg_idx].AsNarrow().AsUInt64();
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

struct MemTargetInfo {
  bool is_unpacked = false;
  size_t element_width = 0;
  size_t element_count = 0;
  int32_t lower_bound = 0;
};

auto GetMemTargetInfo(const RuntimeValue& value, const common::Type& type)
    -> MemTargetInfo {
  if (type.kind == common::Type::Kind::kUnpackedArray) {
    const auto& array_data = std::get<common::UnpackedArrayData>(type.data);
    if (array_data.element_type->kind != common::Type::Kind::kIntegral) {
      throw common::InternalError(
          "interpreter", "readmem/writemem target element must be integral");
    }
    return {
        .is_unpacked = true,
        .element_width = array_data.element_type->GetBitWidth(),
        .element_count = value.AsArray().size(),
        .lower_bound = array_data.lower_bound,
    };
  }

  if (type.kind == common::Type::Kind::kIntegral) {
    const auto& integral = std::get<common::IntegralData>(type.data);
    if (integral.element_type != nullptr) {
      return {
          .is_unpacked = false,
          .element_width = integral.element_type->GetBitWidth(),
          .element_count = integral.element_count,
          .lower_bound = integral.element_lower,
      };
    }
    return {
        .is_unpacked = false,
        .element_width = integral.bit_width,
        .element_count = 1,
        .lower_bound = 0,
    };
  }

  throw common::InternalError(
      "interpreter", "readmem/writemem target must be an array");
}

auto ParseMemTokenToValue(std::string_view token, size_t bit_width, bool is_hex)
    -> RuntimeValue {
  auto words = common::mem_io::ParseMemTokenToWords(
      token, bit_width, is_hex, [](std::string_view message) {
        throw common::InternalError("interpreter", std::string(message));
      });

  if (bit_width <= 64) {
    return RuntimeValue::IntegralUnsigned(
        words.empty() ? 0 : words[0], bit_width);
  }
  common::WideBit wide(std::move(words));
  return RuntimeValue::IntegralWide(std::move(wide), bit_width);
}

auto ExtractPackedElement(
    const RuntimeValue& value, size_t index, size_t element_width)
    -> RuntimeValue {
  size_t start_bit = index * element_width;
  if (element_width <= 64) {
    uint64_t mask = common::MakeBitMask(static_cast<uint32_t>(element_width));
    uint64_t extracted = 0;
    if (value.IsWide()) {
      auto shifted = value.AsWideBit().ShiftRightLogical(start_bit);
      extracted = shifted.GetWord(0) & mask;
    } else {
      extracted = (value.AsNarrow().AsUInt64() >> start_bit) & mask;
    }
    return RuntimeValue::IntegralUnsigned(extracted, element_width);
  }

  auto wide_value = value.IsWide() ? value.AsWideBit()
                                   : common::WideBit::FromUInt64(
                                         value.AsNarrow().AsUInt64(), 2);
  auto extracted = wide_value.ExtractSlice(start_bit, element_width);
  return RuntimeValue::IntegralWide(std::move(extracted), element_width);
}

auto StorePackedElement(
    const RuntimeValue& current, size_t index, size_t element_width,
    const RuntimeValue& new_value) -> RuntimeValue {
  const auto& current_data = std::get<common::IntegralData>(current.type.data);
  size_t storage_width = current_data.bit_width;
  bool storage_is_wide = current.IsWide();
  bool element_is_wide = element_width > 64;
  size_t shift = index * element_width;

  if (!storage_is_wide && !element_is_wide) {
    uint64_t elem_mask =
        common::MakeBitMask(static_cast<uint32_t>(element_width));
    uint64_t clear_mask = ~(elem_mask << shift);
    uint64_t merged = (current.AsNarrow().AsUInt64() & clear_mask) |
                      ((new_value.AsNarrow().AsUInt64() & elem_mask) << shift);
    return current_data.is_signed
               ? RuntimeValue::IntegralSigned(
                     static_cast<int64_t>(merged), storage_width)
               : RuntimeValue::IntegralUnsigned(merged, storage_width);
  }

  size_t storage_words = common::wide_ops::WordsForBits(storage_width);
  auto current_wide = storage_is_wide
                          ? current.AsWideBit()
                          : common::WideBit::FromUInt64(
                                current.AsNarrow().AsUInt64(), storage_words);
  auto value_wide = element_is_wide
                        ? new_value.AsWideBit()
                        : common::WideBit::FromUInt64(
                              new_value.AsNarrow().AsUInt64(), storage_words);
  auto merged = current_wide.InsertSlice(value_wide, shift, element_width);
  return RuntimeValue::IntegralWide(
      std::move(merged), storage_width, current_data.is_signed);
}

auto FormatMemValue(const RuntimeValue& value, size_t bit_width, bool is_hex)
    -> std::string {
  auto word_count = common::wide_ops::WordsForBits(bit_width);
  std::vector<uint64_t> words(word_count, 0);
  value.MatchIntegral(
      [&](RuntimeValue::NarrowIntegral n) {
        if (!words.empty()) {
          words[0] = n.AsUInt64();
        }
      },
      [&](const common::WideBit& w) {
        for (size_t i = 0; i < words.size(); ++i) {
          words[i] = w.GetWord(i);
        }
      });
  return common::mem_io::FormatMemWords(words, bit_width, is_hex);
}

// Execute unary math function (real -> real)
auto ExecuteMathUnary(std::string_view name, double arg) -> double {
  if (name == "$ln") {
    return std::log(arg);
  }
  if (name == "$log10") {
    return std::log10(arg);
  }
  if (name == "$exp") {
    return std::exp(arg);
  }
  if (name == "$sqrt") {
    return std::sqrt(arg);
  }
  if (name == "$floor") {
    return std::floor(arg);
  }
  if (name == "$ceil") {
    return std::ceil(arg);
  }
  if (name == "$sin") {
    return std::sin(arg);
  }
  if (name == "$cos") {
    return std::cos(arg);
  }
  if (name == "$tan") {
    return std::tan(arg);
  }
  if (name == "$asin") {
    return std::asin(arg);
  }
  if (name == "$acos") {
    return std::acos(arg);
  }
  if (name == "$atan") {
    return std::atan(arg);
  }
  if (name == "$sinh") {
    return std::sinh(arg);
  }
  if (name == "$cosh") {
    return std::cosh(arg);
  }
  if (name == "$tanh") {
    return std::tanh(arg);
  }
  if (name == "$asinh") {
    return std::asinh(arg);
  }
  if (name == "$acosh") {
    return std::acosh(arg);
  }
  if (name == "$atanh") {
    return std::atanh(arg);
  }
  std::unreachable();
}

// Execute binary math function (real, real -> real)
auto ExecuteMathBinary(std::string_view name, double a, double b) -> double {
  if (name == "$pow") {
    return std::pow(a, b);
  }
  if (name == "$atan2") {
    return std::atan2(a, b);
  }
  if (name == "$hypot") {
    return std::hypot(a, b);
  }
  std::unreachable();
}

// Extract int64 value from RuntimeValue, handling both narrow and wide sources.
// For wide sources, extracts the low 64 bits.
auto ExtractInt64FromSource(const RuntimeValue& src) -> int64_t {
  return src.IsWide() ? static_cast<int64_t>(src.AsWideBit().GetWord(0))
                      : src.AsNarrow().AsInt64();
}

// Create a WideBit from an int64 value, with optional sign extension.
// Masks the final word to the specified bit width for correct behavior.
auto CreateWideFromInt64(int64_t value, size_t bit_width, bool sign_extend)
    -> common::WideBit {
  auto num_words = common::wide_ops::WordsForBits(bit_width);
  common::WideBit wide(num_words);
  wide.SetWord(0, static_cast<uint64_t>(value));

  if (sign_extend && value < 0) {
    for (size_t i = 1; i < num_words; ++i) {
      wide.SetWord(i, ~uint64_t{0});
    }
  }

  // Mask final word to bit width (matches SDK behavior)
  common::wide_ops::MaskToWidth(wide.Words(), bit_width);
  return wide;
}

// Compute the actual array index after adjusting for lower bound and checking
// bounds. Returns the adjusted index or throws DiagnosticException if out of
// bounds.
auto ComputeArrayIndex(const RuntimeValue& array_value, int64_t sv_index)
    -> size_t {
  const auto& array_data =
      std::get<common::UnpackedArrayData>(array_value.type.data);
  int32_t lower_bound = array_data.lower_bound;

  auto actual_idx = static_cast<size_t>(sv_index - lower_bound);

  if (actual_idx >= array_value.AsArray().size()) {
    throw DiagnosticException(
        Diagnostic::Error(
            {}, fmt::format(
                    "array index {} out of bounds (size {})", sv_index,
                    array_value.AsArray().size())));
  }

  return actual_idx;
}

}  // namespace

// Execute a single instruction in the given context
auto RunInstruction(
    const lir::Instruction& instr, SimulationContext& simulation_context,
    ProcessContext& process_context, ProcessEffect& effect,
    const std::shared_ptr<InstanceContext>& instance_context)
    -> InstructionResult {
  auto& temp_table = process_context.temp_table;
  auto& module_variable_table = simulation_context.variable_table;
  auto& process_variable_table = process_context.variable_table;

  // Helper to resolve symbol through instance context port bindings.
  // Returns (target_symbol, target_instance) where target_instance is nullptr
  // if not bound (use current instance or global).
  auto resolve_binding = [&instance_context](const auto* symbol)
      -> std::pair<common::SymbolRef, std::shared_ptr<InstanceContext>> {
    if (instance_context != nullptr) {
      return instance_context->ResolveBinding(symbol);
    }
    return {symbol, nullptr};
  };

  auto get_temp = [&](const lir::Operand& operand) -> RuntimeValue {
    assert(operand.IsTemp());
    return temp_table.Read(std::get<lir::TempRef>(operand.value));
  };

  auto read_variable = [&](const lir::Operand& operand) -> RuntimeValue {
    assert(operand.IsVariable());
    const auto* symbol = std::get<lir::SymbolRef>(operand.value);

    // Check process-local first
    if (process_variable_table.Exists(symbol)) {
      return process_variable_table.Read(symbol);
    }

    // Resolve through port bindings (output port → target signal/instance)
    auto [target_symbol, target_instance] = resolve_binding(symbol);

    // If bound, read from target instance's storage
    if (target_instance != nullptr) {
      return target_instance->Read(target_symbol);
    }

    // Otherwise, read from per-instance storage (local vars, input ports)
    if (instance_context != nullptr && instance_context->Exists(symbol)) {
      return instance_context->Read(symbol);
    }

    // Fallback to global table (for backwards compat with non-hierarchical
    // code)
    return module_variable_table.Read(symbol);
  };

  auto store_variable = [&](const lir::Operand& operand,
                            const RuntimeValue& value, bool is_non_blocking) {
    assert(operand.IsVariable());
    const auto* symbol = std::get<lir::SymbolRef>(operand.value);

    // Check process-local first
    if (process_variable_table.Exists(symbol)) {
      process_variable_table.Write(symbol, value);
      return;
    }

    // Resolve through port bindings (output port → target signal/instance)
    auto [target_symbol, target_instance] = resolve_binding(symbol);

    // If bound (output port), write to target instance's storage
    if (target_instance != nullptr) {
      if (!is_non_blocking) {
        target_instance->Write(target_symbol, value);
        effect.RecordVariableModification(target_symbol, target_instance);
      } else {
        effect.RecordNbaAction(
            {.variable = target_symbol,
             .value = value,
             .instance = target_instance});
      }
      return;
    }

    // Otherwise, write to per-instance storage (local vars, input ports)
    if (instance_context != nullptr) {
      if (!is_non_blocking) {
        instance_context->Write(symbol, value);
        effect.RecordVariableModification(symbol, instance_context);
      } else {
        effect.RecordNbaAction(
            {.variable = symbol, .value = value, .instance = instance_context});
      }
      return;
    }

    // Fallback to global table (for backwards compat with non-hierarchical
    // code)
    if (!is_non_blocking) {
      module_variable_table.Write(symbol, value);
      effect.RecordVariableModification(symbol);  // Global storage
    } else {
      effect.RecordNbaAction(
          {.variable = symbol, .value = value, .instance = nullptr});
    }
  };

  // Store to hierarchical target: traverse instance path and store to target
  auto store_hierarchical = [&](const std::vector<common::SymbolRef>& instances,
                                common::SymbolRef target,
                                const RuntimeValue& value,
                                bool is_non_blocking) {
    // Traverse instance path
    auto target_instance = instance_context;
    for (const auto& inst_sym : instances) {
      target_instance = target_instance->LookupChild(inst_sym);
      if (!target_instance) {
        throw DiagnosticException(
            Diagnostic::Error(
                {}, fmt::format("Unknown child instance: {}", inst_sym->name)));
      }
    }

    // Write to target instance
    if (!is_non_blocking) {
      target_instance->Write(target, value);
      effect.RecordVariableModification(target, target_instance);
    } else {
      effect.RecordNbaAction(
          {.variable = target, .value = value, .instance = target_instance});
    }
  };

  // Load from hierarchical target: traverse instance path and load from target
  auto load_hierarchical = [&](const std::vector<common::SymbolRef>& instances,
                               common::SymbolRef target) -> RuntimeValue {
    // Traverse instance path
    auto target_instance = instance_context;
    for (const auto& inst_sym : instances) {
      target_instance = target_instance->LookupChild(inst_sym);
      if (!target_instance) {
        throw DiagnosticException(
            Diagnostic::Error(
                {}, fmt::format("Unknown child instance: {}", inst_sym->name)));
      }
    }

    // Read from target instance
    return target_instance->Read(target);
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

  auto get_operand_value = [&](const lir::Operand& operand) -> RuntimeValue {
    if (operand.IsTemp()) {
      return get_temp(operand);
    }
    if (operand.IsVariable()) {
      return read_variable(operand);
    }
    if (operand.IsLiteral()) {
      const auto& literal = std::get<lir::LiteralRef>(operand.value);
      return RuntimeValue::FromLiteral(literal);
    }
    throw common::InternalError(
        "interpreter", "unexpected operand kind for system call");
  };

  auto handle_mem_io = [&](bool is_read, bool is_hex) -> InstructionResult {
    std::string_view task_name = is_read ? "$readmem" : "$writemem";
    if (instr.operands.size() < 2 || instr.operands.size() > 4) {
      throw common::InternalError(
          "interpreter",
          std::format("{} expects 2-4 operands", task_name));
    }
    const auto filename_value = get_operand_value(instr.operands[0]);
    if (!filename_value.IsString()) {
      throw common::InternalError(
          "interpreter",
          std::format("{} filename must be a string", task_name));
    }
    if (!instr.operands[1].IsVariable()) {
      throw common::InternalError(
          "interpreter",
          std::format("{} target must be a variable", task_name));
    }
    auto target_value = read_variable(instr.operands[1]);
    auto info = GetMemTargetInfo(target_value, target_value.type);

    std::optional<int64_t> start_addr;
    std::optional<int64_t> end_addr;
    if (instr.operands.size() >= 3) {
      auto start_val = get_operand_value(instr.operands[2]);
      if (!start_val.IsTwoState() || start_val.IsWide()) {
        throw common::InternalError(
            "interpreter",
            std::format(
                "{} start address must be narrow integral", task_name));
      }
      start_addr = start_val.AsNarrow().AsInt64();
    }
    if (instr.operands.size() == 4) {
      auto end_val = get_operand_value(instr.operands[3]);
      if (!end_val.IsTwoState() || end_val.IsWide()) {
        throw common::InternalError(
            "interpreter",
            std::format("{} end address must be narrow integral", task_name));
      }
      end_addr = end_val.AsNarrow().AsInt64();
    }

    if (info.element_count == 0 || info.element_width == 0) {
      throw common::InternalError(
          "interpreter", std::format("{} target has zero size", task_name));
    }

    int64_t min_addr = info.lower_bound;
    int64_t max_addr =
        info.lower_bound + static_cast<int64_t>(info.element_count) - 1;
    int64_t current_addr = start_addr.value_or(min_addr);
    int64_t final_addr = end_addr.value_or(max_addr);

    if (current_addr < min_addr || current_addr > max_addr) {
      throw common::InternalError(
          "interpreter",
          std::format("{} start address out of bounds", task_name));
    }
    if (final_addr < min_addr || final_addr > max_addr) {
      throw common::InternalError(
          "interpreter", std::format("{} end address out of bounds", task_name));
    }

    auto path = common::mem_io::ResolveMemPath(filename_value.AsString());
    auto handle_read_mem = [&]() -> InstructionResult {
      std::ifstream in(path);
      if (!in) {
        throw common::InternalError(
            "interpreter", std::format(
                               "failed to open memory file: {}", path.string()));
      }

      std::string content(
          (std::istreambuf_iterator<char>(in)),
          std::istreambuf_iterator<char>());

      RuntimeValue packed_value = target_value;
      auto write_value = [&](int64_t addr, const RuntimeValue& value) {
        if (addr < min_addr || addr > max_addr) {
          throw common::InternalError(
              "interpreter",
              std::format("{} address out of bounds", task_name));
        }
        size_t index = static_cast<size_t>(addr - min_addr);
        if (info.is_unpacked) {
          target_value.SetElement(index, value);
        } else {
          packed_value =
              StorePackedElement(packed_value, index, info.element_width, value);
        }
      };

      common::mem_io::ParseMemFile(
          content, is_hex, min_addr, max_addr, current_addr, final_addr,
          task_name,
          [](std::string_view message) {
            throw common::InternalError("interpreter", std::string(message));
          },
          [&](std::string_view token, int64_t addr) {
            auto value =
                ParseMemTokenToValue(token, info.element_width, is_hex);
            write_value(addr, value);
          });

      if (info.is_unpacked) {
        store_variable(instr.operands[1], target_value, false);
      } else {
        store_variable(instr.operands[1], packed_value, false);
      }
      return InstructionResult::Continue();
    };

    auto handle_write_mem = [&]() -> InstructionResult {
      std::ofstream out(path);
      if (!out) {
        throw common::InternalError(
            "interpreter",
            std::format(
                "failed to open memory file for write: {}", path.string()));
      }

      if (start_addr.has_value()) {
        out << "@"
            << common::mem_io::FormatMemAddress(
                   static_cast<uint64_t>(*start_addr), is_hex)
            << "\n";
      }

      for (int64_t addr = current_addr; addr <= final_addr; ++addr) {
        size_t index = static_cast<size_t>(addr - min_addr);
        RuntimeValue element;
        if (info.is_unpacked) {
          element = target_value.GetElement(index);
        } else {
          element =
              ExtractPackedElement(target_value, index, info.element_width);
        }
        out << FormatMemValue(element, info.element_width, is_hex) << "\n";
      }
      return InstructionResult::Continue();
    };

    return is_read ? handle_read_mem() : handle_write_mem();
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
      if (instr.target_symbol != nullptr) {
        // Hierarchical load: traverse instance_path, load from target_symbol
        const auto value =
            load_hierarchical(instr.instance_path, instr.target_symbol);
        temp_table.Write(instr.result.value(), value);
      } else {
        // Regular load: variable in operands[0]
        const auto& src_variable = read_variable(instr.operands[0]);
        temp_table.Write(instr.result.value(), src_variable);
      }
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreVariable: {
      if (instr.target_symbol != nullptr) {
        // Hierarchical store: traverse instance_path, store to target_symbol
        const auto value = get_temp(instr.operands[0]);
        store_hierarchical(
            instr.instance_path, instr.target_symbol, value, false);
      } else {
        // Regular store: variable in operands[0], value in operands[1]
        const auto variable = instr.operands[0];
        const auto value = get_temp(instr.operands[1]);
        assert(variable.IsVariable());
        store_variable(variable, value, false);
      }
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreVariableNonBlocking: {
      if (instr.target_symbol != nullptr) {
        // Hierarchical store: traverse instance_path, store to target_symbol
        const auto value = get_temp(instr.operands[0]);
        store_hierarchical(
            instr.instance_path, instr.target_symbol, value, true);
      } else {
        // Regular store: variable in operands[0], value in operands[1]
        const auto variable = instr.operands[0];
        const auto value = get_temp(instr.operands[1]);
        assert(variable.IsVariable());
        store_variable(variable, value, true);
      }
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadUnpackedElement: {
      // Load element from unpacked array variable: result = array[index]
      assert(instr.operands.size() == 2);
      assert(instr.operands[0].IsVariable());
      assert(instr.result.has_value());

      auto array_value = read_variable(instr.operands[0]);
      assert(array_value.IsArray());

      auto index_value = get_temp(instr.operands[1]);
      assert(!index_value.IsWide() && "array index cannot be wide");
      auto actual_idx =
          ComputeArrayIndex(array_value, index_value.AsNarrow().AsInt64());

      temp_table.Write(
          instr.result.value(), array_value.GetElement(actual_idx));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreUnpackedElement: {
      // Store element to unpacked array variable: array[index] = value
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsVariable());

      // RuntimeValue uses shared_ptr for array storage, so modifications
      // through this copy will affect the original in the variable table
      auto array_value = read_variable(instr.operands[0]);
      assert(array_value.IsArray());

      auto index_value = get_temp(instr.operands[1]);
      assert(!index_value.IsWide() && "array index cannot be wide");
      auto actual_idx =
          ComputeArrayIndex(array_value, index_value.AsNarrow().AsInt64());

      array_value.SetElement(actual_idx, get_temp(instr.operands[2]));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadUnpackedElementFromTemp: {
      // Load element from unpacked array in temp: result = array_temp[index]
      // Used for multi-dimensional array access (e.g., arr[i][j])
      assert(instr.operands.size() == 2);
      assert(instr.operands[0].IsTemp());
      assert(instr.result.has_value());

      auto array_value = get_temp(instr.operands[0]);
      assert(array_value.IsArray());

      auto index_value = get_temp(instr.operands[1]);
      assert(!index_value.IsWide() && "array index cannot be wide");
      auto actual_idx =
          ComputeArrayIndex(array_value, index_value.AsNarrow().AsInt64());

      temp_table.Write(
          instr.result.value(), array_value.GetElement(actual_idx));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreUnpackedElementToTemp: {
      // Store element to unpacked array in temp: array_temp[index] = value
      // Used for multi-dimensional array writes (copy-modify-store pattern)
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsTemp());

      // RuntimeValue uses shared_ptr for array storage, so modifications
      // through this copy will affect the original in the temp table
      auto array_value = get_temp(instr.operands[0]);
      assert(array_value.IsArray());

      auto index_value = get_temp(instr.operands[1]);
      assert(!index_value.IsWide() && "array index cannot be wide");
      auto actual_idx =
          ComputeArrayIndex(array_value, index_value.AsNarrow().AsInt64());

      array_value.SetElement(actual_idx, get_temp(instr.operands[2]));
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
      assert(!index_value.IsWide() && "packed element index cannot be wide");
      auto index = static_cast<size_t>(index_value.AsNarrow().AsInt64());

      // Get element width from result type
      const auto& result_type = instr.result_type.value();
      assert(result_type.kind == common::Type::Kind::kIntegral);
      auto result_data = std::get<common::IntegralData>(result_type.data);
      size_t element_width = result_data.bit_width;

      // Compute bit position: start_bit = index * element_width
      size_t start_bit = index * element_width;

      // Extract element
      RuntimeValue result;
      if (element_width <= 64) {
        // Narrow element - extract as uint64_t
        uint64_t mask =
            common::MakeBitMask(static_cast<uint32_t>(element_width));
        uint64_t extracted = 0;
        if (value.IsWide()) {
          auto shifted = value.AsWideBit().ShiftRightLogical(start_bit);
          extracted = shifted.GetWord(0) & mask;
        } else {
          extracted = (value.AsNarrow().AsUInt64() >> start_bit) & mask;
        }
        result = RuntimeValue::IntegralUnsigned(extracted, element_width);
      } else {
        // Wide element - extract as WideBit
        auto wide_value = value.IsWide() ? value.AsWideBit()
                                         : common::WideBit::FromUInt64(
                                               value.AsNarrow().AsUInt64(), 2);
        auto extracted = wide_value.ExtractSlice(start_bit, element_width);
        result =
            RuntimeValue::IntegralWide(std::move(extracted), element_width);
      }
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
      assert(!lsb_value.IsWide() && "slice index cannot be wide");
      auto lsb = static_cast<size_t>(lsb_value.AsNarrow().AsInt64());

      const auto& result_type = instr.result_type.value();
      assert(result_type.kind == common::Type::Kind::kIntegral);
      auto result_data = std::get<common::IntegralData>(result_type.data);
      size_t width = result_data.bit_width;

      // Extract slice
      RuntimeValue result;
      if (width <= 64) {
        // Narrow slice - extract as uint64_t
        uint64_t mask = common::MakeBitMask(static_cast<uint32_t>(width));
        uint64_t extracted = 0;
        if (value.IsWide()) {
          auto shifted = value.AsWideBit().ShiftRightLogical(lsb);
          extracted = shifted.GetWord(0) & mask;
        } else {
          extracted = (value.AsNarrow().AsUInt64() >> lsb) & mask;
        }
        result = result_data.is_signed
                     ? RuntimeValue::IntegralSigned(
                           static_cast<int64_t>(extracted), width)
                     : RuntimeValue::IntegralUnsigned(extracted, width);
      } else {
        // Wide slice - extract as WideBit
        auto wide_value = value.IsWide() ? value.AsWideBit()
                                         : common::WideBit::FromUInt64(
                                               value.AsNarrow().AsUInt64(), 2);
        auto extracted = wide_value.ExtractSlice(lsb, width);
        result = RuntimeValue::IntegralWide(
            std::move(extracted), width, result_data.is_signed);
      }
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
      assert(!index_value.IsWide() && "packed element index cannot be wide");
      auto index = static_cast<size_t>(index_value.AsNarrow().AsInt64());

      auto new_value = get_temp(instr.operands[2]);

      // Get element width from result_type (the element being stored)
      const auto& elem_type = instr.result_type.value();
      size_t element_width = elem_type.GetBitWidth();

      // Get storage width and signedness
      const auto& current_data =
          std::get<common::IntegralData>(current.type.data);
      size_t storage_width = current_data.bit_width;
      bool storage_is_wide = current.IsWide();
      bool element_is_wide = element_width > 64;

      // Compute bit position
      size_t shift = index * element_width;

      RuntimeValue result;
      if (!storage_is_wide && !element_is_wide) {
        // Narrow storage, narrow element - existing mask-and-merge logic
        uint64_t elem_mask =
            common::MakeBitMask(static_cast<uint32_t>(element_width));
        uint64_t clear_mask = ~(elem_mask << shift);
        uint64_t merged =
            (current.AsNarrow().AsUInt64() & clear_mask) |
            ((new_value.AsNarrow().AsUInt64() & elem_mask) << shift);
        result = current_data.is_signed
                     ? RuntimeValue::IntegralSigned(
                           static_cast<int64_t>(merged), storage_width)
                     : RuntimeValue::IntegralUnsigned(merged, storage_width);
      } else {
        // Wide storage or wide element - use WideBit InsertSlice
        size_t storage_words = common::wide_ops::WordsForBits(storage_width);
        auto current_wide =
            storage_is_wide ? current.AsWideBit()
                            : common::WideBit::FromUInt64(
                                  current.AsNarrow().AsUInt64(), storage_words);
        auto value_wide = element_is_wide ? new_value.AsWideBit()
                                          : common::WideBit::FromUInt64(
                                                new_value.AsNarrow().AsUInt64(),
                                                storage_words);
        auto merged =
            current_wide.InsertSlice(value_wide, shift, element_width);
        result = RuntimeValue::IntegralWide(
            std::move(merged), storage_width, current_data.is_signed);
      }
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

        // Wide target type - create WideBit and sign-extend if needed
        if (two_state_data.bit_width > 64) {
          common::WideBit wide = src.IsWide() ? src.AsWideBit() : [&]() {
            auto src_data = std::get<common::IntegralData>(src.type.data);
            return CreateWideFromInt64(
                src.AsNarrow().AsInt64(), two_state_data.bit_width,
                src_data.is_signed);
          }();

          RuntimeValue result = RuntimeValue::IntegralWide(
              std::move(wide), two_state_data.bit_width,
              two_state_data.is_signed);
          temp_table.Write(instr.result.value(), result);
          return InstructionResult::Continue();
        }

        // Narrow target type - extract value (may be from wide source)
        int64_t raw_value = ExtractInt64FromSource(src);

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
        double real_value = 0.0;
        if (src.IsWide()) {
          // Wide source - convert full value to double
          // Note: For signed wide, we'd need to handle two's complement
          // but typically unsigned for large values
          real_value = src.AsWideBit().ToDouble();
        } else {
          int64_t raw_value = src.AsNarrow().AsInt64();
          real_value =
              two_state_data.is_signed
                  ? static_cast<double>(raw_value)
                  : static_cast<double>(static_cast<uint64_t>(raw_value));
        }
        temp_table.Write(instr.result.value(), RuntimeValue::Real(real_value));
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kReal &&
          target_type.kind == common::Type::Kind::kIntegral) {
        auto two_state_data = std::get<common::IntegralData>(target_type.data);
        auto raw_value = static_cast<int64_t>(src.AsDouble());

        // Wide target type
        if (two_state_data.bit_width > 64) {
          // Real-to-integral: sign-extend if negative
          common::WideBit wide = CreateWideFromInt64(
              raw_value, two_state_data.bit_width,
              /*sign_extend=*/true);
          RuntimeValue result = RuntimeValue::IntegralWide(
              std::move(wide), two_state_data.bit_width,
              two_state_data.is_signed);
          temp_table.Write(instr.result.value(), result);
          return InstructionResult::Continue();
        }

        // Narrow target type
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
        float float_value = 0.0F;
        if (src.IsWide()) {
          // Wide source - convert full value to float
          float_value = static_cast<float>(src.AsWideBit().ToDouble());
        } else {
          int64_t raw_value = src.AsNarrow().AsInt64();
          float_value =
              two_state_data.is_signed
                  ? static_cast<float>(raw_value)
                  : static_cast<float>(static_cast<uint64_t>(raw_value));
        }
        temp_table.Write(
            instr.result.value(), RuntimeValue::ShortReal(float_value));
        return InstructionResult::Continue();
      }

      if (src.type.kind == common::Type::Kind::kShortReal &&
          target_type.kind == common::Type::Kind::kIntegral) {
        auto two_state_data = std::get<common::IntegralData>(target_type.data);
        auto raw_value = static_cast<int64_t>(src.AsFloat());

        // Wide target type
        if (two_state_data.bit_width > 64) {
          // Shortreal-to-integral: sign-extend if negative
          common::WideBit wide = CreateWideFromInt64(
              raw_value, two_state_data.bit_width,
              /*sign_extend=*/true);
          RuntimeValue result = RuntimeValue::IntegralWide(
              std::move(wide), two_state_data.bit_width,
              two_state_data.is_signed);
          temp_table.Write(instr.result.value(), result);
          return InstructionResult::Continue();
        }

        // Narrow target type
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

    // Concatenation: result = {op0, op1, ...}
    // Operands are ordered MSB to LSB (first operand is most significant)
    case lir::InstructionKind::kConcatenation: {
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());

      const auto& result_type = instr.result_type.value();
      size_t result_width = result_type.GetBitWidth();

      // Collect operand values
      std::vector<RuntimeValue> operand_values;
      operand_values.reserve(instr.operands.size());
      for (const auto& operand : instr.operands) {
        operand_values.push_back(get_temp(operand));
      }

      // RuntimeValue::Concatenate handles narrow/wide dispatch internally
      temp_table.Write(
          instr.result.value(),
          RuntimeValue::Concatenate(operand_values, result_width));
      return InstructionResult::Continue();
    }

    // Control flow
    case lir::InstructionKind::kComplete: {
      return InstructionResult::Complete();
    }

    case lir::InstructionKind::kWaitEvent: {
      // Pass triggers through - resolution happens in simulation_runner
      return InstructionResult::WaitEvent(instr.wait_triggers);
    }

    case lir::InstructionKind::kDelay: {
      assert(instr.operands[0].IsLiteral());
      const auto& literal = std::get<lir::LiteralRef>(instr.operands[0].value);
      // Delay amounts are never wide
      const auto delay_amount =
          RuntimeValue::FromLiteral(literal).AsNarrow().AsUInt64();
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
          // Diagnostic level is never wide
          level = static_cast<int>(
              get_operand_value(instr.operands[0]).AsNarrow().AsUInt64());
        }

        // Print diagnostics based on level (VCS style)
        if (level >= 1) {
          simulation_context.display_output
              << instr.system_call_name << " called at time "
              << simulation_context.current_time << "\n";
        }

        return InstructionResult::Finish(is_stop);
      }

      if (instr.system_call_name == "$readmemh" ||
          instr.system_call_name == "$readmemb") {
        bool is_hex = instr.system_call_name == "$readmemh";
        return handle_mem_io(true, is_hex);
      }

      if (instr.system_call_name == "$writememh" ||
          instr.system_call_name == "$writememb") {
        bool is_hex = instr.system_call_name == "$writememh";
        return handle_mem_io(false, is_hex);
      }

      // Handle all display/write variants
      if (instr.system_call_name == "$display" ||
          instr.system_call_name == "$displayb" ||
          instr.system_call_name == "$displayo" ||
          instr.system_call_name == "$displayh" ||
          instr.system_call_name == "$write" ||
          instr.system_call_name == "$writeb" ||
          instr.system_call_name == "$writeo" ||
          instr.system_call_name == "$writeh") {
        auto props = GetDisplayVariantProps(instr.system_call_name);

        // Empty call - just print newline if needed
        if (instr.operands.empty()) {
          if (props.append_newline) {
            simulation_context.display_output << "\n";
          }
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
        const auto& first = get_operand_value(instr.operands[0]);
        if (first.IsString()) {
          auto fmt_str = first.AsString();
          if (fmt_str.find('%') != std::string::npos) {
            // Format string case: use specifiers from format string
            std::vector<RuntimeValue> args;
            for (size_t i = 1; i < instr.operands.size(); ++i) {
              args.push_back(get_operand_value(instr.operands[i]));
            }
            simulation_context.display_output
                << FormatDisplay(fmt_str, args, &time_ctx);
            if (props.append_newline) {
              simulation_context.display_output << "\n";
            }
            return InstructionResult::Continue();
          }
        }

        // No format specifiers - generate format string with variant's default
        std::string gen_fmt;
        std::vector<RuntimeValue> args;
        for (const auto& operand : instr.operands) {
          const auto& value = get_operand_value(operand);
          if (value.IsString()) {
            gen_fmt += "%s";
          } else if (value.IsReal() || value.IsShortReal()) {
            gen_fmt += "%f";
          } else {
            gen_fmt += "%";
            gen_fmt += props.default_format;
          }
          args.push_back(value);
        }
        simulation_context.display_output
            << FormatDisplay(gen_fmt, args, &time_ctx);
        if (props.append_newline) {
          simulation_context.display_output << "\n";
        }
        return InstructionResult::Continue();
      }

      // Handle strobe variants - same as display but queued to Postponed region
      if (instr.system_call_name == "$strobe" ||
          instr.system_call_name == "$strobeb" ||
          instr.system_call_name == "$strobeo" ||
          instr.system_call_name == "$strobeh") {
        auto props = GetDisplayVariantProps(instr.system_call_name);

        // Collect argument values now (they'll be read in Postponed region)
        std::vector<RuntimeValue> arg_values;
        for (const auto& operand : instr.operands) {
          arg_values.push_back(get_operand_value(operand));
        }

        // Create time format context
        TimeFormatContext time_ctx{
            .time_format = simulation_context.time_format,
            .module_unit_power = simulation_context.timescale
                                     ? simulation_context.timescale->unit_power
                                     : common::TimeScale::kDefaultUnitPower,
            .global_precision_power =
                simulation_context.global_precision_power};

        // Queue the output for Postponed region
        effect.RecordPostponedAction(
            PostponedAction{
                .action = [&simulation_context, props,
                           arg_values = std::move(arg_values), time_ctx]() {
                  // Empty call - just print newline if needed
                  if (arg_values.empty()) {
                    if (props.append_newline) {
                      simulation_context.display_output << "\n";
                    }
                    return;
                  }

                  // Check if first operand is a format string
                  const auto& first = arg_values[0];
                  if (first.IsString()) {
                    auto fmt_str = first.AsString();
                    if (fmt_str.find('%') != std::string::npos) {
                      std::vector<RuntimeValue> format_args(
                          arg_values.begin() + 1, arg_values.end());
                      simulation_context.display_output
                          << FormatDisplay(fmt_str, format_args, &time_ctx);
                      if (props.append_newline) {
                        simulation_context.display_output << "\n";
                      }
                      return;
                    }
                  }

                  // No format specifiers - generate format string
                  std::string gen_fmt;
                  for (const auto& value : arg_values) {
                    if (value.IsString()) {
                      gen_fmt += "%s";
                    } else if (value.IsReal() || value.IsShortReal()) {
                      gen_fmt += "%f";
                    } else {
                      gen_fmt += "%";
                      gen_fmt += props.default_format;
                    }
                  }
                  simulation_context.display_output
                      << FormatDisplay(gen_fmt, arg_values, &time_ctx);
                  if (props.append_newline) {
                    simulation_context.display_output << "\n";
                  }
                }});
        return InstructionResult::Continue();
      }

      // Handle monitor variants - register for value change tracking
      if (instr.system_call_name == "$monitor" ||
          instr.system_call_name == "$monitorb" ||
          instr.system_call_name == "$monitoro" ||
          instr.system_call_name == "$monitorh") {
        auto props = GetDisplayVariantProps(instr.system_call_name);

        // Collect argument values
        std::vector<RuntimeValue> arg_values;
        for (const auto& operand : instr.operands) {
          arg_values.push_back(get_operand_value(operand));
        }

        // Build format string (same logic as $display)
        std::string format_string;
        std::vector<RuntimeValue> format_args;

        if (!arg_values.empty() && arg_values[0].IsString()) {
          auto fmt_str = arg_values[0].AsString();
          if (fmt_str.find('%') != std::string::npos) {
            format_string = fmt_str;
            format_args.assign(arg_values.begin() + 1, arg_values.end());
          }
        }

        // If no format string, generate one
        if (format_string.empty()) {
          for (const auto& value : arg_values) {
            if (value.IsString()) {
              format_string += "%s";
            } else if (value.IsReal() || value.IsShortReal()) {
              format_string += "%f";
            } else {
              format_string += "%";
              format_string += props.default_format;
            }
          }
          format_args = std::move(arg_values);
        }

        // Build monitored variables list from instruction metadata
        std::vector<MonitoredVariable> variables;
        for (const auto& arg : instr.monitored_args) {
          if (arg) {
            variables.push_back(
                MonitoredVariable{
                    .expression_block_index = arg->expression_block_index});
          }
        }

        // Get time format context
        auto module_unit_power = simulation_context.timescale
                                     ? simulation_context.timescale->unit_power
                                     : common::TimeScale::kDefaultUnitPower;
        TimeFormatContext time_ctx{
            .time_format = simulation_context.time_format,
            .module_unit_power = module_unit_power,
            .global_precision_power =
                simulation_context.global_precision_power};

        // Print immediately on first call (IEEE 1800 §21.2.3)
        simulation_context.display_output
            << FormatDisplay(format_string, format_args, &time_ctx);
        if (props.append_newline) {
          simulation_context.display_output << "\n";
        }

        // Set up the active monitor (replaces any previous monitor)
        // Use std::move since format_string and format_args are no longer
        // needed
        simulation_context.active_monitor = MonitorState{
            .enabled = true,
            .format_string = std::move(format_string),
            .default_format = props.default_format,
            .append_newline = props.append_newline,
            .variables = std::move(variables),
            .previous_values = std::move(format_args),
            .instance = instance_context,
            .time_format = simulation_context.time_format,
            .module_unit_power = module_unit_power,
            .global_precision_power =
                simulation_context.global_precision_power};

        return InstructionResult::Continue();
      }

      // Handle $monitoron - enable monitoring
      if (instr.system_call_name == "$monitoron") {
        if (simulation_context.active_monitor) {
          simulation_context.active_monitor->enabled = true;
        }
        return InstructionResult::Continue();
      }

      // Handle $monitoroff - disable monitoring
      if (instr.system_call_name == "$monitoroff") {
        if (simulation_context.active_monitor) {
          simulation_context.active_monitor->enabled = false;
        }
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
        auto result = RuntimeValue::IntegralUnsigned(scale_time(), 64);
        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (instr.system_call_name == "$stime") {
        assert(instr.result.has_value());
        // $stime returns low 32 bits of scaled time as unsigned
        auto result =
            RuntimeValue::IntegralUnsigned(scale_time() & 0xFFFFFFFF, 32);
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

        if (!instr.operands.empty()) {
          tf.units = static_cast<int8_t>(
              get_operand_value(instr.operands[0]).AsNarrow().AsInt64());
        }
        if (instr.operands.size() >= 2) {
          tf.precision = static_cast<int>(
              get_operand_value(instr.operands[1]).AsNarrow().AsInt64());
        }
        if (instr.operands.size() >= 3) {
          tf.suffix = get_operand_value(instr.operands[2]).AsString();
        }
        if (instr.operands.size() >= 4) {
          tf.min_width = static_cast<int>(
              get_operand_value(instr.operands[3]).AsNarrow().AsInt64());
        }

        return InstructionResult::Continue();
      }

      if (instr.system_call_name == "$timeunit") {
        assert(instr.result.has_value());
        // $timeunit returns module's time unit as power of 10
        int8_t unit = simulation_context.timescale
                          ? simulation_context.timescale->unit_power
                          : common::TimeScale::kDefaultUnitPower;
        auto result = RuntimeValue::IntegralSigned(unit, 32);
        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (instr.system_call_name == "$timeunit_root") {
        assert(instr.result.has_value());
        // $timeunit($root) returns global simulation precision
        int8_t unit = simulation_context.global_precision_power;
        auto result = RuntimeValue::IntegralSigned(unit, 32);
        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (instr.system_call_name == "$timeprecision") {
        assert(instr.result.has_value());
        // $timeprecision returns module's time precision as power of 10
        int8_t precision = simulation_context.timescale
                               ? simulation_context.timescale->precision_power
                               : common::TimeScale::kDefaultPrecisionPower;
        auto result = RuntimeValue::IntegralSigned(precision, 32);
        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (instr.system_call_name == "$timeprecision_root") {
        assert(instr.result.has_value());
        // $timeprecision($root) returns global simulation precision
        int8_t precision = simulation_context.global_precision_power;
        auto result = RuntimeValue::IntegralSigned(precision, 32);
        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (instr.system_call_name == "$printtimescale") {
        // $printtimescale() prints current module's timescale
        auto ts =
            simulation_context.timescale.value_or(common::TimeScale::Default());
        simulation_context.display_output
            << "Time scale of (" << simulation_context.module_name << ") is "
            << ts.ToString() << "\n";
        return InstructionResult::Continue();
      }

      if (instr.system_call_name == "$printtimescale_root") {
        // $printtimescale($root) prints global precision (unit = precision)
        auto gp = simulation_context.global_precision_power;
        auto unit_str = sdk::PowerToString(gp);
        simulation_context.display_output << "Time scale of ($root) is "
                                          << unit_str << " / " << unit_str
                                          << "\n";
        return InstructionResult::Continue();
      }

      // $signed, $unsigned: reinterpret signedness, preserving bit pattern
      if (instr.system_call_name == "$signed") {
        assert(instr.result.has_value());
        assert(instr.result_type.has_value());
        assert(instr.operands.size() == 1);
        const auto& src = get_operand_value(instr.operands[0]);
        // $signed: reinterpret bits as signed, preserving bit width
        auto target_data =
            std::get<common::IntegralData>(instr.result_type->data);
        auto result = RuntimeValue::IntegralSigned(
            ExtractInt64FromSource(src), target_data.bit_width);
        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      if (instr.system_call_name == "$unsigned") {
        assert(instr.result.has_value());
        assert(instr.result_type.has_value());
        assert(instr.operands.size() == 1);
        const auto& src = get_operand_value(instr.operands[0]);
        // $unsigned: reinterpret bits as unsigned, preserving bit width
        auto target_data =
            std::get<common::IntegralData>(instr.result_type->data);
        auto result = RuntimeValue::IntegralUnsigned(
            static_cast<uint64_t>(ExtractInt64FromSource(src)),
            target_data.bit_width);
        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      // $itor: convert integer to real
      if (instr.system_call_name == "$itor") {
        assert(instr.result.has_value());
        assert(!instr.operands.empty());
        const auto& src = get_operand_value(instr.operands[0]);
        auto src_data = std::get<common::IntegralData>(src.type.data);
        double real_value =
            src_data.is_signed
                ? static_cast<double>(ExtractInt64FromSource(src))
                : static_cast<double>(
                      static_cast<uint64_t>(ExtractInt64FromSource(src)));
        temp_table.Write(instr.result.value(), RuntimeValue::Real(real_value));
        return InstructionResult::Continue();
      }

      // $rtoi: convert real to integer by truncation toward zero
      if (instr.system_call_name == "$rtoi") {
        assert(instr.result.has_value());
        assert(instr.result_type.has_value());
        assert(!instr.operands.empty());
        const auto& src = get_operand_value(instr.operands[0]);
        auto target_data =
            std::get<common::IntegralData>(instr.result_type->data);
        auto raw_value = static_cast<int64_t>(src.AsDouble());
        auto result =
            target_data.is_signed
                ? RuntimeValue::IntegralSigned(raw_value, target_data.bit_width)
                : RuntimeValue::IntegralUnsigned(
                      static_cast<uint64_t>(raw_value), target_data.bit_width);
        temp_table.Write(instr.result.value(), result);
        return InstructionResult::Continue();
      }

      // $realtobits: real → 64-bit IEEE 754 representation
      if (instr.system_call_name == "$realtobits") {
        assert(instr.result.has_value());
        assert(!instr.operands.empty());
        const auto& src = get_operand_value(instr.operands[0]);
        auto bits = std::bit_cast<uint64_t>(src.AsDouble());
        temp_table.Write(
            instr.result.value(), RuntimeValue::IntegralUnsigned(bits, 64));
        return InstructionResult::Continue();
      }

      // $bitstoreal: 64-bit vector → real (IEEE 754)
      if (instr.system_call_name == "$bitstoreal") {
        assert(instr.result.has_value());
        assert(!instr.operands.empty());
        const auto& src = get_operand_value(instr.operands[0]);
        auto real_value = std::bit_cast<double>(src.AsNarrow().AsUInt64());
        temp_table.Write(instr.result.value(), RuntimeValue::Real(real_value));
        return InstructionResult::Continue();
      }

      // $shortrealtobits: shortreal → 32-bit IEEE 754 representation
      if (instr.system_call_name == "$shortrealtobits") {
        assert(instr.result.has_value());
        assert(!instr.operands.empty());
        const auto& src = get_operand_value(instr.operands[0]);
        auto bits = std::bit_cast<uint32_t>(src.AsFloat());
        temp_table.Write(
            instr.result.value(), RuntimeValue::IntegralUnsigned(bits, 32));
        return InstructionResult::Continue();
      }

      // $bitstoshortreal: 32-bit vector → shortreal (IEEE 754)
      if (instr.system_call_name == "$bitstoshortreal") {
        assert(instr.result.has_value());
        assert(!instr.operands.empty());
        const auto& src = get_operand_value(instr.operands[0]);
        auto bits = static_cast<uint32_t>(src.AsNarrow().AsUInt64());
        auto shortreal_value = std::bit_cast<float>(bits);
        temp_table.Write(
            instr.result.value(), RuntimeValue::ShortReal(shortreal_value));
        return InstructionResult::Continue();
      }

      // $clog2: ceiling of log base 2 (arg treated as unsigned, 0 → 0)
      if (instr.system_call_name == "$clog2") {
        assert(instr.result.has_value());
        assert(!instr.operands.empty());
        const auto& src = get_operand_value(instr.operands[0]);
        uint64_t n = src.AsNarrow().AsUInt64();
        int result = (n == 0) ? 0 : std::bit_width(n - 1);
        temp_table.Write(
            instr.result.value(), RuntimeValue::IntegralSigned(result, 32));
        return InstructionResult::Continue();
      }

      // Math functions: use category-based dispatch from registry
      const auto* func_info =
          common::FindSystemFunction(instr.system_call_name);
      if (func_info != nullptr) {
        using Category = common::SystemFunctionCategory;
        if (func_info->category == Category::kMathUnary) {
          assert(instr.result.has_value());
          assert(instr.operands.size() == 1);
          double arg = get_operand_value(instr.operands[0]).AsDouble();
          double result = ExecuteMathUnary(instr.system_call_name, arg);
          temp_table.Write(instr.result.value(), RuntimeValue::Real(result));
          return InstructionResult::Continue();
        }
        if (func_info->category == Category::kMathBinary) {
          assert(instr.result.has_value());
          assert(instr.operands.size() == 2);
          double a = get_operand_value(instr.operands[0]).AsDouble();
          double b = get_operand_value(instr.operands[1]).AsDouble();
          double result = ExecuteMathBinary(instr.system_call_name, a, b);
          temp_table.Write(instr.result.value(), RuntimeValue::Real(result));
          return InstructionResult::Continue();
        }
      }

      // Supported system calls are validated in AST→MIR
      assert(false && "unsupported system call should be rejected in AST→MIR");
    }

    case lir::InstructionKind::kMethodCall: {
      // Generic method call - currently handles enum methods (next, prev, name)
      // Future: string methods, class methods
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());
      assert(!instr.operands.empty());

      const auto& receiver = get_temp(instr.operands[0]);
      int64_t current_value = receiver.AsNarrow().AsInt64();
      const auto& members = instr.enum_members;

      // Find current position in enum member list
      size_t current_pos = 0;
      bool found = false;
      for (size_t i = 0; i < members.size(); ++i) {
        if (members[i].value == current_value) {
          current_pos = i;
          found = true;
          break;
        }
      }

      RuntimeValue result;
      if (instr.method_name == "next") {
        if (found) {
          // next(N): move forward N positions with wrap-around
          auto step = static_cast<size_t>(instr.method_step);
          size_t target_pos = (current_pos + step) % members.size();
          result = RuntimeValue::IntegralSigned(
              members[target_pos].value, instr.result_type->GetBitWidth());
        } else {
          // Invalid value: return 0 (implementation-defined per IEEE 1800-2023)
          result =
              RuntimeValue::IntegralSigned(0, instr.result_type->GetBitWidth());
        }
      } else if (instr.method_name == "prev") {
        if (found) {
          // prev(N): move backward N positions with wrap-around
          // Add members.size() before subtracting to avoid underflow
          size_t step = static_cast<size_t>(instr.method_step) % members.size();
          size_t target_pos =
              (current_pos + members.size() - step) % members.size();
          result = RuntimeValue::IntegralSigned(
              members[target_pos].value, instr.result_type->GetBitWidth());
        } else {
          // Invalid value: return 0 (implementation-defined per IEEE 1800-2023)
          result =
              RuntimeValue::IntegralSigned(0, instr.result_type->GetBitWidth());
        }
      } else if (instr.method_name == "name") {
        if (found) {
          result = RuntimeValue::String(members[current_pos].name);
        } else {
          // Invalid value: return empty string (implementation-defined)
          result = RuntimeValue::String("");
        }
      } else {
        assert(false && "unsupported method call");
      }

      temp_table.Write(instr.result.value(), result);
      return InstructionResult::Continue();
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
        if (condition.IsWide()) {
          condition_result = !condition.AsWideBit().IsZero();
        } else {
          condition_result = condition.AsNarrow().raw != 0;
        }
      }

      const auto& next_label = condition_result ? true_target : false_target;
      return InstructionResult::Jump(next_label);
    }
  }
}

}  // namespace lyra::interpreter
