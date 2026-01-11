#include "lyra/interpreter/instruction_runner.hpp"

#include <bit>
#include <cassert>
#include <cctype>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <iterator>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <fmt/core.h>
#include <fmt/format.h>

#include "lyra/common/bit_utils.hpp"
#include "lyra/common/diagnostic.hpp"
#include "lyra/common/format_string.hpp"
#include "lyra/common/internal_error.hpp"
#include "lyra/common/mem_io.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/time_format.hpp"
#include "lyra/common/type.hpp"
#include "lyra/common/wide_bit_ops.hpp"
#include "lyra/interpreter/builtin_ops.hpp"
#include "lyra/interpreter/call_frame.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_frame.hpp"
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

auto IsTruthy(const RuntimeValue& value) -> bool {
  if (value.IsReal()) {
    return value.AsDouble() != 0.0;
  }
  if (value.IsShortReal()) {
    return value.AsFloat() != 0.0F;
  }
  assert(value.IsTwoState());
  if (value.IsWide()) {
    return !value.AsWideBit().IsZero();
  }
  return value.AsNarrow().raw != 0;
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
  auto words_result =
      common::mem_io::ParseMemTokenToWords(token, bit_width, is_hex);
  if (!words_result) {
    throw std::runtime_error(words_result.error());
  }
  auto words = std::move(*words_result);

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

// Convert integral RuntimeValue to string per LRM 6.16.
// Each 8 bits forms one character, MSB first, null bytes are skipped.
auto IntegralToString(const RuntimeValue& val) -> std::string {
  std::string result;
  size_t width = std::get<common::IntegralData>(val.type.data).bit_width;

  if (val.IsWide()) {
    const auto& wide = val.AsWideBit();
    // Extract bytes from MSB to LSB
    for (size_t i = width; i >= 8; i -= 8) {
      size_t byte_start = i - 8;
      // Extract 8-bit value by reading bits
      uint8_t ch = 0;
      for (size_t b = 0; b < 8; ++b) {
        ch |= static_cast<uint8_t>(wide.GetBit(byte_start + b) << b);
      }
      if (ch != 0) {
        result += static_cast<char>(ch);
      }
    }
  } else {
    uint64_t bits = val.AsNarrow().AsUInt64();
    for (size_t i = width; i >= 8; i -= 8) {
      auto ch = static_cast<uint8_t>((bits >> (i - 8)) & 0xFF);
      if (ch != 0) {
        result += static_cast<char>(ch);
      }
    }
  }
  return result;
}

// Extract format string info from a RuntimeValue (first arg of $display etc).
// The is_string_literal flag comes from the instruction's
// first_operand_is_string_literal field, computed at MIR-to-LIR time.
auto ExtractFormatString(const RuntimeValue& val, bool is_string_literal)
    -> common::FormatStringInfo {
  common::FormatStringInfo info;
  if (val.IsString()) {
    info.text = val.AsString();
    info.is_string_literal = true;
  } else if (is_string_literal && val.IsTwoState()) {
    info.text = IntegralToString(val);
    info.is_string_literal = true;
  }
  info.has_format_specifiers =
      info.is_string_literal && info.text.find('%') != std::string::npos;
  return info;
}

// Get format specifier for a single value based on its type.
auto GetFormatSpecifier(const RuntimeValue& val, char default_format) -> char {
  if (val.IsString()) {
    return 's';
  }
  if (val.IsReal() || val.IsShortReal()) {
    return 'f';
  }
  return default_format;
}

// Build format string from a list of values using default format for integrals.
auto BuildFormatString(
    const std::vector<RuntimeValue>& values, char default_format)
    -> std::string {
  std::string fmt;
  for (const auto& val : values) {
    fmt += '%';
    fmt += GetFormatSpecifier(val, default_format);
  }
  return fmt;
}

// Compute the actual array index after adjusting for lower bound and checking
// bounds. Returns the adjusted index or throws DiagnosticException if out of
// bounds.
auto ComputeArrayIndex(const RuntimeValue& array_value, int64_t sv_index)
    -> size_t {
  // Dynamic arrays always have lower_bound 0
  int32_t lower_bound = 0;
  if (array_value.type.kind == common::Type::Kind::kUnpackedArray) {
    const auto& array_data =
        std::get<common::UnpackedArrayData>(array_value.type.data);
    lower_bound = array_data.lower_bound;
  }

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

// Handle kCall instruction: use resolved function pointer, prepare frame.
auto HandleCall(const lir::Instruction& instr, TempTable& temp_table)
    -> InstructionResult {
  const auto* func = instr.callee;
  if (func == nullptr) {
    throw common::InternalError(
        "kCall",
        fmt::format("function '{}' not resolved", instr.called_function_name));
  }

  // Create call frame (return address will be set by process_runner)
  auto frame = std::make_unique<CallFrame>();
  frame->function = func;
  frame->return_value_dest = instr.result;

  // Initialize parameters from arguments
  for (size_t i = 0; i < func->parameters.size(); ++i) {
    const auto& param = func->parameters[i];
    auto temp_ref = std::get<lir::TempRef>(instr.operands[i].value);
    RuntimeValue arg_value = temp_table.Read(temp_ref);
    frame->local_variables[param.variable.symbol] = std::move(arg_value);
  }

  // Initialize local variables with default values
  for (const auto& local : func->local_variables) {
    frame->local_variables[local.symbol] =
        RuntimeValue::DefaultValueForType(local.type);
  }

  return InstructionResult::CallFunction(func->entry_label, std::move(frame));
}

// Handle kReturn instruction: get return value, pop frame, store result.
auto HandleReturn(
    const lir::Instruction& instr, ProcessFrame& frame, TempTable& temp_table)
    -> InstructionResult {
  if (frame.call_stack.empty()) {
    throw common::InternalError("kReturn", "return outside of function");
  }

  // Get return value BEFORE popping (temp_table points to current frame)
  std::optional<RuntimeValue> return_value;
  if (!instr.operands.empty()) {
    auto temp_ref = std::get<lir::TempRef>(instr.operands[0].value);
    return_value = temp_table.Read(temp_ref);
  }

  // Pop the call frame
  CallFrame call_frame = std::move(frame.call_stack.back());
  frame.call_stack.pop_back();

  // Store return value in caller's temp table (after pop)
  if (return_value && call_frame.return_value_dest) {
    TempTable& caller_temp_table = frame.call_stack.empty()
                                       ? frame.temp_table
                                       : frame.call_stack.back().temp_table;
    caller_temp_table.Write(
        *call_frame.return_value_dest, std::move(*return_value));
  }

  return InstructionResult::ReturnFromFunction(
      call_frame.return_block_index, call_frame.return_instruction_index);
}

}  // namespace

// Execute a single instruction in the given context
auto RunInstruction(
    const lir::Instruction& instr, SimulationContext& simulation_context,
    ProcessFrame& frame, ProcessEffect& effect,
    const std::shared_ptr<InstanceContext>& instance_context)
    -> InstructionResult {
  // Use function-local temp table when inside a function, otherwise use
  // process temp table. This ensures recursive calls don't overwrite temps.
  auto& temp_table = frame.call_stack.empty()
                         ? frame.temp_table
                         : frame.call_stack.back().temp_table;
  auto& module_variable_table = simulation_context.variable_table;
  auto& process_variable_table = frame.variable_table;

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

    // Check function-local variables first (parameters and locals)
    if (!frame.call_stack.empty()) {
      auto& call_frame = frame.call_stack.back();
      auto it = call_frame.local_variables.find(symbol);
      if (it != call_frame.local_variables.end()) {
        return it->second;
      }
    }

    // Check process-local next
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

    // Deep copy arrays for value semantics (nested arrays must be independent)
    const RuntimeValue actual_value = value.DeepCopy();

    // Check function-local variables first (parameters and locals)
    if (!frame.call_stack.empty()) {
      auto& call_frame = frame.call_stack.back();
      auto it = call_frame.local_variables.find(symbol);
      if (it != call_frame.local_variables.end()) {
        it->second = actual_value;
        return;
      }
    }

    // Check process-local next
    if (process_variable_table.Exists(symbol)) {
      process_variable_table.Write(symbol, actual_value);
      return;
    }

    // Resolve through port bindings (output port → target signal/instance)
    auto [target_symbol, target_instance] = resolve_binding(symbol);

    // If bound (output port), write to target instance's storage
    if (target_instance != nullptr) {
      if (!is_non_blocking) {
        target_instance->Write(target_symbol, actual_value);
        effect.RecordVariableModification(target_symbol, target_instance);
      } else {
        effect.RecordNbaAction(
            {.variable = target_symbol,
             .value = actual_value,
             .instance = target_instance,
             .array_index = std::nullopt});
      }
      return;
    }

    // Otherwise, write to per-instance storage (local vars, input ports)
    if (instance_context != nullptr) {
      if (!is_non_blocking) {
        instance_context->Write(symbol, actual_value);
        effect.RecordVariableModification(symbol, instance_context);
      } else {
        effect.RecordNbaAction(
            {.variable = symbol,
             .value = actual_value,
             .instance = instance_context,
             .array_index = std::nullopt});
      }
      return;
    }

    // Fallback to global table (for backwards compat with non-hierarchical
    // code)
    if (!is_non_blocking) {
      module_variable_table.Write(symbol, actual_value);
      effect.RecordVariableModification(symbol);  // Global storage
    } else {
      effect.RecordNbaAction(
          {.variable = symbol,
           .value = actual_value,
           .instance = nullptr,
           .array_index = std::nullopt});
    }
  };

  // Store to hierarchical target: traverse instance path and store to target
  auto store_hierarchical = [&](const std::vector<common::SymbolRef>& instances,
                                common::SymbolRef target,
                                const RuntimeValue& value,
                                bool is_non_blocking) {
    // Deep copy arrays for value semantics (nested arrays must be independent)
    const RuntimeValue actual_value = value.DeepCopy();

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
      target_instance->Write(target, actual_value);
      effect.RecordVariableModification(target, target_instance);
    } else {
      effect.RecordNbaAction(
          {.variable = target,
           .value = actual_value,
           .instance = target_instance,
           .array_index = std::nullopt});
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
          "interpreter", std::format("{} expects 2-4 operands", task_name));
    }
    bool filename_is_string_literal = instr.first_operand_is_string_literal;
    const auto filename_value = get_operand_value(instr.operands[0]);
    std::string filename;
    if (filename_value.IsString()) {
      filename = filename_value.AsString();
    } else if (filename_is_string_literal && filename_value.IsTwoState()) {
      filename = IntegralToString(filename_value);
    } else {
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
            std::format("{} start address must be narrow integral", task_name));
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
          "interpreter",
          std::format("{} end address out of bounds", task_name));
    }

    auto path = common::mem_io::ResolveMemPath(filename);
    auto handle_read_mem = [&]() -> InstructionResult {
      std::ifstream in(path);
      if (!in) {
        throw common::InternalError(
            "interpreter",
            std::format("failed to open memory file: {}", path.string()));
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
        auto index = static_cast<size_t>(addr - min_addr);
        if (info.is_unpacked) {
          target_value.SetElement(index, value);
        } else {
          packed_value = StorePackedElement(
              packed_value, index, info.element_width, value);
        }
      };

      auto parse_result = common::mem_io::ParseMemFile(
          content, is_hex, min_addr, max_addr, current_addr, final_addr,
          task_name, [&](std::string_view token, int64_t addr) {
            auto value =
                ParseMemTokenToValue(token, info.element_width, is_hex);
            write_value(addr, value);
          });
      if (!parse_result.success) {
        throw std::runtime_error(parse_result.error);
      }

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
        auto index = static_cast<size_t>(addr - min_addr);
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

      const auto* symbol = std::get<lir::SymbolRef>(instr.operands[0].value);

      // Check if this is a local variable (no triggers needed)
      bool is_local = false;
      if (!frame.call_stack.empty()) {
        auto& call_frame = frame.call_stack.back();
        is_local = call_frame.local_variables.contains(symbol);
      }
      if (!is_local) {
        is_local = process_variable_table.Exists(symbol);
      }

      // RuntimeValue uses shared_ptr for array storage, so modifications
      // through this copy will affect the original in the variable table
      auto array_value = read_variable(instr.operands[0]);
      assert(array_value.IsArray());

      // For non-local variables, snapshot old value BEFORE modification.
      // This is critical: arrays use shared_ptr, so in-place modification
      // would make old and new values identical if we snapshot after.
      if (!is_local) {
        auto [target_symbol, target_instance] = resolve_binding(symbol);
        if (target_instance != nullptr) {
          target_instance->UpdatePrevious(target_symbol, array_value);
        } else if (instance_context != nullptr) {
          instance_context->UpdatePrevious(symbol, array_value);
        }
        // Note: global table case handled below
      }

      auto index_value = get_temp(instr.operands[1]);
      assert(!index_value.IsWide() && "array index cannot be wide");
      auto actual_idx =
          ComputeArrayIndex(array_value, index_value.AsNarrow().AsInt64());

      array_value.SetElement(actual_idx, get_temp(instr.operands[2]));

      // Record modification for trigger system
      if (!is_local) {
        auto [target_symbol, target_instance] = resolve_binding(symbol);
        if (target_instance != nullptr) {
          effect.RecordVariableModification(target_symbol, target_instance);
        } else if (instance_context != nullptr) {
          effect.RecordVariableModification(symbol, instance_context);
        } else {
          effect.RecordVariableModification(symbol);
        }
      }

      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreUnpackedElementNonBlocking: {
      // NBA to unpacked array element: array[index] <= value
      // Queue the write to be applied in NBA region
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsVariable());

      const auto* symbol = std::get<lir::SymbolRef>(instr.operands[0].value);
      auto array_value = read_variable(instr.operands[0]);
      assert(array_value.IsArray());

      auto index_value = get_temp(instr.operands[1]);
      assert(!index_value.IsWide() && "array index cannot be wide");
      auto actual_idx =
          ComputeArrayIndex(array_value, index_value.AsNarrow().AsInt64());

      auto element_value = get_temp(instr.operands[2]);

      // Resolve binding for port outputs
      auto [target_symbol, target_instance] = resolve_binding(symbol);

      // Queue NBA action with array index
      if (target_instance != nullptr) {
        effect.RecordNbaAction(
            {.variable = target_symbol,
             .value = element_value,
             .instance = target_instance,
             .array_index = actual_idx});
      } else if (instance_context != nullptr) {
        effect.RecordNbaAction(
            {.variable = symbol,
             .value = element_value,
             .instance = instance_context,
             .array_index = actual_idx});
      } else {
        effect.RecordNbaAction(
            {.variable = symbol,
             .value = element_value,
             .instance = nullptr,
             .array_index = actual_idx});
      }

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

    case lir::InstructionKind::kNewDynamicArray: {
      // Allocate/resize dynamic array: new[size] or new[size](init)
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());
      assert(!instr.operands.empty());

      auto size_val = get_operand_value(instr.operands[0]);
      auto size = static_cast<size_t>(size_val.AsNarrow().AsInt64());

      const auto& dyn_data =
          std::get<common::DynamicArrayData>(instr.result_type->data);
      const auto& elem_type = *dyn_data.element_type;

      std::vector<RuntimeValue> elements;
      elements.reserve(size);

      if (instr.operands.size() > 1) {
        // Resize with init: deep-copy elements for value semantics
        const auto& init = get_operand_value(instr.operands[1]);
        const auto& init_arr = init.AsArray();
        for (size_t i = 0; i < size; ++i) {
          if (i < init_arr.size()) {
            elements.push_back(init_arr[i].DeepCopy());
          } else {
            elements.push_back(RuntimeValue::DefaultValueForType(elem_type));
          }
        }
      } else {
        // Default init
        for (size_t i = 0; i < size; ++i) {
          elements.push_back(RuntimeValue::DefaultValueForType(elem_type));
        }
      }

      temp_table.Write(
          instr.result.value(),
          RuntimeValue::Array(*instr.result_type, std::move(elements)));
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kLoadPackedBits: {
      // Load bits from packed vector: result = value[bit_offset +: width]
      // operands[0] = value (packed vector)
      // operands[1] = bit_offset (pre-computed)
      // result_type contains the width
      assert(instr.operands.size() == 2);
      assert(instr.operands[0].IsTemp());
      assert(instr.operands[1].IsTemp());
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());

      auto value = get_temp(instr.operands[0]);
      auto offset_value = get_temp(instr.operands[1]);
      assert(!offset_value.IsWide() && "bit offset cannot be wide");
      auto bit_offset = static_cast<size_t>(offset_value.AsNarrow().AsInt64());

      const auto& result_type = instr.result_type.value();
      assert(result_type.kind == common::Type::Kind::kIntegral);
      auto result_data = std::get<common::IntegralData>(result_type.data);
      size_t width = result_data.bit_width;

      RuntimeValue result;
      if (width <= 64) {
        // Narrow - extract as uint64_t
        uint64_t mask = common::MakeBitMask(static_cast<uint32_t>(width));
        uint64_t extracted = 0;
        if (value.IsWide()) {
          auto shifted = value.AsWideBit().ShiftRightLogical(bit_offset);
          extracted = shifted.GetWord(0) & mask;
        } else {
          extracted = (value.AsNarrow().AsUInt64() >> bit_offset) & mask;
        }
        result = result_data.is_signed
                     ? RuntimeValue::IntegralSigned(
                           static_cast<int64_t>(extracted), width)
                     : RuntimeValue::IntegralUnsigned(extracted, width);
      } else {
        // Wide - extract as WideBit
        auto wide_value = value.IsWide() ? value.AsWideBit()
                                         : common::WideBit::FromUInt64(
                                               value.AsNarrow().AsUInt64(), 2);
        auto extracted = wide_value.ExtractSlice(bit_offset, width);
        result = RuntimeValue::IntegralWide(
            std::move(extracted), width, result_data.is_signed);
      }
      temp_table.Write(instr.result.value(), result);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStorePackedBits: {
      // Store bits to packed vector: variable[bit_offset +: width] = value
      // operands[0] = variable (packed vector)
      // operands[1] = bit_offset (pre-computed)
      // operands[2] = value to store
      // result_type contains slice width
      assert(instr.operands.size() == 3);
      assert(instr.operands[0].IsVariable());
      assert(instr.operands[1].IsTemp());
      assert(instr.operands[2].IsTemp());
      assert(instr.result_type.has_value());

      auto current = read_variable(instr.operands[0]);
      assert(current.IsTwoState());

      auto offset_value = get_temp(instr.operands[1]);
      assert(!offset_value.IsWide() && "bit offset cannot be wide");
      auto bit_offset = static_cast<size_t>(offset_value.AsNarrow().AsInt64());

      auto new_value = get_temp(instr.operands[2]);

      // Get slice width from result_type
      const auto& slice_type = instr.result_type.value();
      size_t slice_width = slice_type.GetBitWidth();

      // Get storage width and signedness
      const auto& current_data =
          std::get<common::IntegralData>(current.type.data);
      size_t storage_width = current_data.bit_width;
      bool storage_is_wide = current.IsWide();
      bool slice_is_wide = slice_width > 64;

      RuntimeValue result;
      if (!storage_is_wide && !slice_is_wide) {
        // Narrow storage, narrow slice - mask-and-merge
        uint64_t slice_mask =
            common::MakeBitMask(static_cast<uint32_t>(slice_width));
        uint64_t clear_mask = ~(slice_mask << bit_offset);
        uint64_t merged =
            (current.AsNarrow().AsUInt64() & clear_mask) |
            ((new_value.AsNarrow().AsUInt64() & slice_mask) << bit_offset);
        result = current_data.is_signed
                     ? RuntimeValue::IntegralSigned(
                           static_cast<int64_t>(merged), storage_width)
                     : RuntimeValue::IntegralUnsigned(merged, storage_width);
      } else {
        // Wide storage or wide slice - use WideBit InsertSlice
        size_t storage_words = common::wide_ops::WordsForBits(storage_width);
        auto current_wide =
            storage_is_wide ? current.AsWideBit()
                            : common::WideBit::FromUInt64(
                                  current.AsNarrow().AsUInt64(), storage_words);
        auto value_wide =
            slice_is_wide ? new_value.AsWideBit()
                          : common::WideBit::FromUInt64(
                                new_value.AsNarrow().AsUInt64(), storage_words);
        auto merged =
            current_wide.InsertSlice(value_wide, bit_offset, slice_width);
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

      // Helper to check if a type is integral-like (bitvector)
      auto is_integral_like = [](common::Type::Kind kind) {
        return kind == common::Type::Kind::kIntegral ||
               kind == common::Type::Kind::kPackedStruct;
      };

      if (is_integral_like(src.type.kind) &&
          is_integral_like(target_type.kind)) {
        // Get bit width and signedness for the target type
        size_t target_width = target_type.GetBitWidth();
        bool target_signed =
            target_type.IsPackedStruct()
                ? std::get<common::PackedStructData>(target_type.data).is_signed
                : std::get<common::IntegralData>(target_type.data).is_signed;

        // Get source signedness for sign extension
        bool src_signed =
            src.type.IsPackedStruct()
                ? std::get<common::PackedStructData>(src.type.data).is_signed
                : std::get<common::IntegralData>(src.type.data).is_signed;

        // Wide target type - create WideBit and sign-extend if needed
        if (target_width > 64) {
          common::WideBit wide = src.IsWide() ? src.AsWideBit() : [&]() {
            return CreateWideFromInt64(
                src.AsNarrow().AsInt64(), target_width, src_signed);
          }();

          RuntimeValue result = RuntimeValue::IntegralWide(
              std::move(wide), target_width, target_signed);
          temp_table.Write(instr.result.value(), result);
          return InstructionResult::Continue();
        }

        // Narrow target type - extract value (may be from wide source)
        int64_t raw_value = ExtractInt64FromSource(src);

        // Apply sign/bitwidth conversion
        RuntimeValue result =
            target_signed
                ? RuntimeValue::IntegralSigned(raw_value, target_width)
                : RuntimeValue::IntegralUnsigned(
                      static_cast<uint64_t>(raw_value), target_width);

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

      // Integral to string conversion (LRM 6.16)
      if (src.type.kind == common::Type::Kind::kIntegral &&
          target_type.kind == common::Type::Kind::kString) {
        temp_table.Write(
            instr.result.value(), RuntimeValue::String(IntegralToString(src)));
        return InstructionResult::Continue();
      }

      // Dynamic array to dynamic array: pass through unchanged
      // This handles cases where slang inserts a conversion expression for
      // type-compatible arrays (e.g., in new[size](init) expressions)
      if (src.type.IsDynamicArray() && target_type.IsDynamicArray()) {
        temp_table.Write(instr.result.value(), src);
        return InstructionResult::Continue();
      }

      throw DiagnosticException(
          Diagnostic::Error(
              {},
              fmt::format(
                  "conversion only supports two-state/real/shortreal/string "
                  "types, got: {} -> {}",
                  src.type, target_type)));
    }

    // Concatenation: result = {op0, op1, ...}
    // Operands are ordered MSB to LSB (first operand is most significant)
    case lir::InstructionKind::kConcatenation: {
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());

      const auto& result_type = instr.result_type.value();

      // String concatenation: collect strings and join
      if (result_type.kind == common::Type::Kind::kString) {
        std::string result;
        for (const auto& operand : instr.operands) {
          const auto& val = get_temp(operand);
          if (val.IsString()) {
            result += val.AsString();
          } else {
            // Integral operand - convert per LRM (8 bits = 1 char)
            result += IntegralToString(val);
          }
        }
        temp_table.Write(instr.result.value(), RuntimeValue::String(result));
        return InstructionResult::Continue();
      }

      // Integral concatenation: existing path
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

        // Extract format string info from first operand
        bool first_is_string_literal = instr.first_operand_is_string_literal;
        auto fmt_info = ExtractFormatString(
            get_operand_value(instr.operands[0]), first_is_string_literal);

        // Collect arguments (skip first if it was a format string/prefix)
        std::vector<RuntimeValue> args;
        size_t first_arg_idx = fmt_info.is_string_literal ? 1 : 0;
        for (size_t i = first_arg_idx; i < instr.operands.size(); ++i) {
          args.push_back(get_operand_value(instr.operands[i]));
        }

        // Print prefix (string literal without format specifiers)
        if (fmt_info.is_string_literal && !fmt_info.has_format_specifiers) {
          simulation_context.display_output << fmt_info.text;
        }

        // Format and print arguments
        std::string format_str =
            fmt_info.has_format_specifiers
                ? fmt_info.text
                : BuildFormatString(args, props.default_format);
        if (!args.empty()) {
          simulation_context.display_output
              << FormatDisplay(format_str, args, &time_ctx);
        }

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

        // Check if first operand is a string literal (from instruction
        // metadata)
        bool first_is_string_literal = instr.first_operand_is_string_literal;

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
                           arg_values = std::move(arg_values), time_ctx,
                           first_is_string_literal]() {
                  // Empty call - just print newline if needed
                  if (arg_values.empty()) {
                    if (props.append_newline) {
                      simulation_context.display_output << "\n";
                    }
                    return;
                  }

                  // Extract format string info from first argument
                  auto fmt_info = ExtractFormatString(
                      arg_values[0], first_is_string_literal);

                  // Collect arguments (skip first if it was a format
                  // string/prefix)
                  std::vector<RuntimeValue> args;
                  size_t first_arg_idx = fmt_info.is_string_literal ? 1 : 0;
                  for (size_t i = first_arg_idx; i < arg_values.size(); ++i) {
                    args.push_back(arg_values[i]);
                  }

                  // Print prefix (string literal without format specifiers)
                  if (fmt_info.is_string_literal &&
                      !fmt_info.has_format_specifiers) {
                    simulation_context.display_output << fmt_info.text;
                  }

                  // Format and print arguments
                  std::string format_str =
                      fmt_info.has_format_specifiers
                          ? fmt_info.text
                          : BuildFormatString(args, props.default_format);
                  if (!args.empty()) {
                    simulation_context.display_output
                        << FormatDisplay(format_str, args, &time_ctx);
                  }

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

        // Check if first operand is a string literal (from instruction
        // metadata)
        bool first_is_string_literal = instr.first_operand_is_string_literal;

        // Collect argument values
        std::vector<RuntimeValue> arg_values;
        for (const auto& operand : instr.operands) {
          arg_values.push_back(get_operand_value(operand));
        }

        // Extract format string info from first argument
        common::FormatStringInfo fmt_info;
        if (!arg_values.empty()) {
          fmt_info =
              ExtractFormatString(arg_values[0], first_is_string_literal);
        }

        // Collect format arguments (skip first if it was a format
        // string/prefix)
        std::vector<RuntimeValue> format_args;
        size_t first_arg_idx = fmt_info.is_string_literal ? 1 : 0;
        for (size_t i = first_arg_idx; i < arg_values.size(); ++i) {
          format_args.push_back(arg_values[i]);
        }

        // Build format string: prefix (if any) + format specifiers
        std::string format_string;
        if (fmt_info.has_format_specifiers) {
          format_string = fmt_info.text;
        } else {
          if (fmt_info.is_string_literal) {
            format_string = fmt_info.text;  // Use as prefix
          }
          format_string += BuildFormatString(format_args, props.default_format);
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

        // Set up monitor state as a closure with captured prev values.
        // This matches codegen's mutable lambda capture semantics.
        CallFrame closure;
        for (size_t i = 0; i < format_args.size(); ++i) {
          std::string capture_name = "__capture_prev_" + std::to_string(i);
          closure.captures[capture_name] = std::move(format_args[i]);
        }

        simulation_context.active_monitor = MonitorState{
            .enabled = true,
            .instance = instance_context,
            .check_process_name = instr.monitor_check_function_name,
            .closure = std::move(closure)};

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
      // Generic method call - handles enum methods (next, prev, name) and
      // array methods (size, delete).
      //
      // Note: Unlike codegen which uses registry-driven dispatch, we use string
      // comparison here. This is acceptable because: (1) only 5 methods total,
      // (2) string comparison is fast for short strings, (3) simpler than
      // pulling in registry dependency for runtime dispatch.
      assert(instr.result.has_value());
      assert(instr.result_type.has_value());
      assert(!instr.operands.empty());

      // Array methods have empty enum_members
      if (instr.enum_members.empty()) {
        // RuntimeValue uses shared_ptr for array storage, so modifications
        // through this copy will affect the original variable
        auto receiver = get_temp(instr.operands[0]);

        if (instr.method_name == "size") {
          auto size = static_cast<int32_t>(receiver.AsArray().size());
          temp_table.Write(
              instr.result.value(), RuntimeValue::IntegralSigned(size, 32));
        } else if (instr.method_name == "delete") {
          // Shared ptr semantics: clearing temp clears original variable
          receiver.AsArray().clear();
          // delete() returns void, write a dummy value
          temp_table.Write(
              instr.result.value(),
              RuntimeValue::DefaultValueForType(*instr.result_type));
        } else {
          assert(false && "unsupported array method call");
        }
        return InstructionResult::Continue();
      }

      // Enum methods
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

      return InstructionResult::Jump(
          IsTruthy(condition) ? true_target : false_target);
    }

    case lir::InstructionKind::kCall:
      return HandleCall(instr, temp_table);

    case lir::InstructionKind::kReturn:
      return HandleReturn(instr, frame, temp_table);

    case lir::InstructionKind::kLoadCapture: {
      // Load value from the closure's captures.
      // Used by closures (e.g., $monitor check processes) to load captured
      // values.
      if (!simulation_context.active_monitor.has_value()) {
        throw common::InternalError(
            "kLoadCapture", "no active monitor (closure context)");
      }
      auto& captures = simulation_context.active_monitor->closure.captures;

      auto it = captures.find(instr.capture_name);
      if (it == captures.end()) {
        throw common::InternalError(
            "kLoadCapture",
            std::format("capture '{}' not found", instr.capture_name));
      }

      temp_table.Write(instr.result.value(), it->second);
      return InstructionResult::Continue();
    }

    case lir::InstructionKind::kStoreCapture: {
      // Store value to the closure's captures.
      // Used by closures (e.g., $monitor check processes) to update captured
      // values.
      if (!simulation_context.active_monitor.has_value()) {
        throw common::InternalError(
            "kStoreCapture", "no active monitor (closure context)");
      }

      auto& captures = simulation_context.active_monitor->closure.captures;
      auto value = get_temp(instr.operands[0]);

      captures[instr.capture_name] = value;
      return InstructionResult::Continue();
    }
  }
}

}  // namespace lyra::interpreter
