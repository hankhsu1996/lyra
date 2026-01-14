#include "lyra/interpreter/system_call/display.hpp"

#include <cassert>
#include <cstddef>
#include <string>
#include <utility>
#include <vector>

#include "lyra/common/display_variant.hpp"
#include "lyra/common/format_string.hpp"
#include "lyra/common/timescale.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/interpreter/system_call/format.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

auto HandleDisplayCalls(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult {
  auto& simulation_context = ctx.GetSimulationContext();

  // Handle file output variants ($fdisplay, $fwrite)
  if (instr.system_call_name == "$fdisplay" ||
      instr.system_call_name == "$fdisplayb" ||
      instr.system_call_name == "$fdisplayo" ||
      instr.system_call_name == "$fdisplayh" ||
      instr.system_call_name == "$fwrite" ||
      instr.system_call_name == "$fwriteb" ||
      instr.system_call_name == "$fwriteo" ||
      instr.system_call_name == "$fwriteh") {
    auto props = common::GetDisplayVariantProps(instr.system_call_name);

    // operands[0] = descriptor, operands[1..] = format args
    // format_operand = format string (if present)
    if (instr.operands.empty()) {
      return InstructionResult::Continue();  // Invalid - no descriptor
    }

    // Get descriptor from first operand
    auto descriptor_value = ctx.GetOperandValue(instr.operands[0]);
    auto descriptor =
        static_cast<uint32_t>(descriptor_value.AsNarrow().AsInt64());

    // Collect remaining arguments (prepend format_operand if present)
    std::vector<RuntimeValue> arg_values;
    if (instr.format_operand) {
      arg_values.push_back(ctx.GetOperandValue(*instr.format_operand));
    }
    for (size_t i = 1; i < instr.operands.size(); ++i) {
      arg_values.push_back(ctx.GetOperandValue(instr.operands[i]));
    }

    // Create time format context for %t specifier
    TimeFormatContext time_ctx{
        .time_format = simulation_context.time_format,
        .module_unit_power = simulation_context.timescale
                                 ? simulation_context.timescale->unit_power
                                 : common::TimeScale::kDefaultUnitPower,
        .global_precision_power = simulation_context.global_precision_power};

    // Format message
    std::string message = FormatMessage(
        arg_values, instr.format_string_is_literal,
        common::RadixToChar(props->radix), &time_ctx);

    if (props->append_newline) {
      message += "\n";
    }

    // Write to file(s) via descriptor
    simulation_context.file_manager.WriteToDescriptor(
        sdk::FileDescriptor{descriptor}, message,
        simulation_context.display_output);

    return InstructionResult::Continue();
  }

  // Handle file strobe variants ($fstrobe) - queued to Postponed region
  if (instr.system_call_name == "$fstrobe" ||
      instr.system_call_name == "$fstrobeb" ||
      instr.system_call_name == "$fstrobeo" ||
      instr.system_call_name == "$fstrobeh") {
    auto props = common::GetDisplayVariantProps(instr.system_call_name);
    bool first_is_string_literal = instr.format_string_is_literal;
    char default_format = common::RadixToChar(props->radix);
    bool append_newline = props->append_newline;

    // operands[0] = descriptor, operands[1..] = format args
    if (instr.operands.empty()) {
      return InstructionResult::Continue();  // Invalid - no descriptor
    }

    // Get descriptor from first operand (captured now, used in Postponed)
    auto descriptor_value = ctx.GetOperandValue(instr.operands[0]);
    auto descriptor =
        static_cast<uint32_t>(descriptor_value.AsNarrow().AsInt64());

    // Collect remaining arguments now (prepend format_operand if present)
    std::vector<RuntimeValue> arg_values;
    if (instr.format_operand) {
      arg_values.push_back(ctx.GetOperandValue(*instr.format_operand));
    }
    for (size_t i = 1; i < instr.operands.size(); ++i) {
      arg_values.push_back(ctx.GetOperandValue(instr.operands[i]));
    }

    // Create time format context
    TimeFormatContext time_ctx{
        .time_format = simulation_context.time_format,
        .module_unit_power = simulation_context.timescale
                                 ? simulation_context.timescale->unit_power
                                 : common::TimeScale::kDefaultUnitPower,
        .global_precision_power = simulation_context.global_precision_power};

    // Queue the output for Postponed region
    ctx.GetEffect().RecordPostponedAction(
        PostponedAction{
            .action = [&simulation_context, default_format, append_newline,
                       descriptor, arg_values = std::move(arg_values), time_ctx,
                       first_is_string_literal]() {
              std::string message = FormatMessage(
                  arg_values, first_is_string_literal, default_format,
                  &time_ctx);
              if (append_newline) {
                message += "\n";
              }
              simulation_context.file_manager.WriteToDescriptor(
                  sdk::FileDescriptor{descriptor}, message,
                  simulation_context.display_output);
            }});
    return InstructionResult::Continue();
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
    auto props = common::GetDisplayVariantProps(instr.system_call_name);

    // Collect argument values (prepend format_operand if present)
    std::vector<RuntimeValue> arg_values;
    if (instr.format_operand) {
      arg_values.push_back(ctx.GetOperandValue(*instr.format_operand));
    }
    for (const auto& operand : instr.operands) {
      arg_values.push_back(ctx.GetOperandValue(operand));
    }

    // Create time format context for %t specifier
    TimeFormatContext time_ctx{
        .time_format = simulation_context.time_format,
        .module_unit_power = simulation_context.timescale
                                 ? simulation_context.timescale->unit_power
                                 : common::TimeScale::kDefaultUnitPower,
        .global_precision_power = simulation_context.global_precision_power};

    // Format and output message
    std::string message = FormatMessage(
        arg_values, instr.format_string_is_literal,
        common::RadixToChar(props->radix), &time_ctx);
    simulation_context.display_output << message;

    if (props->append_newline) {
      simulation_context.display_output << "\n";
    }
    return InstructionResult::Continue();
  }

  // Handle $sformatf - pure function returning formatted string
  if (instr.system_call_name == "$sformatf") {
    auto props = common::GetDisplayVariantProps(instr.system_call_name);

    // Collect argument values (format_operand is format string, operands are
    // args)
    std::vector<RuntimeValue> arg_values;
    if (instr.format_operand) {
      arg_values.push_back(ctx.GetOperandValue(*instr.format_operand));
    }
    for (const auto& operand : instr.operands) {
      arg_values.push_back(ctx.GetOperandValue(operand));
    }

    // Create time format context for %t specifier
    TimeFormatContext time_ctx{
        .time_format = simulation_context.time_format,
        .module_unit_power = simulation_context.timescale
                                 ? simulation_context.timescale->unit_power
                                 : common::TimeScale::kDefaultUnitPower,
        .global_precision_power = simulation_context.global_precision_power};

    // Format message (no newline for $sformatf)
    std::string result = FormatMessage(
        arg_values, instr.format_string_is_literal,
        common::RadixToChar(props->radix), &time_ctx);

    // Store result in the temp
    assert(instr.result.has_value());
    ctx.GetTempTable().Write(
        instr.result.value(), RuntimeValue::String(result));
    return InstructionResult::Continue();
  }

  // Handle $sformat/$swrite* - effectful tasks that write to output variable
  if (instr.system_call_name == "$sformat" ||
      instr.system_call_name == "$swrite" ||
      instr.system_call_name == "$swriteb" ||
      instr.system_call_name == "$swriteo" ||
      instr.system_call_name == "$swriteh") {
    auto props = common::GetDisplayVariantProps(instr.system_call_name);

    // Collect argument values (format_operand + operands, NOT output_targets)
    std::vector<RuntimeValue> arg_values;
    if (instr.format_operand) {
      arg_values.push_back(ctx.GetOperandValue(*instr.format_operand));
    }
    for (const auto& operand : instr.operands) {
      arg_values.push_back(ctx.GetOperandValue(operand));
    }

    // Create time format context for %t specifier
    TimeFormatContext time_ctx{
        .time_format = simulation_context.time_format,
        .module_unit_power = simulation_context.timescale
                                 ? simulation_context.timescale->unit_power
                                 : common::TimeScale::kDefaultUnitPower,
        .global_precision_power = simulation_context.global_precision_power};

    // Format message (no newline for $sformat/$swrite)
    std::string result = FormatMessage(
        arg_values, instr.format_string_is_literal,
        common::RadixToChar(props->radix), &time_ctx);

    // Store result in output variable (overwrite, not append)
    ctx.StoreVariable(
        instr.output_targets[0], RuntimeValue::String(result), false);
    return InstructionResult::Continue();
  }

  // Handle strobe variants - same as display but queued to Postponed region
  if (instr.system_call_name == "$strobe" ||
      instr.system_call_name == "$strobeb" ||
      instr.system_call_name == "$strobeo" ||
      instr.system_call_name == "$strobeh") {
    auto props = common::GetDisplayVariantProps(instr.system_call_name);
    bool first_is_string_literal = instr.format_string_is_literal;
    char default_format = common::RadixToChar(props->radix);
    bool append_newline = props->append_newline;

    // Collect argument values now (prepend format_operand if present)
    std::vector<RuntimeValue> arg_values;
    if (instr.format_operand) {
      arg_values.push_back(ctx.GetOperandValue(*instr.format_operand));
    }
    for (const auto& operand : instr.operands) {
      arg_values.push_back(ctx.GetOperandValue(operand));
    }

    // Create time format context
    TimeFormatContext time_ctx{
        .time_format = simulation_context.time_format,
        .module_unit_power = simulation_context.timescale
                                 ? simulation_context.timescale->unit_power
                                 : common::TimeScale::kDefaultUnitPower,
        .global_precision_power = simulation_context.global_precision_power};

    // Queue the output for Postponed region
    ctx.GetEffect().RecordPostponedAction(
        PostponedAction{
            .action = [&simulation_context, default_format, append_newline,
                       arg_values = std::move(arg_values), time_ctx,
                       first_is_string_literal]() {
              std::string message = FormatMessage(
                  arg_values, first_is_string_literal, default_format,
                  &time_ctx);
              simulation_context.display_output << message;
              if (append_newline) {
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
    auto props = common::GetDisplayVariantProps(instr.system_call_name);

    // Check if format_operand is a string literal
    bool first_is_string_literal = instr.format_string_is_literal;

    // Collect argument values (prepend format_operand if present)
    std::vector<RuntimeValue> arg_values;
    if (instr.format_operand) {
      arg_values.push_back(ctx.GetOperandValue(*instr.format_operand));
    }
    for (const auto& operand : instr.operands) {
      arg_values.push_back(ctx.GetOperandValue(operand));
    }

    // Extract format string info from first argument
    common::FormatStringInfo fmt_info;
    if (!arg_values.empty()) {
      fmt_info = ExtractFormatString(arg_values[0], first_is_string_literal);
    }

    // Collect format arguments (skip first if it was a format string/prefix)
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
      format_string +=
          BuildFormatString(format_args, common::RadixToChar(props->radix));
    }

    // Get time format context
    auto module_unit_power = simulation_context.timescale
                                 ? simulation_context.timescale->unit_power
                                 : common::TimeScale::kDefaultUnitPower;
    TimeFormatContext time_ctx{
        .time_format = simulation_context.time_format,
        .module_unit_power = module_unit_power,
        .global_precision_power = simulation_context.global_precision_power};

    // Print immediately on first call (IEEE 1800 §21.2.3)
    simulation_context.display_output
        << FormatDisplay(format_string, format_args, &time_ctx);
    if (props->append_newline) {
      simulation_context.display_output << "\n";
    }

    // Set up monitor state with captured prev values.
    // This matches codegen's mutable lambda capture semantics.
    std::unordered_map<std::string, RuntimeValue> captures;
    for (size_t i = 0; i < format_args.size(); ++i) {
      std::string capture_name = "__capture_prev_" + std::to_string(i);
      captures[capture_name] = std::move(format_args[i]);
    }

    simulation_context.active_monitor = MonitorState{
        .enabled = true,
        .instance = ctx.GetHierarchyContext(),
        .check_process_name = instr.monitor_check_function_name,
        .captures = std::move(captures),
        .file_descriptor = std::nullopt};

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

  // Handle file monitor variants ($fmonitor)
  if (instr.system_call_name == "$fmonitor" ||
      instr.system_call_name == "$fmonitorb" ||
      instr.system_call_name == "$fmonitoro" ||
      instr.system_call_name == "$fmonitorh") {
    auto props = common::GetDisplayVariantProps(instr.system_call_name);

    // operands[0] = descriptor, operands[1..] = values to monitor
    if (instr.operands.empty()) {
      return InstructionResult::Continue();  // Invalid - no descriptor
    }

    // Get descriptor from first operand
    auto descriptor_value = ctx.GetOperandValue(instr.operands[0]);
    auto descriptor =
        static_cast<uint32_t>(descriptor_value.AsNarrow().AsInt64());

    bool first_is_string_literal = instr.format_string_is_literal;

    // Collect argument values (format_operand + operands[1..])
    std::vector<RuntimeValue> arg_values;
    if (instr.format_operand) {
      arg_values.push_back(ctx.GetOperandValue(*instr.format_operand));
    }
    for (size_t i = 1; i < instr.operands.size(); ++i) {
      arg_values.push_back(ctx.GetOperandValue(instr.operands[i]));
    }

    // Extract format string info from first argument
    common::FormatStringInfo fmt_info;
    if (!arg_values.empty()) {
      fmt_info = ExtractFormatString(arg_values[0], first_is_string_literal);
    }

    // Collect format arguments (skip first if it was a format string/prefix)
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
      format_string +=
          BuildFormatString(format_args, common::RadixToChar(props->radix));
    }

    // Get time format context
    auto module_unit_power = simulation_context.timescale
                                 ? simulation_context.timescale->unit_power
                                 : common::TimeScale::kDefaultUnitPower;
    TimeFormatContext time_ctx{
        .time_format = simulation_context.time_format,
        .module_unit_power = module_unit_power,
        .global_precision_power = simulation_context.global_precision_power};

    // Print immediately on first call (IEEE 1800-2023 21.2.3)
    std::string initial_message =
        FormatDisplay(format_string, format_args, &time_ctx);
    if (props->append_newline) {
      initial_message += "\n";
    }
    simulation_context.file_manager.WriteToDescriptor(
        sdk::FileDescriptor{descriptor}, initial_message,
        simulation_context.display_output);

    // Set up monitor state with captured prev values
    std::unordered_map<std::string, RuntimeValue> captures;
    for (size_t i = 0; i < format_args.size(); ++i) {
      std::string capture_name = "__capture_prev_" + std::to_string(i);
      captures[capture_name] = std::move(format_args[i]);
    }
    // Store file descriptor for synthesized check process (LoadCapture)
    captures["__file_descriptor"] =
        RuntimeValue::IntegralUnsigned(descriptor, 32);

    simulation_context.active_monitor = MonitorState{
        .enabled = true,
        .instance = ctx.GetHierarchyContext(),
        .check_process_name = instr.monitor_check_function_name,
        .captures = std::move(captures),
        .file_descriptor = descriptor};

    return InstructionResult::Continue();
  }

  std::unreachable();
}

}  // namespace lyra::interpreter
