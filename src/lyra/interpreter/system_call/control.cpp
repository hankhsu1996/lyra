#include "lyra/interpreter/system_call/control.hpp"

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <string>
#include <utility>
#include <vector>

#include "lyra/common/timescale.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/interpreter/system_call/format.hpp"
#include "lyra/interpreter/temp_table.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/sdk/plusargs.hpp"

namespace lyra::interpreter {

auto HandleControlCalls(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult {
  auto& simulation_context = ctx.GetSimulationContext();

  // Simulation control tasks: $finish, $stop, $exit
  if (instr.system_call_name == "$finish" ||
      instr.system_call_name == "$stop" || instr.system_call_name == "$exit") {
    bool is_stop = (instr.system_call_name == "$stop");

    // Get diagnostic level: 0 = nothing, 1 = time, 2 = time + stats
    // $exit has no argument, treat as level 1
    // $finish and $stop default to 1 if no argument (handled in lowering)
    int level = 1;
    if (!instr.operands.empty()) {
      // Diagnostic level is never wide
      level = static_cast<int>(
          ctx.GetOperandValue(instr.operands[0]).AsNarrow().AsUInt64());
    }

    // Print diagnostics based on level (VCS style)
    if (level >= 1) {
      simulation_context.display_output
          << instr.system_call_name << " called at time "
          << simulation_context.current_time << "\n";
    }

    return InstructionResult::Finish(is_stop);
  }

  // Severity tasks: $fatal, $error, $warning, $info
  if (instr.system_call_name == "$fatal" ||
      instr.system_call_name == "$error" ||
      instr.system_call_name == "$warning" ||
      instr.system_call_name == "$info") {
    // Determine severity string
    std::string severity;
    if (instr.system_call_name == "$fatal") {
      severity = "FATAL";
    } else if (instr.system_call_name == "$error") {
      severity = "ERROR";
    } else if (instr.system_call_name == "$warning") {
      severity = "WARNING";
    } else {
      severity = "INFO";
    }

    // Parse arguments based on task type
    size_t arg_idx = 0;
    int finish_num = 1;  // Default for $fatal

    if (instr.system_call_name == "$fatal" && !instr.operands.empty()) {
      finish_num = static_cast<int>(
          ctx.GetOperandValue(instr.operands[0]).AsNarrow().AsUInt64());
      arg_idx = 1;
    }

    // Get file and line from instruction metadata
    std::string file_str = instr.source_file.value_or("");
    int line_num = static_cast<int>(instr.source_line.value_or(0));

    // Get hierarchy path for error reporting
    std::string scope = ctx.GetHierarchyContext()->hierarchy_path;

    // Build message from remaining arguments (if any)
    std::string message;
    bool has_format = instr.format_operand.has_value();
    if (has_format || arg_idx < instr.operands.size()) {
      // Collect message arguments (prepend format_operand if present)
      std::vector<RuntimeValue> msg_args;
      if (instr.format_operand) {
        msg_args.push_back(ctx.GetOperandValue(*instr.format_operand));
      }
      for (size_t i = arg_idx; i < instr.operands.size(); ++i) {
        msg_args.push_back(ctx.GetOperandValue(instr.operands[i]));
      }

      TimeFormatContext time_ctx{
          .time_format = simulation_context.time_format,
          .module_unit_power = simulation_context.timescale
                                   ? simulation_context.timescale->unit_power
                                   : common::TimeScale::kDefaultUnitPower,
          .global_precision_power = simulation_context.global_precision_power};

      message = FormatMessage(
          msg_args, instr.format_string_is_literal, 'd', &time_ctx);
    }

    // Only print if finish_num >= 1 for $fatal (or always for others)
    bool should_print = instr.system_call_name != "$fatal" || finish_num >= 1;

    if (should_print) {
      simulation_context.display_output << severity << ": " << file_str << ":"
                                        << line_num << ": " << scope << " @ "
                                        << simulation_context.current_time;
      if (!message.empty()) {
        simulation_context.display_output << ": " << message;
      }
      simulation_context.display_output << "\n";
    }

    // Only $fatal terminates
    if (instr.system_call_name == "$fatal") {
      return InstructionResult::Finish(true);  // is_stop = true for error exit
    }
    return InstructionResult::Continue();
  }

  // Plusargs handling
  if (instr.system_call_name == "$test$plusargs") {
    assert(instr.result.has_value());
    assert(instr.operands.size() == 1);

    sdk::PlusargsQuery query(simulation_context.plusargs);
    const auto& query_val = ctx.GetOperandValue(instr.operands[0]);
    std::string query_str = query_val.IsString() ? query_val.AsString()
                                                 : IntegralToString(query_val);
    int32_t result = query.TestPlusargs(query_str);
    ctx.GetTempTable().Write(
        instr.result.value(), RuntimeValue::IntegralSigned(result, 32));
    return InstructionResult::Continue();
  }

  if (instr.system_call_name == "$value$plusargs") {
    assert(instr.result.has_value());
    assert(instr.operands.size() == 1);
    assert(instr.output_targets.size() == 1);

    sdk::PlusargsQuery query(simulation_context.plusargs);
    const auto& format_val = ctx.GetOperandValue(instr.operands[0]);
    std::string format = format_val.IsString() ? format_val.AsString()
                                               : IntegralToString(format_val);

    // Determine format specifier to select appropriate query
    auto pos = format.find('%');
    char spec = (pos != std::string::npos && pos + 1 < format.size())
                    ? format[pos + 1]
                    : '\0';
    // Handle %0d -> %d
    if (spec == '0' && pos + 2 < format.size()) {
      spec = format[pos + 2];
    }

    int32_t matched = 0;
    if (spec == 'd' || spec == 'D') {
      auto result = query.ValuePlusargsInt(format);
      if (result.matched) {
        matched = 1;
        auto target_operand = lir::Operand::Variable(instr.output_targets[0]);
        ctx.StoreVariable(
            target_operand, RuntimeValue::IntegralSigned(result.value, 32),
            false);
      }
    } else if (spec == 's' || spec == 'S') {
      auto result = query.ValuePlusargsString(format);
      if (result.matched) {
        matched = 1;
        auto target_operand = lir::Operand::Variable(instr.output_targets[0]);
        ctx.StoreVariable(
            target_operand, RuntimeValue::String(result.value), false);
      }
    }

    ctx.GetTempTable().Write(
        instr.result.value(), RuntimeValue::IntegralSigned(matched, 32));
    return InstructionResult::Continue();
  }

  std::unreachable();
}

}  // namespace lyra::interpreter
