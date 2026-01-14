#include "lyra/interpreter/system_call/time.hpp"

#include <cassert>
#include <cstdint>
#include <utility>

#include "lyra/common/timescale.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/interpreter/temp_table.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/sdk/time_utils.hpp"

namespace lyra::interpreter {

auto HandleTimeCalls(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult {
  auto& simulation_context = ctx.GetSimulationContext();

  // Helper: scale raw simulation time to module's timeunit per LRM
  auto scale_time = [&simulation_context]() -> uint64_t {
    uint64_t raw_time = simulation_context.current_time;
    if (!simulation_context.timescale) {
      return raw_time;
    }
    uint64_t divisor = simulation_context.timescale->TimeDivisor(
        simulation_context.global_precision_power);
    return raw_time / divisor;
  };

  // $time: returns 64-bit unsigned time in module's timeunit
  if (instr.system_call_name == "$time") {
    assert(instr.result.has_value());
    auto result = RuntimeValue::IntegralUnsigned(scale_time(), 64);
    ctx.GetTempTable().Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  // $stime: returns low 32 bits of scaled time as unsigned
  if (instr.system_call_name == "$stime") {
    assert(instr.result.has_value());
    auto result = RuntimeValue::IntegralUnsigned(scale_time() & 0xFFFFFFFF, 32);
    ctx.GetTempTable().Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  // $realtime: returns scaled time as real (double)
  if (instr.system_call_name == "$realtime") {
    assert(instr.result.has_value());
    auto scaled_time = static_cast<double>(simulation_context.current_time);
    if (simulation_context.timescale) {
      auto divisor =
          static_cast<double>(simulation_context.timescale->TimeDivisor(
              simulation_context.global_precision_power));
      scaled_time /= divisor;
    }
    auto result = RuntimeValue::Real(scaled_time);
    ctx.GetTempTable().Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  // $timeformat: configure time formatting for %t
  if (instr.system_call_name == "$timeformat") {
    auto& tf = simulation_context.time_format;

    if (!instr.operands.empty()) {
      tf.units = static_cast<int8_t>(
          ctx.GetOperandValue(instr.operands[0]).AsNarrow().AsInt64());
    }
    if (instr.operands.size() >= 2) {
      tf.precision = static_cast<int>(
          ctx.GetOperandValue(instr.operands[1]).AsNarrow().AsInt64());
    }
    if (instr.operands.size() >= 3) {
      tf.suffix = ctx.GetOperandValue(instr.operands[2]).AsString();
    }
    if (instr.operands.size() >= 4) {
      tf.min_width = static_cast<int>(
          ctx.GetOperandValue(instr.operands[3]).AsNarrow().AsInt64());
    }

    return InstructionResult::Continue();
  }

  // $timeunit: returns module's time unit as power of 10
  if (instr.system_call_name == "$timeunit") {
    assert(instr.result.has_value());
    int8_t unit = simulation_context.timescale
                      ? simulation_context.timescale->unit_power
                      : common::TimeScale::kDefaultUnitPower;
    auto result = RuntimeValue::IntegralSigned(unit, 32);
    ctx.GetTempTable().Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  // $timeunit_root: returns global simulation precision
  if (instr.system_call_name == "$timeunit_root") {
    assert(instr.result.has_value());
    int8_t unit = simulation_context.global_precision_power;
    auto result = RuntimeValue::IntegralSigned(unit, 32);
    ctx.GetTempTable().Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  // $timeprecision: returns module's time precision as power of 10
  if (instr.system_call_name == "$timeprecision") {
    assert(instr.result.has_value());
    int8_t precision = simulation_context.timescale
                           ? simulation_context.timescale->precision_power
                           : common::TimeScale::kDefaultPrecisionPower;
    auto result = RuntimeValue::IntegralSigned(precision, 32);
    ctx.GetTempTable().Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  // $timeprecision_root: returns global simulation precision
  if (instr.system_call_name == "$timeprecision_root") {
    assert(instr.result.has_value());
    int8_t precision = simulation_context.global_precision_power;
    auto result = RuntimeValue::IntegralSigned(precision, 32);
    ctx.GetTempTable().Write(instr.result.value(), result);
    return InstructionResult::Continue();
  }

  // $printtimescale: prints current module's timescale
  if (instr.system_call_name == "$printtimescale") {
    auto ts =
        simulation_context.timescale.value_or(common::TimeScale::Default());
    simulation_context.display_output
        << "Time scale of (" << simulation_context.module_name << ") is "
        << ts.ToString() << "\n";
    return InstructionResult::Continue();
  }

  // $printtimescale_root: prints global precision (unit = precision)
  if (instr.system_call_name == "$printtimescale_root") {
    auto gp = simulation_context.global_precision_power;
    auto unit_str = sdk::PowerToString(gp);
    simulation_context.display_output << "Time scale of ($root) is " << unit_str
                                      << " / " << unit_str << "\n";
    return InstructionResult::Continue();
  }

  std::unreachable();
}

}  // namespace lyra::interpreter
