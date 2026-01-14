#include "lyra/interpreter/system_call/file_io.hpp"

#include <cassert>
#include <cstdint>
#include <string>

#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/interpreter/system_call/format.hpp"
#include "lyra/interpreter/temp_table.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

namespace {

/// Extract filename string from operand value.
auto GetFilenameString(const RuntimeValue& value, bool is_literal)
    -> std::string {
  if (value.IsString()) {
    return value.AsString();
  }
  if (is_literal && value.IsTwoState()) {
    return IntegralToString(value);
  }
  throw std::runtime_error("$fopen filename must be a string");
}

}  // namespace

auto HandleFileIoCalls(const lir::Instruction& instr, InstructionContext& ctx)
    -> InstructionResult {
  auto& sim = ctx.GetSimulationContext();

  if (instr.system_call_name == "$fopen") {
    assert(instr.result.has_value());

    // Get filename
    auto filename_value = ctx.GetOperandValue(instr.operands[0]);
    std::string filename =
        GetFilenameString(filename_value, instr.format_string_is_literal);

    sdk::FileDescriptor fd = sdk::FileDescriptor::Invalid();
    if (instr.operands.size() == 1) {
      // MCD mode
      fd = sim.file_manager.FopenMcd(filename);
    } else {
      // FD mode with explicit mode string
      auto mode_value = ctx.GetOperandValue(instr.operands[1]);
      std::string mode =
          GetFilenameString(mode_value, instr.format_string_is_literal);
      fd = sim.file_manager.FopenFd(filename, mode);
    }

    // Write result (int32)
    ctx.GetTempTable().Write(
        instr.result.value(), RuntimeValue::IntegralSigned(fd.ToInt32(), 32));
    return InstructionResult::Continue();
  }

  if (instr.system_call_name == "$fclose") {
    assert(!instr.result.has_value());

    // Get descriptor
    auto desc_value = ctx.GetOperandValue(instr.operands[0]);
    auto descriptor = static_cast<uint32_t>(desc_value.AsNarrow().AsInt64());
    sdk::FileDescriptor fd{descriptor};

    sim.file_manager.Fclose(fd);

    // Cancel $fmonitor if its file descriptor matches (IEEE 1800-2023 21.3.1)
    if (sim.active_monitor &&
        sim.active_monitor->file_descriptor == descriptor) {
      sim.active_monitor.reset();
    }

    return InstructionResult::Continue();
  }

  // Should not reach here - validated in AST->MIR
  assert(false && "unsupported file I/O system call");
  return InstructionResult::Continue();
}

}  // namespace lyra::interpreter
