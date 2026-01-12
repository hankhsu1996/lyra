#include "lyra/interpreter/system_call_runner.hpp"

#include <cassert>
#include <memory>

#include "lyra/common/system_function.hpp"
#include "lyra/interpreter/instruction/context.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_frame.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/interpreter/system_call/control.hpp"
#include "lyra/interpreter/system_call/conversion.hpp"
#include "lyra/interpreter/system_call/display.hpp"
#include "lyra/interpreter/system_call/io.hpp"
#include "lyra/interpreter/system_call/math.hpp"
#include "lyra/interpreter/system_call/time.hpp"
#include "lyra/interpreter/temp_table.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

auto RunSystemCall(
    const lir::Instruction& instr, SimulationContext& simulation_context,
    ProcessFrame& frame, ProcessEffect& effect, TempTable& temp_table,
    const std::shared_ptr<HierarchyContext>& hierarchy_context)
    -> InstructionResult {
  // Create context that bundles all dependencies
  InstructionContext ctx(
      simulation_context, frame, effect, temp_table, hierarchy_context);

  // Look up the system function info
  const auto* func_info = common::FindSystemFunction(instr.system_call_name);

  // Dispatch based on category
  if (func_info != nullptr) {
    using Cat = common::SystemFunctionCategory;
    switch (func_info->category) {
      case Cat::kSimControl:
      case Cat::kSeverity:
      case Cat::kPlusargs:
        return HandleControlCalls(instr, ctx);

      case Cat::kDisplay:
        return HandleDisplayCalls(instr, ctx);

      case Cat::kTimeFormat:
      case Cat::kPrintTimescale:
      case Cat::kTimeQuery:
        return HandleTimeCalls(instr, ctx);

      case Cat::kTypeConversion:
      case Cat::kBitCast:
      case Cat::kMathIntegral:
        return HandleConversionCalls(instr, ctx);

      case Cat::kMathUnary:
      case Cat::kMathBinary:
        return HandleMathCalls(instr, ctx);

      case Cat::kMemIo:
        return HandleMemIoCalls(instr, ctx);
    }
  }

  // Supported system calls are validated in AST->MIR
  assert(false && "unsupported system call should be rejected in AST->MIR");
  return InstructionResult::Continue();
}

}  // namespace lyra::interpreter
