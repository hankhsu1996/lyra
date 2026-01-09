#include "lyra/interpreter/basic_block_runner.hpp"

#include <cstddef>

#include "lyra/common/diagnostic.hpp"
#include "lyra/interpreter/basic_block_result.hpp"
#include "lyra/interpreter/instruction_result.hpp"
#include "lyra/interpreter/instruction_runner.hpp"
#include "lyra/interpreter/process_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/basic_block.hpp"

namespace lyra::interpreter {

using ResultKind = InstructionResult::Kind;

auto RunBlock(
    const lir::BasicBlock& block, std::size_t start_instruction_index,
    const lir::Module& module, SimulationContext& simulation_context,
    ProcessContext& process_context, ProcessEffect& effect,
    const std::shared_ptr<InstanceContext>& instance_context)
    -> BasicBlockResult {
  const auto& instructions = block.instructions;

  for (std::size_t i = start_instruction_index; i < instructions.size(); ++i) {
    const auto& instr = instructions[i];

    auto instruction_result = RunInstruction(
        instr, module, simulation_context, process_context, effect,
        instance_context);

    switch (instruction_result.kind) {
      case ResultKind::kComplete:
        return BasicBlockResult::Complete();
      case ResultKind::kContinue:
        break;
      case ResultKind::kWaitEvent:
        return BasicBlockResult::WaitEvent(instruction_result.triggers, i + 1);
      case ResultKind::kDelay:
        return BasicBlockResult::Delay(instruction_result.delay_amount, i + 1);
      case ResultKind::kFinish:
        return BasicBlockResult::Finish(instruction_result.is_stop);
      case ResultKind::kJump:
        return BasicBlockResult::Jump(instruction_result.target_label);
      case ResultKind::kCallFunction:
        return BasicBlockResult::CallFunction(
            instruction_result.target_label, i + 1,
            std::move(instruction_result.call_frame));
      case ResultKind::kReturnFromFunction:
        return BasicBlockResult::ReturnFromFunction(
            instruction_result.return_block_index,
            instruction_result.return_instruction_index);
    }
  }
  throw DiagnosticException(
      Diagnostic::Error({}, "unreachable code in basic block runner"));
}

}  // namespace lyra::interpreter
