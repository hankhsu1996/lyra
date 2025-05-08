#include "lyra/interpreter/basic_block_runner.hpp"

namespace lyra::interpreter {

auto BasicBlockRunner::RunBlock(
    const lir::BasicBlock& block, std::size_t start_instruction_index,
    ProcessEffect& effect) -> BasicBlockResult {
  const auto& instructions = block.instructions;

  for (std::size_t i = start_instruction_index; i < instructions.size(); ++i) {
    const auto& instr = instructions[i];

    auto instruction_result = instruction_runner_.RunInstruction(instr, effect);

    switch (instruction_result.kind) {
      case InstructionResult::Kind::kComplete:
        return BasicBlockResult::Complete();
      case InstructionResult::Kind::kContinue:
        break;
      case InstructionResult::Kind::kWaitEvent:
        return BasicBlockResult::WaitEvent(instruction_result.triggers, i + 1);
      case InstructionResult::Kind::kDelay:
        return BasicBlockResult::Delay(instruction_result.delay_amount, i + 1);
      case InstructionResult::Kind::kFinish:
        return BasicBlockResult::Finish();
      case InstructionResult::Kind::kJump:
        return BasicBlockResult::Jump(instruction_result.target_label);
    }
  }
  throw std::runtime_error("Unreachable code in basic block runner");
}

}  // namespace lyra::interpreter
