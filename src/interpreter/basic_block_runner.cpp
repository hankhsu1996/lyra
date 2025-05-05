#include "interpreter/basic_block_runner.hpp"

namespace lyra::interpreter {

BasicBlockRunner::BasicBlockRunner(ExecutionContext& context)
    : instruction_runner_(context), ctx_(context) {
}

auto BasicBlockRunner::RunBlock(
    const lir::BasicBlock& block, std::size_t start_instruction_index)
    -> BasicBlockResult {
  const auto& instructions = block.instructions;

  // Track modified variables during execution
  std::vector<std::string> modified_variables;

  for (std::size_t i = start_instruction_index; i < instructions.size(); ++i) {
    const auto& instr = instructions[i];
    auto instruction_result = instruction_runner_.ExecuteInstruction(instr);

    // If a variable was modified, add it to our list
    if (instruction_result.modified_variable) {
      modified_variables.push_back(*instruction_result.modified_variable);
    }

    switch (instruction_result.kind) {
      case InstructionResult::Kind::kContinue:
        break;
      case InstructionResult::Kind::kWaitEvent:
        return BasicBlockResult::WaitEvent(
            instruction_result.triggers, i + 1, std::move(modified_variables));
      case InstructionResult::Kind::kDelay:
        return BasicBlockResult::Delay(
            instruction_result.delay_amount, i + 1,
            std::move(modified_variables));
      case InstructionResult::Kind::kFinish:
        return BasicBlockResult::Finish(std::move(modified_variables));
      case InstructionResult::Kind::kJump:
        return BasicBlockResult::Jump(
            instruction_result.target_label, std::move(modified_variables));
    }
  }

  return BasicBlockResult::Fallthrough(std::move(modified_variables));
}

}  // namespace lyra::interpreter
