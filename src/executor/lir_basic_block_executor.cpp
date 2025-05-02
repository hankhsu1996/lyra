#include "executor/lir_basic_block_executor.hpp"

namespace lyra {

LIRBasicBlockExecutor::LIRBasicBlockExecutor(ExecutionContext& context)
    : instruction_executor_(context), ctx_(context) {
}

auto LIRBasicBlockExecutor::RunBlock(
    const lir::BasicBlock& block, std::size_t start_instruction_index)
    -> LIRBasicBlockResult {
  const auto& instructions = block.instructions;

  // Track modified signals during execution
  std::vector<std::string> modified_signals;

  for (std::size_t i = start_instruction_index; i < instructions.size(); ++i) {
    const auto& instr = instructions[i];
    auto instruction_result = instruction_executor_.ExecuteInstruction(instr);

    // If a signal was modified, add it to our list
    if (instruction_result.modified_signal) {
      modified_signals.push_back(*instruction_result.modified_signal);
    }

    switch (instruction_result.kind) {
      case lir::LIRInstructionResult::Kind::kContinue:
        break;
      case lir::LIRInstructionResult::Kind::kDelay:
        return LIRBasicBlockResult::Delay(
            instruction_result.delay_amount, i + 1,
            std::move(modified_signals));
      case lir::LIRInstructionResult::Kind::kFinish:
        return LIRBasicBlockResult::Finish(std::move(modified_signals));
      case lir::LIRInstructionResult::Kind::kJump:
        return LIRBasicBlockResult::Jump(
            instruction_result.target_label, std::move(modified_signals));
    }
  }

  return LIRBasicBlockResult::Fallthrough(std::move(modified_signals));
}

}  // namespace lyra
