#include "interpreter/lir_basic_block_executor.hpp"

namespace lyra::interpreter {

LIRBasicBlockExecutor::LIRBasicBlockExecutor(ExecutionContext& context)
    : instruction_executor_(context), ctx_(context) {
}

auto LIRBasicBlockExecutor::RunBlock(
    const lir::BasicBlock& block, std::size_t start_instruction_index)
    -> LIRBasicBlockResult {
  const auto& instructions = block.instructions;

  // Track modified variables during execution
  std::vector<std::string> modified_variables;

  for (std::size_t i = start_instruction_index; i < instructions.size(); ++i) {
    const auto& instr = instructions[i];
    auto instruction_result = instruction_executor_.ExecuteInstruction(instr);

    // If a variable was modified, add it to our list
    if (instruction_result.modified_variable) {
      modified_variables.push_back(*instruction_result.modified_variable);
    }

    switch (instruction_result.kind) {
      case LIRInstructionResult::Kind::kContinue:
        break;
      case LIRInstructionResult::Kind::kDelay:
        return LIRBasicBlockResult::Delay(
            instruction_result.delay_amount, i + 1,
            std::move(modified_variables));
      case LIRInstructionResult::Kind::kFinish:
        return LIRBasicBlockResult::Finish(std::move(modified_variables));
      case LIRInstructionResult::Kind::kJump:
        return LIRBasicBlockResult::Jump(
            instruction_result.target_label, std::move(modified_variables));
    }
  }

  return LIRBasicBlockResult::Fallthrough(std::move(modified_variables));
}

}  // namespace lyra::interpreter
