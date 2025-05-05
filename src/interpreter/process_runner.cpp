#include "interpreter/process_runner.hpp"

namespace lyra::interpreter {

ProcessRunner::ProcessRunner(ExecutionContext& context)
    : block_runner_(context), ctx_(context) {
}

auto ProcessRunner::RunProcess(
    const std::shared_ptr<lir::Process>& process, std::size_t block_index,
    std::size_t instruction_index) -> ProcessResult {
  // Track modified variables during execution
  std::vector<std::string> modified_variables;

  while (true) {
    // get the basic block
    const auto& block = *process->blocks[block_index];
    auto block_result = block_runner_.RunBlock(block, instruction_index);

    // Aggregate any modified variables
    modified_variables.insert(
        modified_variables.end(), block_result.modified_variables.begin(),
        block_result.modified_variables.end());

    switch (block_result.kind) {
      case BasicBlockResult::Kind::kFallthrough:
        // Move to the next block if available, otherwise we're done with the
        // process
        if (block_index + 1 < process->blocks.size()) {
          ++block_index;
          instruction_index = 0;
          continue;
        }
        return ProcessResult::Complete(std::move(modified_variables));

      case BasicBlockResult::Kind::kWaitEvent:
        return ProcessResult::WaitEvent(
            block_result.triggers, block_index,
            block_result.resume_instruction_index,
            std::move(modified_variables));

      case BasicBlockResult::Kind::kDelay:
        // Store where to resume (current block, instruction index from result)
        return ProcessResult::Delay(
            block_result.delay_amount, block_index,
            block_result.resume_instruction_index,
            std::move(modified_variables));

      case BasicBlockResult::Kind::kFinish:
        return ProcessResult::Finish(std::move(modified_variables));

      case BasicBlockResult::Kind::kJump:
        block_index = process->FindBlockIndexByLabel(block_result.target_label);
        instruction_index = 0;
        continue;
    }
  }
}

}  // namespace lyra::interpreter
