#include "lyra/interpreter/process_runner.hpp"

#include "lyra/interpreter/process_effect.hpp"

namespace lyra::interpreter {

auto ProcessRunner::RunProcess(
    const std::shared_ptr<lir::Process>& process, std::size_t block_index,
    std::size_t instruction_index, ProcessEffect& effect) -> ProcessResult {
  while (true) {
    // get the basic block
    const auto& block = *process->blocks[block_index];
    auto block_result =
        block_runner_.RunBlock(block, instruction_index, effect);

    switch (block_result.kind) {
      case BasicBlockResult::Kind::kComplete:
        return ProcessResult::Complete();

      case BasicBlockResult::Kind::kWaitEvent:
        return ProcessResult::WaitEvent(
            block_result.triggers, block_index,
            block_result.resume_instruction_index);

      case BasicBlockResult::Kind::kDelay:
        // Store where to resume (current block, instruction index from result)
        return ProcessResult::Delay(
            block_result.delay_amount, block_index,
            block_result.resume_instruction_index);

      case BasicBlockResult::Kind::kFinish:
        return ProcessResult::Finish();

      case BasicBlockResult::Kind::kJump:
        block_index = process->FindBlockIndexByLabel(block_result.target_label);
        instruction_index = 0;
        continue;
    }
  }
}

}  // namespace lyra::interpreter
