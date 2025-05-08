#include "lyra/interpreter/process_runner.hpp"

namespace lyra::interpreter {

ProcessRunner::ProcessRunner(ExecutionContext& context)
    : block_runner_(context), context_(context) {
}

auto ProcessRunner::RunProcess(
    const std::shared_ptr<lir::Process>& process, std::size_t block_index,
    std::size_t instruction_index) -> ProcessResult {
  // Track modified variables during execution
  std::vector<std::string> modified_variables;
  std::vector<NbaAction> nba_actions;
  std::vector<PostponedAction> postponed_actions;

  while (true) {
    // get the basic block
    const auto& block = *process->blocks[block_index];
    auto block_result = block_runner_.RunBlock(block, instruction_index);

    // Aggregate any modified variables
    modified_variables.insert(
        modified_variables.end(), block_result.modified_variables.begin(),
        block_result.modified_variables.end());
    nba_actions.insert(
        nba_actions.end(), block_result.nba_actions.begin(),
        block_result.nba_actions.end());
    postponed_actions.insert(
        postponed_actions.end(), block_result.postponed_actions.begin(),
        block_result.postponed_actions.end());

    switch (block_result.kind) {
      case BasicBlockResult::Kind::kComplete:
        return ProcessResult::Complete(
            std::move(modified_variables), std::move(nba_actions),
            std::move(postponed_actions));

      case BasicBlockResult::Kind::kWaitEvent:
        return ProcessResult::WaitEvent(
            block_result.triggers, block_index,
            block_result.resume_instruction_index,
            std::move(modified_variables), std::move(nba_actions),
            std::move(postponed_actions));

      case BasicBlockResult::Kind::kDelay:
        // Store where to resume (current block, instruction index from result)
        return ProcessResult::Delay(
            block_result.delay_amount, block_index,
            block_result.resume_instruction_index,
            std::move(modified_variables), std::move(nba_actions),
            std::move(postponed_actions));

      case BasicBlockResult::Kind::kFinish:
        return ProcessResult::Finish(
            std::move(modified_variables), std::move(nba_actions),
            std::move(postponed_actions));

      case BasicBlockResult::Kind::kJump:
        block_index = process->FindBlockIndexByLabel(block_result.target_label);
        instruction_index = 0;
        continue;
    }
  }
}

}  // namespace lyra::interpreter
