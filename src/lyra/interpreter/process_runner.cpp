#include "lyra/interpreter/process_runner.hpp"

#include <cstddef>
#include <memory>

#include "lyra/common/internal_error.hpp"
#include "lyra/interpreter/basic_block_runner.hpp"
#include "lyra/interpreter/process_context.hpp"
#include "lyra/interpreter/process_effect.hpp"
#include "lyra/interpreter/process_result.hpp"
#include "lyra/interpreter/simulation_context.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::interpreter {

auto RunProcess(
    const std::shared_ptr<lir::Process>& process, std::size_t block_index,
    std::size_t instruction_index, const lir::Module& module,
    SimulationContext& simulation_context, ProcessContext& process_context,
    ProcessEffect& effect,
    const std::shared_ptr<InstanceContext>& instance_context) -> ProcessResult {
  for (const auto& variable : process->variables) {
    process_context.variable_table.InitializeVariable(variable);
  }

  // Track which blocks we're currently executing (process or function)
  const std::vector<std::unique_ptr<lir::BasicBlock>>* current_blocks =
      &process->blocks;

  while (true) {
    // get the basic block
    const auto& block = *(*current_blocks)[block_index];
    auto block_result = RunBlock(
        block, instruction_index, module, simulation_context, process_context,
        effect, instance_context);

    switch (block_result.kind) {
      case BasicBlockResult::Kind::kComplete:
        return ProcessResult::Complete();

      case BasicBlockResult::Kind::kWaitEvent:
        return ProcessResult::WaitEvent(
            block_result.triggers, block_index,
            block_result.resume_instruction_index);

      case BasicBlockResult::Kind::kDelay:
        // #0 delays go to Inactive region (same time slot)
        // Non-zero delays go to delay queue (future time slot)
        if (block_result.delay_amount == 0) {
          return ProcessResult::ScheduleInactive(
              block_index, block_result.resume_instruction_index);
        }
        return ProcessResult::Delay(
            block_result.delay_amount, block_index,
            block_result.resume_instruction_index);

      case BasicBlockResult::Kind::kFinish:
        return ProcessResult::Finish(block_result.is_stop);

      case BasicBlockResult::Kind::kJump: {
        // Jump within current context (process or function)
        std::optional<size_t> target_idx;
        const auto* current_func = process_context.CurrentFunction();
        if (current_func != nullptr) {
          target_idx =
              current_func->FindBlockIndexByLabel(block_result.target_label);
        } else {
          target_idx =
              process->FindBlockIndexByLabel(block_result.target_label);
        }
        if (!target_idx) {
          throw common::InternalError("RunProcess", "jump target not found");
        }
        block_index = *target_idx;
        instruction_index = 0;
        continue;
      }

      case BasicBlockResult::Kind::kCallFunction: {
        // Receive the call frame from instruction_runner and complete it
        if (!block_result.call_frame) {
          throw common::InternalError(
              "RunProcess", "call_frame is null in kCallFunction");
        }

        // Complete the frame with return address and push to stack
        auto& frame = *block_result.call_frame;
        frame.return_block_index = block_index;
        frame.return_instruction_index = block_result.resume_instruction_index;
        process_context.call_stack.push_back(std::move(frame));

        // CurrentFunction() now returns the function we're calling
        const auto* func = process_context.CurrentFunction();

        // Find the target block in the function
        auto target_idx =
            func->FindBlockIndexByLabel(block_result.target_label);
        if (!target_idx) {
          throw common::InternalError(
              "RunProcess", "function entry block not found");
        }

        // Switch to executing function blocks
        current_blocks = &func->blocks;
        block_index = *target_idx;
        instruction_index = 0;
        continue;
      }

      case BasicBlockResult::Kind::kReturnFromFunction: {
        // Switch back to caller's blocks (process or parent function).
        // CurrentFunction() reflects the caller after kReturn popped the frame.
        const auto* caller_func = process_context.CurrentFunction();
        if (caller_func == nullptr) {
          // Returning to process
          current_blocks = &process->blocks;
        } else {
          // Returning to parent function
          current_blocks = &caller_func->blocks;
        }

        block_index = block_result.return_block_index;
        instruction_index = block_result.return_instruction_index;
        continue;
      }
    }
  }
}

}  // namespace lyra::interpreter
