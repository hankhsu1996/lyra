#include "core/simulation_scheduler.hpp"

#include <cstdint>

#include <fmt/core.h>

namespace lyra {

SimulationScheduler::SimulationScheduler(
    const lir::Module& module, ExecutionContext& context,
    VariableTriggerMap variable_triggers)
    : module_(module),
      context_(context),
      executor_(module, context),
      variable_to_triggers_(std::move(variable_triggers)) {
}

auto SimulationScheduler::Run() -> uint64_t {
  ScheduleInitialProcesses();
  ScheduleAlwaysProcesses();

  while (!active_queue_.empty() || !delay_queue_.empty()) {
    if (active_queue_.empty() && !delay_queue_.empty()) {
      const auto& next_event = delay_queue_.top();
      simulation_time_ = next_event.ready_time;
      active_queue_.push(
          {next_event.process, next_event.block_index,
           next_event.instruction_index});
      delay_queue_.pop();
    }

    if (!active_queue_.empty()) {
      ExecuteOneEvent();
    }

    if (finish_requested_ && active_queue_.empty() && delay_queue_.empty()) {
      break;
    }
  }

  return simulation_time_;
}

void SimulationScheduler::ScheduleInitialProcesses() {
  for (const auto& process : module_.get().processes) {
    if (process->kind == lir::ProcessKind::kInitial) {
      active_queue_.push({process, 0, 0});
    }
  }
}

void SimulationScheduler::ScheduleAlwaysProcesses() {
  for (const auto& process : module_.get().processes) {
    if (process->kind == lir::ProcessKind::kAlways) {
      active_queue_.push({process, 0, 0});
    }
  }
}

void SimulationScheduler::ExecuteOneEvent() {
  ScheduledEvent event = active_queue_.front();
  active_queue_.pop();

  auto& process = event.process;
  size_t block_index = event.block_index;
  size_t instruction_index = event.instruction_index;

  while (true) {
    const auto& block = *process->blocks[block_index];
    const auto& instructions = block.instructions;

    while (instruction_index < instructions.size()) {
      const auto& instr = instructions[instruction_index];
      auto result = executor_.ExecuteInstruction(instr);
      ++instruction_index;

      if (result.action == lir::ExecuteResult::Action::kDelay) {
        delay_queue_.push(
            {simulation_time_ + result.delay_amount, process, block_index,
             instruction_index});
        return;
      }

      if (result.action == lir::ExecuteResult::Action::kFinish) {
        finish_requested_ = true;
        return;
      }

      if (result.action == lir::ExecuteResult::Action::kJump) {
        block_index = process->FindBlockIndexByLabel(result.target_label);
        instruction_index = 0;
        break;
      }

      if (instr.kind == lir::InstructionKind::kStoreVariable) {
        const auto& dst = std::get<std::string>(instr.operands[0].data);

        RuntimeValue old_val = context_.get().variable_table.ReadPrevious(dst);
        RuntimeValue new_val = context_.get().variable_table.Read(dst);

        int64_t old_int = old_val.AsInt();
        int64_t new_int = new_val.AsInt();

        auto it = variable_to_triggers_.find(dst);
        if (it != variable_to_triggers_.end()) {
          for (const auto& [trigger, triggered_process] : it->second) {
            bool should_trigger = false;
            switch (trigger.edge_kind) {
              case common::EdgeKind::kAnyEdge:
                should_trigger = (old_int != new_int);
                break;
              case common::EdgeKind::kPosedge:
                should_trigger = (old_int == 0 && new_int == 1);
                break;
              case common::EdgeKind::kNegedge:
                should_trigger = (old_int == 1 && new_int == 0);
                break;
            }

            if (should_trigger) {
              active_queue_.push({triggered_process, 0, 0});
            }
          }
        }

        context_.get().variable_table.UpdatePrevious(dst, new_val);
      }
    }

    return;  // no jump occurred, natural termination
  }
}

}  // namespace lyra
