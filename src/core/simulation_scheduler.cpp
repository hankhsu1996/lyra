#include "core/simulation_scheduler.hpp"

#include <cstdint>

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
      // No ready processes, advance simulation time
      const auto& next_event = delay_queue_.top();
      simulation_time_ = next_event.ready_time;
      active_queue_.push({next_event.process, next_event.program_counter});
      delay_queue_.pop();
    }

    if (!active_queue_.empty()) {
      ExecuteOneEvent();
    }
  }

  return simulation_time_;
}

void SimulationScheduler::ScheduleInitialProcesses() {
  for (const auto& process : module_.get().processes) {
    if (process->kind == lir::ProcessKind::kInitial) {
      active_queue_.push({process, 0});
    }
  }
}

void SimulationScheduler::ScheduleAlwaysProcesses() {
  for (const auto& process : module_.get().processes) {
    if (process->kind == lir::ProcessKind::kAlways) {
      active_queue_.push({process, 0});
    }
  }
}

void SimulationScheduler::ExecuteOneEvent() {
  ScheduledEvent event = active_queue_.front();
  active_queue_.pop();

  auto& process = event.process;
  size_t program_counter = event.program_counter;

  const auto& instructions = process->instructions;

  while (program_counter < instructions.size()) {
    const auto& instr = instructions[program_counter];
    auto result = executor_.ExecuteInstruction(instr);

    ++program_counter;

    if (result.action == lir::ExecuteResult::Action::kDelay) {
      uint64_t resume_time = simulation_time_ + result.delay_amount;
      delay_queue_.push({resume_time, process, program_counter});
      return;  // Pause current process until delay expires
    }

    if (instr.kind == lir::InstructionKind::kStoreSignal) {
      const auto& destination_signal =
          std::get<std::string>(instr.operands[0].data);

      RuntimeValue old_value =
          context_.get().signal_table.ReadPrevious(destination_signal);
      RuntimeValue new_value =
          context_.get().signal_table.Read(destination_signal);

      int64_t old_int = old_value.AsInt();
      int64_t new_int = new_value.AsInt();

      auto it = variable_to_triggers_.find(destination_signal);
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
            active_queue_.push({triggered_process, 0});
          }
        }
      }

      context_.get().signal_table.UpdatePrevious(destination_signal, new_value);
    }
  }
}

}  // namespace lyra
