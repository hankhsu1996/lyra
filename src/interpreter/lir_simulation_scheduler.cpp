#include "interpreter/lir_simulation_scheduler.hpp"

#include <cstdint>
#include <unordered_set>

#include <fmt/core.h>

#include "core/runtime_value.hpp"

namespace lyra::interpreter {

LIRSimulationScheduler::LIRSimulationScheduler(
    const lir::Module& module, ExecutionContext& context)
    : module_(module), context_(context), process_interpreter_(context) {
}

auto LIRSimulationScheduler::Run() -> uint64_t {
  // Initialize variables before starting simulation
  InitializeVariables();

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

void LIRSimulationScheduler::InitializeVariables() {
  for (const auto& variable : module_.get().variables) {
    context_.get().variable_table.InitializeVariable(
        variable.name, variable.type);
  }
}

void LIRSimulationScheduler::ScheduleInitialProcesses() {
  for (const auto& process : module_.get().processes) {
    if (process->kind == lir::ProcessKind::kInitial) {
      active_queue_.push({process, 0, 0});
    }
  }
}

void LIRSimulationScheduler::ScheduleAlwaysProcesses() {
  for (const auto& process : module_.get().processes) {
    if (process->kind == lir::ProcessKind::kAlways) {
      active_queue_.push({process, 0, 0});
    }
  }
}

void LIRSimulationScheduler::ExecuteOneEvent() {
  ScheduledEvent event = active_queue_.front();
  active_queue_.pop();

  auto& process = event.process;
  std::size_t block_index = event.block_index;
  std::size_t instruction_index = event.instruction_index;

  auto result =
      process_interpreter_.RunProcess(process, block_index, instruction_index);

  WakeWaitingProcesses(result.modified_variables);

  switch (result.kind) {
    case LIRProcessResult::Kind::kDelay: {
      DelayedEvent delayed_event{
          .ready_time = simulation_time_ + result.delay_amount,
          .process = process,
          .block_index = result.block_index,
          .instruction_index = result.resume_instruction_index};
      delay_queue_.push(delayed_event);
      break;
    }

    case LIRProcessResult::Kind::kWaitEvent: {
      for (const auto& trigger : result.triggers) {
        wait_map_[trigger.variable].insert(process);
        wait_set_[process] = {
            .block_index = result.block_index,
            .instruction_index = result.resume_instruction_index,
            .edge_kind = trigger.edge_kind};
      }
      break;
    }
    case LIRProcessResult::Kind::kFinish: {
      finish_requested_ = true;
      break;
    }

    case LIRProcessResult::Kind::kComplete: {
      break;
    }
  }
}

void LIRSimulationScheduler::WakeWaitingProcesses(
    const std::vector<std::string>& modified_variables) {
  std::unordered_set<std::shared_ptr<lir::Process>> resumed;

  for (const auto& variable : modified_variables) {
    RuntimeValue old_value =
        context_.get().variable_table.ReadPrevious(variable);
    RuntimeValue new_value = context_.get().variable_table.Read(variable);

    auto map_it = wait_map_.find(variable);
    if (map_it == wait_map_.end()) {
      continue;
    }

    std::unordered_set<std::shared_ptr<lir::Process>> to_remove;

    for (const auto& process : map_it->second) {
      if (resumed.contains(process)) {
        continue;
      }

      auto info_it = wait_set_.find(process);
      if (info_it == wait_set_.end()) {
        continue;  // Should not happen, but safety check
      }

      const auto& wait_info = info_it->second;
      bool should_trigger = false;

      switch (wait_info.edge_kind) {
        case common::EdgeKind::kAnyChange:
          should_trigger = (old_value != new_value);
          break;

        case common::EdgeKind::kPosedge:
        case common::EdgeKind::kNegedge:
        case common::EdgeKind::kBothEdge: {
          assert(old_value.IsTwoState() && new_value.IsTwoState());

          int64_t old_int = old_value.AsInt64();
          int64_t new_int = new_value.AsInt64();

          switch (wait_info.edge_kind) {
            case common::EdgeKind::kPosedge:
              should_trigger = (old_int == 0 && new_int == 1);
              break;
            case common::EdgeKind::kNegedge:
              should_trigger = (old_int == 1 && new_int == 0);
              break;
            case common::EdgeKind::kBothEdge:
              should_trigger = (old_int == 0 && new_int == 1) ||
                               (old_int == 1 && new_int == 0);
              break;
            default:
              break;
          }
          break;
        }
      }

      if (should_trigger) {
        active_queue_.push(ScheduledEvent{
            .process = process,
            .block_index = wait_info.block_index,
            .instruction_index = wait_info.instruction_index});
        resumed.insert(process);
        to_remove.insert(process);
        wait_set_.erase(process);  // Cleanup from wait_set_
      }
    }

    for (const auto& proc : to_remove) {
      map_it->second.erase(proc);
    }

    if (map_it->second.empty()) {
      wait_map_.erase(map_it);
    }

    context_.get().variable_table.UpdatePrevious(variable, new_value);
  }
}

}  // namespace lyra::interpreter
