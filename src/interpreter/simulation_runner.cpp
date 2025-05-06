#include "interpreter/simulation_runner.hpp"

#include <cstdint>
#include <unordered_set>

#include <fmt/core.h>

#include "runtime/runtime_value.hpp"

namespace lyra::interpreter {

SimulationRunner::SimulationRunner(
    const lir::Module& module, ExecutionContext& context)
    : module_(module), context_(context), process_runner_(context) {
}

auto SimulationRunner::Run() -> uint64_t {
  // Initialize simulation state
  InitializeVariables();
  ScheduleInitialProcesses();
  ScheduleAlwaysProcesses();

  // Main simulation loop: continue until all queues are empty
  while (!active_queue_.empty() || !delay_queue_.empty()) {
    // Advance simulation time if no active events
    if (active_queue_.empty()) {
      auto it = delay_queue_.begin();
      simulation_time_ = it->first;

      // Move delayed events scheduled for this time into the active queue
      for (const auto& event : it->second) {
        active_queue_.push(event);
      }
      delay_queue_.erase(it);
    }

    // Run active queue
    while (!active_queue_.empty()) {
      ExecuteOneEvent();
    }

    // Run inactive queue (handle #0 delays)
    while (!inactive_queue_.empty()) {
      std::queue<ScheduledEvent> current;
      std::swap(current, inactive_queue_);

      while (!current.empty()) {
        active_queue_.push(current.front());
        current.pop();
      }

      while (!active_queue_.empty()) {
        ExecuteOneEvent();
      }
    }

    // Execute all non-blocking assignments
    while (!nba_queue_.empty()) {
      const auto& action = nba_queue_.front();
      context_.get().variable_table.Write(action.variable, action.value);
      nba_queue_.pop();
    }

    // Execute all postponed actions (e.g., $strobe, $monitor)
    while (!postponed_queue_.empty()) {
      const auto& action = postponed_queue_.front();
      action.action();
      postponed_queue_.pop();
    }

    // Stop simulation if $finish was requested
    if (finish_requested_) {
      break;
    }
  }

  return simulation_time_;
}

void SimulationRunner::InitializeVariables() {
  for (const auto& variable : module_.get().variables) {
    context_.get().variable_table.InitializeVariable(
        variable.name, variable.type);
  }
}

void SimulationRunner::ScheduleInitialProcesses() {
  for (const auto& process : module_.get().processes) {
    if (process->kind == lir::ProcessKind::kInitial) {
      active_queue_.push({process, 0, 0});
    }
  }
}

void SimulationRunner::ScheduleAlwaysProcesses() {
  for (const auto& process : module_.get().processes) {
    if (process->kind == lir::ProcessKind::kAlways) {
      active_queue_.push({process, 0, 0});
    }
  }
}

void SimulationRunner::ExecuteOneEvent() {
  ScheduledEvent event = active_queue_.front();
  active_queue_.pop();

  auto& process = event.process;
  std::size_t block_index = event.block_index;
  std::size_t instruction_index = event.instruction_index;

  auto result =
      process_runner_.RunProcess(process, block_index, instruction_index);

  WakeWaitingProcesses(result.modified_variables);

  for (const auto& action : result.nba_actions) {
    nba_queue_.push(action);
  }
  for (const auto& action : result.postponed_actions) {
    postponed_queue_.push(action);
  }

  switch (result.kind) {
    case ProcessResult::Kind::kDelay: {
      SimulationTime delay_time = simulation_time_ + result.delay_amount;
      ScheduledEvent event{
          .process = process,
          .block_index = result.block_index,
          .instruction_index = result.resume_instruction_index};
      delay_queue_[delay_time].push_back(std::move(event));
      break;
    }

    case ProcessResult::Kind::kWaitEvent: {
      for (const auto& trigger : result.triggers) {
        wait_map_[trigger.variable].insert(process);
        wait_set_[process] = {
            .block_index = result.block_index,
            .instruction_index = result.resume_instruction_index,
            .edge_kind = trigger.edge_kind};
      }
      break;
    }
    case ProcessResult::Kind::kFinish: {
      finish_requested_ = true;
      break;
    }

    case ProcessResult::Kind::kComplete: {
      break;
    }
  }
}

void SimulationRunner::WakeWaitingProcesses(
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
