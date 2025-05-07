#include "interpreter/simulation_runner.hpp"

#include <cstdint>

#include <fmt/core.h>

namespace lyra::interpreter {

SimulationRunner::SimulationRunner(
    const lir::Module& module, ExecutionContext& context)
    : module_(module),
      context_(context),
      process_runner_(context),
      trigger_manager_(context) {
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

  context_.get().tracer.AddEvent(
      {.name = process->name,
       .time = simulation_time_,
       .detail = fmt::format(
           "Start at block {} instruction {}", block_index,
           instruction_index)});

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
        trigger_manager_.RegisterWaitingProcess(
            process, trigger.variable, trigger.edge_kind, result.block_index,
            result.resume_instruction_index);
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
  // Delegate to trigger manager
  auto events_to_schedule = trigger_manager_.CheckTriggers(modified_variables);

  // Schedule all triggered events
  for (const auto& event : events_to_schedule) {
    active_queue_.push(event);
  }
}

}  // namespace lyra::interpreter
