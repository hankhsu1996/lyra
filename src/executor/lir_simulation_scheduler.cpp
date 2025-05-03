#include "executor/lir_simulation_scheduler.hpp"

#include <cstdint>

#include <fmt/core.h>

namespace lyra {

LIRSimulationScheduler::LIRSimulationScheduler(
    const lir::Module& module, ExecutionContext& context,
    VariableTriggerMap variable_triggers)
    : module_(module),
      context_(context),
      process_interpreter_(context),
      variable_to_triggers_(std::move(variable_triggers)) {
}

auto LIRSimulationScheduler::Run() -> uint64_t {
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

  // Process any variable triggers from this execution
  ProcessVariableTriggers(result.modified_variables);

  if (result.kind == LIRProcessResult::Kind::kDelay) {
    DelayedEvent delayed_event{
        .ready_time = simulation_time_ + result.delay_amount,
        .process = process,
        .block_index = result.block_index,
        .instruction_index = result.resume_instruction_index};
    delay_queue_.push(delayed_event);
    return;
  }

  if (result.kind == LIRProcessResult::Kind::kFinish) {
    finish_requested_ = true;
    return;
  }
}

void LIRSimulationScheduler::ProcessVariableTriggers(
    const std::vector<std::string>& modified_variables) {
  for (const auto& variable_name : modified_variables) {
    auto it = variable_to_triggers_.find(variable_name);
    if (it == variable_to_triggers_.end()) {
      continue;  // No triggers for this variable
    }

    // Get the current and previous values for edge detection
    RuntimeValue old_val =
        context_.get().variable_table.ReadPrevious(variable_name);
    RuntimeValue new_val = context_.get().variable_table.Read(variable_name);

    int64_t old_int = old_val.AsInt();
    int64_t new_int = new_val.AsInt();

    // Check each trigger condition
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

    // Update the previous value for future edge detection
    context_.get().variable_table.UpdatePrevious(variable_name, new_val);
  }
}

}  // namespace lyra
