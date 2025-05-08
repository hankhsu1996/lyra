#include "lyra/interpreter/simulation_runner.hpp"

#include <fmt/core.h>

namespace lyra::interpreter {

SimulationRunner::SimulationRunner(
    const lir::Module& module, SimulationContext& context)
    : module_(module),
      context_(context),
      process_runner_(context),
      trigger_manager_(context) {
}

void SimulationRunner::Run() {
  // Initialize simulation state
  InitializeVariables();
  ScheduleInitialProcesses();
  ScheduleAlwaysProcesses();

  // Main simulation loop: continue until all queues are empty
  while (!active_queue_.empty() || !delay_queue_.empty()) {
    // Advance simulation time if no active events
    if (active_queue_.empty()) {
      auto it = delay_queue_.begin();
      context_.get().current_time = it->first;

      if (context_.get().current_time > kMaxSimulationTime) {
        throw std::runtime_error("Simulation time exceeded max limit");
      }

      // Move delayed events scheduled for this time into the active queue
      for (const auto& event : it->second) {
        active_queue_.push(event);
      }
      delay_queue_.erase(it);
    }

    ExecuteTimeSlot();

    // Stop simulation if $finish was requested
    if (finish_requested_) {
      break;
    }
  }
}

void SimulationRunner::ExecuteTimeSlot() {
  ExecuteRegion(RegionType::kPreponed);
  ExecuteRegion(RegionType::kPreActive);

  while (HasPendingActivity()) {
    // Phase 1: Active phase group
    while (HasActivityInActiveGroup()) {
      ExecuteRegion(RegionType::kActive);

      if (!inactive_queue_.empty()) {
        MoveToActive(inactive_queue_);
      } else if (!nba_queue_.empty()) {
        ExecuteRegion(RegionType::kNba);
        // NBA commits might trigger more Active events
      }
    }

    // Phase 2: Reactive phase group
    while (HasActivityInReactiveGroup()) {
      ExecuteRegion(RegionType::kReactive);

      if (!inactive_queue_.empty()) {
        MoveToActive(inactive_queue_);
      }
    }

    // Phase 3: Pre-Postponed region (optional)
    if (IsAllRegionEmpty()) {
      ExecuteRegion(RegionType::kPrePostponed);
    }
  }

  // Final Phase: Postponed
  ExecuteRegion(RegionType::kPostponed);
}

auto SimulationRunner::HasPendingActivity() const -> bool {
  return !active_queue_.empty() || !inactive_queue_.empty() ||
         !nba_queue_.empty();
}

auto SimulationRunner::HasActivityInActiveGroup() const -> bool {
  return !active_queue_.empty() || !inactive_queue_.empty() ||
         !nba_queue_.empty();
}

auto SimulationRunner::HasActivityInReactiveGroup() const -> bool {
  return !active_queue_.empty() || !inactive_queue_.empty();
}

auto SimulationRunner::IsAllRegionEmpty() const -> bool {
  return active_queue_.empty() && inactive_queue_.empty() && nba_queue_.empty();
}

void SimulationRunner::MoveToActive(std::queue<ScheduledEvent>& source) {
  std::queue<ScheduledEvent> tmp;
  std::swap(tmp, source);
  while (!tmp.empty()) {
    active_queue_.push(tmp.front());
    tmp.pop();
  }
}

void SimulationRunner::ExecuteRegion(RegionType region) {
  switch (region) {
    case RegionType::kActive:
      while (!active_queue_.empty()) {
        ExecuteOneEvent();
      }
      break;

    case RegionType::kReactive:
      while (!active_queue_.empty()) {
        ExecuteOneEvent();
      }
      break;

    case RegionType::kNba: {
      std::vector<std::string> modified_variables;

      while (!nba_queue_.empty()) {
        const auto& action = nba_queue_.front();
        context_.get().variable_table.Write(action.variable, action.value);
        modified_variables.push_back(action.variable);
        nba_queue_.pop();
      }

      WakeWaitingProcesses(modified_variables);
      break;
    }

    case RegionType::kPostponed:
      while (!postponed_queue_.empty()) {
        const auto& action = postponed_queue_.front();
        action.action();
        postponed_queue_.pop();
      }
      break;

    default:
      // Other regions not implemented yet
      break;
  }
}

void SimulationRunner::InitializeVariables() {
  for (const auto& variable : module_.get().variables) {
    context_.get().variable_table.InitializeVariable(
        std::string(variable.symbol.get().name), variable.type);
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

  context_.get().tracer.Record(fmt::format(
      "{} | Start at block {} instruction {}", process->name,
      process->blocks[block_index]->label, instruction_index));

  auto result =
      process_runner_.RunProcess(process, block_index, instruction_index);

  context_.get().tracer.Record(
      fmt::format("{} | {}", process->name, result.Summary()));

  WakeWaitingProcesses(result.modified_variables);

  for (const auto& action : result.nba_actions) {
    nba_queue_.push(action);
  }
  for (const auto& action : result.postponed_actions) {
    postponed_queue_.push(action);
  }

  switch (result.kind) {
    case ProcessResult::Kind::kDelay: {
      SimulationTime delay_time =
          context_.get().current_time + result.delay_amount;
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
