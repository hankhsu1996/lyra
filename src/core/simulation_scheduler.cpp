#include "core/simulation_scheduler.hpp"

namespace lyra {

SimulationScheduler::SimulationScheduler(
    const lir::Module& module, ExecutionContext& ctx,
    VariableTriggerMap variable_triggers)
    : module_(module),
      ctx_(ctx),
      executor_(module, ctx),
      variable_to_triggers_(std::move(variable_triggers)) {
}

void SimulationScheduler::Run() {
  ScheduleInitialProcesses();
  ScheduleAlwaysProcesses();

  while (!active_queue_.empty()) {
    ExecuteOneEvent();
  }
}

void SimulationScheduler::ScheduleInitialProcesses() {
  for (const auto& process : module_.get().processes) {
    if (process->kind == lir::ProcessKind::kInitial) {
      active_queue_.push({process});
    }
  }
}

void SimulationScheduler::ScheduleAlwaysProcesses() {
  for (const auto& process : module_.get().processes) {
    if (process->kind == lir::ProcessKind::kAlways) {
      active_queue_.push({process});
    }
  }
}

void SimulationScheduler::ExecuteOneEvent() {
  ScheduledEvent event = active_queue_.front();
  active_queue_.pop();

  for (const auto& instr : event.process->instructions) {
    executor_.ExecuteInstruction(instr);

    if (instr.kind == lir::InstructionKind::kStoreSignal) {
      const auto& dst_variable = std::get<std::string>(instr.operands[0].data);

      RuntimeValue old_value =
          ctx_.get().signalTable.ReadPrevious(dst_variable);
      RuntimeValue new_value = ctx_.get().signalTable.Read(dst_variable);

      int old_int = old_value.AsInt();
      int new_int = new_value.AsInt();

      auto it = variable_to_triggers_.find(dst_variable);
      if (it != variable_to_triggers_.end()) {
        for (const auto& [trigger, proc] : it->second) {
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
            active_queue_.push({proc});
          }
        }
      }

      ctx_.get().signalTable.UpdatePrevious(dst_variable, new_value);
    }
  }
}

}  // namespace lyra
