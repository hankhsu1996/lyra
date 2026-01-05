#include "lyra/interpreter/trigger_manager.hpp"

#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

#include "lyra/common/trigger.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/interpreter/simulation_runner.hpp"
#include "lyra/interpreter/variable_table.hpp"
#include "lyra/lir/process.hpp"

namespace lyra::interpreter {

void TriggerManager::RegisterWaitingProcess(
    const std::shared_ptr<lir::Process>& process, const SymbolRef& variable,
    common::EdgeKind edge_kind, std::size_t block_index,
    std::size_t instruction_index) {
  wait_map_[variable].insert(process);
  wait_set_[process] = {
      .block_index = block_index,
      .instruction_index = instruction_index,
      .edge_kind = edge_kind};
}

auto TriggerManager::CheckTriggers(
    const std::vector<SymbolRef>& modified_variables)
    -> std::vector<ScheduledEvent> {
  std::vector<ScheduledEvent> events_to_trigger;
  std::unordered_set<std::shared_ptr<lir::Process>> triggered_processes;

  for (const auto& variable : modified_variables) {
    // Read previous and current values of the variable
    RuntimeValue old_value =
        context_.get().variable_table.ReadPrevious(variable);
    RuntimeValue new_value = context_.get().variable_table.Read(variable);

    // Update previous value for this variable - IMPORTANT!
    context_.get().variable_table.UpdatePrevious(variable, new_value);

    // Check if any process is waiting on this variable
    auto map_it = wait_map_.find(variable);
    if (map_it == wait_map_.end()) {
      continue;
    }

    std::unordered_set<std::shared_ptr<lir::Process>> to_remove;

    for (const auto& process : map_it->second) {
      // Skip if this process is already triggered
      if (triggered_processes.contains(process)) {
        continue;
      }

      auto info_it = wait_set_.find(process);
      if (info_it == wait_set_.end()) {
        continue;
      }

      const auto& wait_info = info_it->second;

      if (ShouldTrigger(old_value, new_value, wait_info.edge_kind)) {
        events_to_trigger.push_back(
            ScheduledEvent{
                .process = process,
                .block_index = wait_info.block_index,
                .instruction_index = wait_info.instruction_index});
        triggered_processes.insert(process);
        to_remove.insert(process);
      }

      if (!to_remove.empty()) {
        std::vector<std::string> proc_names;
        for (const auto& proc : to_remove) {
          proc_names.push_back(proc->name);
        }

        std::string trace_detail = fmt::format(
            "Trigger on variable '{}': old = {}, new = {}, triggered "
            "process(es) = {}",
            variable->name, old_value.ToString(), new_value.ToString(),
            fmt::join(proc_names, ", "));

        context_.get().tracer.Record(trace_detail);
      }
    }

    // Remove triggered processes from wait map for this variable
    for (const auto& proc : to_remove) {
      map_it->second.erase(proc);
    }

    if (map_it->second.empty()) {
      vars_to_remove_.push_back(variable);
    }
  }

  // Clean up empty maps
  for (const auto& var : vars_to_remove_) {
    wait_map_.erase(var);
  }
  vars_to_remove_.clear();

  // Remove all triggered processes from wait_set
  for (const auto& proc : triggered_processes) {
    wait_set_.erase(proc);
  }

  return events_to_trigger;
}

auto TriggerManager::ShouldTrigger(
    const RuntimeValue& old_value, const RuntimeValue& new_value,
    common::EdgeKind edge_kind) -> bool {
  if (edge_kind == common::EdgeKind::kAnyChange) {
    return old_value != new_value;
  }

  if (old_value.IsTwoState() && new_value.IsTwoState()) {
    uint64_t old_bit0 = old_value.AsUInt64() & 1;
    uint64_t new_bit0 = new_value.AsUInt64() & 1;

    switch (edge_kind) {
      case common::EdgeKind::kPosedge:
        return (old_bit0 == 0 && new_bit0 == 1);
      case common::EdgeKind::kNegedge:
        return (old_bit0 == 1 && new_bit0 == 0);
      case common::EdgeKind::kBothEdge:
        return (old_bit0 != new_bit0);
      default:
        return false;
    }
  }

  return false;
}

}  // namespace lyra::interpreter
